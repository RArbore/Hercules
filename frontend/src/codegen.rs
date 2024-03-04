extern crate hercules_ir;

use std::collections::{HashMap, VecDeque};

use self::hercules_ir::ir;
use self::hercules_ir::ir::*;
use self::hercules_ir::build::*;

use crate::ssa::SSA;
use crate::semant;
use crate::semant::{Prg, Function, Stmt, Expr, Literal, UnaryOp, BinaryOp};
use crate::types::{TypeSolver, TypeSolverInst, Primitive, Either};

// Loop info is a stack of the loop levels, recording the latch and exit block of each
type LoopInfo = Vec<(NodeID, NodeID)>;

pub fn codegen_program(prg : Prg) -> Module {
    CodeGenerator::build(prg)
}

struct CodeGenerator<'a> {
    builder   : Builder<'a>,
    types     : &'a TypeSolver,
    funcs     : &'a Vec<Function>,
    uid       : usize,
    // The function map tracks a map from function index and set of type variables to its function
    // id in the builder
    functions : HashMap<(usize, Vec<TypeID>), FunctionID>,
    // The worklist tracks a list of functions to codegen, tracking the function's id, its
    // type-solving instantiation (account for the type parameters), the function id, and the entry
    // block id
    worklist  : VecDeque<(usize, TypeSolverInst<'a>, FunctionID, NodeID)>,
}

impl CodeGenerator<'_> {
    fn build((types, funcs) : Prg) -> Module {
        // Identify the functions (by index) which have no type arguments, these are the ones we
        // ask for code to be generated for
        let func_idx
            = funcs.iter().enumerate()
                  .filter_map(|(i, f)|
                                if f.num_type_args == 0 { Some(i) } else { None });

        let mut codegen = CodeGenerator { builder   : Builder::create(),
                                          types     : &types,
                                          funcs     : &funcs,
                                          uid       : 0,
                                          functions : HashMap::new(),
                                          worklist  : VecDeque::new(), };

        // Add the identifed functions to the list to code-gen
        func_idx.for_each(|i| { let _ = codegen.get_function(i, vec![]); });

        codegen.finish()
    }

    fn finish(mut self) -> Module {
        while !self.worklist.is_empty() {
            let (idx, mut type_inst, func, entry) = self.worklist.pop_front().unwrap();
            self.codegen_function(&self.funcs[idx], &mut type_inst, func, entry);
        }

        self.builder.finish()
    }

    fn get_function(&mut self, func_idx : usize, ty_args : Vec<TypeID>) -> FunctionID {
        let func_info = (func_idx, ty_args);
        match self.functions.get(&func_info) {
            Some(func_id) => *func_id,
            None => {
                let ty_args = func_info.1;

                let func = &self.funcs[func_idx];
                let mut solver_inst = self.types.create_instance(ty_args.clone());

                // TODO: Ideally we would write out the type arguments, but now that they're
                // lowered to TypeID we can't do that as far as I can tell
                let name = format!("{}_{}", func.name, self.uid);
                self.uid += 1;

                let mut param_types = vec![];
                for (_, ty) in func.arguments.iter() {
                    param_types.push(solver_inst.lower_type(&mut self.builder, *ty));
                }

                let return_type = solver_inst.lower_type(&mut self.builder, func.return_type);

                let (func_id, entry)
                    = self.builder.create_function(
                        &name, param_types, return_type,
                        func.num_dyn_consts as u32).unwrap();

                self.functions.insert((func_idx, ty_args), func_id);
                self.worklist.push_back((func_idx, solver_inst, func_id, entry));
                func_id
            },
        }
    }

    fn codegen_function(&mut self, func : &Function, types : &mut TypeSolverInst,
                        func_id : FunctionID, entry : NodeID) {
        // Setup the SSA construction data structure
        let mut ssa = SSA::new(func_id, entry);

        // Create nodes for the arguments
        for (idx, (var, _)) in func.arguments.iter().enumerate() {
            let mut node_builder = self.builder.allocate_node(func_id);
            ssa.write_variable(*var, entry, node_builder.id());
            node_builder.build_parameter(idx);
            let _ = self.builder.add_node(node_builder);
        }

        // Generate code for the body
        let None = self.codegen_stmt(&func.body, types, &mut ssa,
                                     func_id, entry, &mut vec![])
            else { panic!("Generated code for a function missing a return") };
    }

    fn codegen_stmt(&mut self, stmt : &Stmt, types : &mut TypeSolverInst,
                    ssa : &mut SSA, func_id : FunctionID, cur_block : NodeID,
                    loops : &mut LoopInfo) -> Option<NodeID> {
        match stmt {
            Stmt::AssignStmt { var, val } => {
                let (val, block) = self.codegen_expr(val, types, ssa, func_id, cur_block);
                ssa.write_variable(*var, block, val);
                Some(block)
            },
            Stmt::IfStmt { cond, thn, els } => {
                let (val_cond, block_cond)
                    = self.codegen_expr(cond, types, ssa, func_id, cur_block);
                let (mut if_node, block_then, block_else)
                    = ssa.create_cond(&mut self.builder, block_cond);

                let then_end = self.codegen_stmt(thn, types, ssa,
                                                 func_id, block_then, loops);
                let else_end =
                    match els {
                        None => Some(block_else),
                        Some(els_stmt) =>
                            self.codegen_stmt(els_stmt, types, ssa,
                                              func_id, block_else, loops),
                    };

                if_node.build_if(block_cond, val_cond);
                let _ = self.builder.add_node(if_node);

                match (then_end, else_end) {
                    (None, els) => els,
                    (thn, None) => thn,
                    (Some(then_term), Some(else_term)) => {
                        let block_join = ssa.create_block(&mut self.builder);
                        ssa.add_pred(block_join, then_term);
                        ssa.add_pred(block_join, else_term);
                        ssa.seal_block(block_join, &mut self.builder);
                        Some(block_join)
                    },
                }
            },
            Stmt::LoopStmt { cond, update, body } => {
                // We generate guarded loops, so the first step is to create
                // a conditional branch, branching on the condition
                let (val_guard, block_guard)
                    = self.codegen_expr(cond, types, ssa, func_id, cur_block);
                let (mut if_node, true_guard, false_proj)
                    = ssa.create_cond(&mut self.builder, block_guard);
                if_node.build_if(block_guard, val_guard);
                let _ = self.builder.add_node(if_node);

                // We then create a region for the exit (since there may be breaks)
                let block_exit = ssa.create_block(&mut self.builder);
                ssa.add_pred(block_exit, false_proj);

                // Now, create a block for the loop's latch, we don't (currently) know any of its
                // predecessors
                let block_latch = ssa.create_block(&mut self.builder);

                // Code-gen any update into the latch and then code-gen the condition
                let block_updated =
                    match update {
                        None => block_latch,
                        Some(stmt) =>
                            self.codegen_stmt(stmt, types, ssa, func_id, block_latch, loops)
                                .expect("Loop update should return control"),
                    };
                let (val_cond, block_cond)
                    = self.codegen_expr(cond, types, ssa, func_id, block_updated);

                let (mut if_node, true_proj, false_proj)
                    = ssa.create_cond(&mut self.builder, block_cond);
                if_node.build_if(block_cond, val_cond);
                let _ = self.builder.add_node(if_node);

                // Add the false projection from the latch as a predecessor of the exit
                ssa.add_pred(block_exit, false_proj);

                // Create a block for the loop header, and add the true branches from the guard and
                // latch as its only predecessors
                let body_block = ssa.create_block(&mut self.builder);
                ssa.add_pred(body_block, true_guard);
                ssa.add_pred(body_block, true_proj);
                ssa.seal_block(body_block, &mut self.builder);

                // Generate code for the body
                loops.push((block_latch, block_exit));
                let body_res = self.codegen_stmt(body, types, ssa, func_id, body_block, loops);
                loops.pop();

                // If the body of the loop can reach some block, we add that block as a predecessor
                // of the latch
                match body_res {
                    None => {},
                    Some(block) => {
                        ssa.add_pred(block_latch, block);
                    },
                }

                // Seal remaining open blocks
                ssa.seal_block(block_exit, &mut self.builder);
                ssa.seal_block(block_latch, &mut self.builder);

                // It is always assumed a loop may be skipped and so control can reach after the
                // loop
                Some(block_exit)
            },
            Stmt::ReturnStmt { expr } => {
                let (val_ret, block_ret)
                    = self.codegen_expr(expr, types, ssa, func_id, cur_block);
                let mut return_node = self.builder.allocate_node(func_id);
                return_node.build_return(block_ret, val_ret);
                let _ = self.builder.add_node(return_node);
                None
            },
            Stmt::BreakStmt {} => {
                let last_loop = loops.len() - 1;
                let (_latch, exit) = loops[last_loop];
                ssa.add_pred(exit, cur_block); // The block that contains this break now leads to
                                               // the exit
                None
            },
            Stmt::ContinueStmt {} => {
                let last_loop = loops.len() - 1;
                let (latch, _exit) = loops[last_loop];
                ssa.add_pred(latch, cur_block); // The block that contains this continue now leads
                                                // to the latch
                None
            },
            Stmt::BlockStmt { body } => {
                let mut block = Some(cur_block);
                for stmt in body.iter() {
                    block = self.codegen_stmt(stmt, types, ssa, func_id,
                                              block.unwrap(), loops);
                }
                block
            },
            Stmt::ExprStmt { expr } => {
                let (_val, block)
                    = self.codegen_expr(expr, types, ssa, func_id, cur_block);
                Some(block)
            },
        }
    }
    
    // The codegen_expr function returns a pair of node IDs, the first is the node whose value is
    // the given expression and the second is the node of a control node at which the value is
    // available
    fn codegen_expr(&mut self, expr : &Expr, types : &mut TypeSolverInst,
                    ssa : &mut SSA, func_id : FunctionID, cur_block : NodeID) 
        -> (NodeID, NodeID) {
        match expr {
            Expr::Variable { var, .. } => {
                (ssa.read_variable(*var, cur_block, &mut self.builder),
                 cur_block)
            },
            Expr::DynConst { idx, .. } => {
                let mut node = self.builder.allocate_node(func_id);
                let node_id = node.id();
                let dyn_const = self.builder.create_dynamic_constant_parameter(*idx);
                node.build_dynamicconstant(dyn_const);
                let _ = self.builder.add_node(node);
                (node_id, cur_block)
            },
            Expr::Read { index, val, .. } => {
                let (collection, block)
                    = self.codegen_expr(val, types, ssa, func_id, cur_block);
                let (indices, end_block) 
                    = self.codegen_indices(index, types, ssa, func_id, block);

                let mut node = self.builder.allocate_node(func_id);
                let node_id = node.id();
                node.build_read(collection, indices.into());
                let _ = self.builder.add_node(node);
                (node_id, end_block)
            },
            Expr::Write { index, val, rep, .. } => {
                let (collection, block)
                    = self.codegen_expr(val, types, ssa, func_id, cur_block);
                let (indices, idx_block) 
                    = self.codegen_indices(index, types, ssa, func_id, block);
                let (replace, end_block)
                    = self.codegen_expr(rep, types, ssa, func_id, idx_block);

                let mut node = self.builder.allocate_node(func_id);
                let node_id = node.id();
                node.build_write(collection, replace, indices.into());
                let _ = self.builder.add_node(node);
                (node_id, end_block)
            },
            Expr::Tuple { vals, typ } => {
                let mut block = cur_block;
                let mut values = vec![];
                for expr in vals {
                    let (val_expr, block_expr)
                        = self.codegen_expr(expr, types, ssa, func_id, block);
                    block = block_expr;
                    values.push(val_expr);
                }
                
                let tuple_type = types.lower_type(&mut self.builder, *typ);
                (self.build_tuple(values, tuple_type, func_id), block)
            },
            Expr::Union { tag, val, typ } => {
                let (value, block)
                    = self.codegen_expr(val, types, ssa, func_id, cur_block);

                let union_type = types.lower_type(&mut self.builder, *typ);
                (self.build_union(*tag, value, union_type, func_id), block)
            },
            Expr::Constant { val, .. } => {
                let const_id = self.build_constant(val, types);
                
                let mut val = self.builder.allocate_node(func_id);
                let val_node = val.id();
                val.build_constant(const_id);
                let _ = self.builder.add_node(val);

                (val_node, cur_block)
            },
            Expr::Zero { typ } => {
                let type_id = types.lower_type(&mut self.builder, *typ);
                let zero_const = self.builder.create_constant_zero(type_id);
                let mut zero = self.builder.allocate_node(func_id);
                let zero_val = zero.id();
                zero.build_constant(zero_const);
                let _ = self.builder.add_node(zero);

                (zero_val, cur_block)
            },
            Expr::UnaryExp { op, expr, .. } => {
                let (val, block)
                    = self.codegen_expr(expr, types, ssa, func_id, cur_block);
                
                let mut expr = self.builder.allocate_node(func_id);
                let expr_id = expr.id();
                expr.build_unary(val,
                                 match op {
                                     UnaryOp::Negation => UnaryOperator::Neg,
                                     UnaryOp::BitwiseNot => UnaryOperator::Not,
                                 });
                let _ = self.builder.add_node(expr);

                (expr_id, block)
            },
            Expr::BinaryExp { op, lhs, rhs, .. } => {
                let (val_lhs, block_lhs)
                    = self.codegen_expr(lhs, types, ssa, func_id, cur_block);
                let (val_rhs, block_rhs)
                    = self.codegen_expr(rhs, types, ssa, func_id, block_lhs);

                let mut expr = self.builder.allocate_node(func_id);
                let expr_id = expr.id();
                expr.build_binary(val_lhs, val_rhs,
                                  match op {
                                      BinaryOp::Add    => BinaryOperator::Add,
                                      BinaryOp::Sub    => BinaryOperator::Sub,
                                      BinaryOp::Mul    => BinaryOperator::Mul,
                                      BinaryOp::Div    => BinaryOperator::Div,
                                      BinaryOp::Mod    => BinaryOperator::Rem,
                                      BinaryOp::BitAnd => BinaryOperator::And,
                                      BinaryOp::BitOr  => BinaryOperator::Or,
                                      BinaryOp::Xor    => BinaryOperator::Xor,
                                      BinaryOp::Lt     => BinaryOperator::LT,
                                      BinaryOp::Le     => BinaryOperator::LTE,
                                      BinaryOp::Gt     => BinaryOperator::GT,
                                      BinaryOp::Ge     => BinaryOperator::GTE,
                                      BinaryOp::Eq     => BinaryOperator::EQ,
                                      BinaryOp::Neq    => BinaryOperator::NE,
                                      BinaryOp::LShift => BinaryOperator::LSh,
                                      BinaryOp::RShift => BinaryOperator::RSh,
                                  });
                let _ = self.builder.add_node(expr);

                (expr_id, block_rhs)
            },
            Expr::CastExpr { expr, typ } => {
                let type_id = types.lower_type(&mut self.builder, *typ);
                let (val, block)
                    = self.codegen_expr(expr, types, ssa, func_id, cur_block);

                let mut expr = self.builder.allocate_node(func_id);
                let expr_id = expr.id();
                expr.build_unary(val, UnaryOperator::Cast(type_id));
                let _ = self.builder.add_node(expr);

                (expr_id, block)
            },
            Expr::CondExpr { cond, thn, els, .. } => {
                // Code-gen the condition
                let (val_cond, block_cond)
                    = self.codegen_expr(cond, types, ssa, func_id, cur_block);

                // Create the if
                let (mut if_builder, then_block, else_block)
                    = ssa.create_cond(&mut self.builder, block_cond);
                if_builder.build_if(block_cond, val_cond);
                let _ = self.builder.add_node(if_builder);

                // Code-gen the branches
                let (then_val, block_then)
                    = self.codegen_expr(thn, types, ssa, func_id, then_block);
                let (else_val, block_else)
                    = self.codegen_expr(els, types, ssa, func_id, else_block);

                // Create the join in the control-flow
                let join = ssa.create_block(&mut self.builder);
                ssa.add_pred(join, block_then);
                ssa.add_pred(join, block_else);
                ssa.seal_block(join, &mut self.builder);

                // Create a phi that joins the two branches
                let mut phi = self.builder.allocate_node(func_id);
                let phi_id = phi.id();
                phi.build_phi(join, vec![then_val, else_val].into());
                let _ = self.builder.add_node(phi);

                (phi_id, join)
            },
            Expr::CallExpr { func, ty_args, dyn_consts, args, .. } => {
                // We start by lowering the type arguments to TypeIDs
                let mut type_params = vec![];
                for typ in ty_args {
                    type_params.push(types.lower_type(&mut self.builder, *typ));
                }

                // With the type arguments, we can now lookup the function
                let func_id = self.get_function(*func, type_params);

                // We then build the dynamic constants
                let dynamic_constants
                    = TypeSolverInst::build_dyn_consts(&mut self.builder, dyn_consts);

                // Code gen for each argument in order
                // For inouts, this becomes an ssa.read_variable
                // We also record the variables which are our inouts
                let mut block = cur_block;
                let mut arg_vals = vec![];
                let mut inouts = vec![];
                for arg in args {
                    match arg {
                        Either::Left(exp) => {
                            let (val, new_block)
                                = self.codegen_expr(exp, types, ssa, func_id, block);
                            block = new_block;
                            arg_vals.push(val);
                        },
                        Either::Right(var) => {
                            inouts.push(*var);
                            arg_vals.push(ssa.read_variable(*var, block, &mut self.builder));
                        },
                    }
                }

                // Create the call expression
                let mut call = self.builder.allocate_node(func_id);
                let call_id = call.id();
                call.build_call(func_id, dynamic_constants.into(), arg_vals.into());
                let _ = self.builder.add_node(call);

                // Read each of the "inout values" and perform the SSA update
                let inouts_index = self.builder.create_field_index(1);
                for (idx, var) in inouts.into_iter().enumerate() {
                    let index = self.builder.create_field_index(idx);
                    let mut read = self.builder.allocate_node(func_id);
                    let read_id = read.id();
                    read.build_read(call_id, vec![inouts_index.clone(), index].into());
                    let _ = self.builder.add_node(read);

                    ssa.write_variable(var, block, read_id);
                }
                
                // Read the "actual return" value and return it
                let value_index = self.builder.create_field_index(0);
                let mut read = self.builder.allocate_node(func_id);
                let read_id = read.id();
                read.build_read(call_id, vec![value_index].into());
                let _ = self.builder.add_node(read);

                (read_id, block)
            },
        }
    }

    // Convert a list of Index from the semantic analysis into a list of indices for the builder.
    // Note that this takes and returns a block since expressions may involve control flow
    fn codegen_indices(&mut self, index : &Vec<semant::Index>, types : &mut TypeSolverInst,
                       ssa : &mut SSA, func_id : FunctionID, cur_block : NodeID)
        -> (Vec<ir::Index>, NodeID) {

        let mut block = cur_block;
        let mut built_index = vec![];
        for idx in index {
            match idx {
                semant::Index::Field(idx) => {
                    built_index.push(self.builder.create_field_index(*idx));
                },
                semant::Index::Variant(idx) => {
                    built_index.push(self.builder.create_variant_index(*idx));
                },
                semant::Index::Array(exps) => {
                    let mut expr_vals = vec![];
                    for exp in exps {
                        let (val, new_block) =
                            self.codegen_expr(exp, types, ssa, func_id, block);
                        block = new_block;
                        expr_vals.push(val);
                    }
                    built_index.push(self.builder.create_position_index(expr_vals.into()));
                },
            }
        }

        (built_index, block)
    }

    fn build_tuple(&mut self, exprs : Vec<NodeID>, typ : TypeID, func_id : FunctionID) -> NodeID {
        let zero_const = self.builder.create_constant_zero(typ);
        
        let mut zero = self.builder.allocate_node(func_id);
        let zero_val = zero.id();
        zero.build_constant(zero_const);
        let _ = self.builder.add_node(zero);

        let mut val = zero_val;
        for (idx, exp) in exprs.into_iter().enumerate() {
            let mut write = self.builder.allocate_node(func_id);
            let write_id = write.id();
            let index = self.builder.create_field_index(idx);

            write.build_write(val, exp, vec![index].into());
            let _ = self.builder.add_node(write);
            val = write_id;
        }

        val
    }
    
    fn build_union(&mut self, tag : usize, val : NodeID, typ : TypeID,
                   func_id : FunctionID) -> NodeID {
        let zero_const = self.builder.create_constant_zero(typ);
        
        let mut zero = self.builder.allocate_node(func_id);
        let zero_val = zero.id();
        zero.build_constant(zero_const);
        let _ = self.builder.add_node(zero);

        let mut write = self.builder.allocate_node(func_id);
        let write_id = write.id();
        let index = self.builder.create_variant_index(tag);

        write.build_write(zero_val, val, vec![index].into());
        let _ = self.builder.add_node(write);

        write_id
    }

    fn build_constant<'a>(&mut self, (lit, typ) : &semant::Constant,
                          types : &mut TypeSolverInst<'a>) -> ConstantID {
        match lit {
            Literal::Unit => {
                self.builder.create_constant_prod(vec![].into())
            },
            Literal::Bool(val) => {
                self.builder.create_constant_bool(*val)
            },
            Literal::Integer(val) => {
                let p = types.as_numeric_type(&mut self.builder, *typ);
                match p {
                    Primitive::I8  => self.builder.create_constant_i8(*val as i8),
                    Primitive::I16 => self.builder.create_constant_i16(*val as i16),
                    Primitive::I32 => self.builder.create_constant_i32(*val as i32),
                    Primitive::I64 => self.builder.create_constant_i64(*val as i64),
                    Primitive::U8  => self.builder.create_constant_u8(*val as u8),
                    Primitive::U16 => self.builder.create_constant_u16(*val as u16),
                    Primitive::U32 => self.builder.create_constant_u32(*val as u32),
                    Primitive::U64 => self.builder.create_constant_u64(*val as u64),
                    Primitive::F32 => self.builder.create_constant_f32(*val as f32),
                    Primitive::F64 => self.builder.create_constant_f64(*val as f64),
                    _ => panic!("Internal error in build_constant for integer"),
                }
            },
            Literal::Float(val) => {
                let p = types.as_numeric_type(&mut self.builder, *typ);
                match p {
                    Primitive::F32 => self.builder.create_constant_f32(*val as f32),
                    Primitive::F64 => self.builder.create_constant_f64(*val as f64),
                    _ => panic!("Internal error in build_constant for float"),
                }
            },
            Literal::Tuple(vals) => {
                let mut constants = vec![];
                for val in vals {
                    constants.push(self.build_constant(val, types));
                }
                self.builder.create_constant_prod(constants.into())
            },
            Literal::Sum(tag, val) => {
                let constant = self.build_constant(val, types);
                let type_id = types.lower_type(&mut self.builder, *typ);
                self.builder.create_constant_sum(type_id, *tag as u32, constant).unwrap()
            },
        }
    }
}
