extern crate bitvec;
extern crate ordered_float;

use std::fmt::Write;
use std::ops::Coroutine;
use std::ops::CoroutineState;
use std::pin::Pin;

use self::bitvec::prelude::*;

use crate::*;

/*
 * A module is a list of functions. Functions contain types, constants, and
 * dynamic constants, which are interned at the module level. Thus, if one
 * wants to run an intraprocedural pass in parallel, it is advised to first
 * destruct the module, then reconstruct it once finished.
 */
#[derive(Debug, Default, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
    pub dynamic_constants: Vec<DynamicConstant>,
}

impl Module {
    /*
     * There are many transformations that need to iterate over the functions
     * in a module, while having mutable access to the interned types,
     * constants, and dynamic constants in a module. This code is really ugly,
     * so write it once.
     */
    pub fn map<F>(self, mut func: F) -> Self
    where
        F: FnMut(
            (Function, FunctionID),
            (Vec<Type>, Vec<Constant>, Vec<DynamicConstant>),
        ) -> (Function, (Vec<Type>, Vec<Constant>, Vec<DynamicConstant>)),
    {
        let Module {
            functions,
            types,
            constants,
            dynamic_constants,
        } = self;
        let mut stuff = (types, constants, dynamic_constants);
        let functions = functions
            .into_iter()
            .enumerate()
            .map(|(idx, function)| {
                let mut new_stuff = (vec![], vec![], vec![]);
                std::mem::swap(&mut stuff, &mut new_stuff);
                let (function, mut new_stuff) = func((function, FunctionID::new(idx)), new_stuff);
                std::mem::swap(&mut stuff, &mut new_stuff);
                function
            })
            .collect();
        let (types, constants, dynamic_constants) = stuff;
        Module {
            functions,
            types,
            constants,
            dynamic_constants,
        }
    }

    /*
     * Printing out types, constants, and dynamic constants fully requires a
     * reference to the module, since references to other types, constants, and
     * dynamic constants are done using IDs.
     */
    pub fn write_type<W: Write>(&self, ty_id: TypeID, w: &mut W) -> std::fmt::Result {
        match &self.types[ty_id.idx()] {
            Type::Control(_) => write!(w, "Control"),
            Type::Boolean => write!(w, "Boolean"),
            Type::Integer8 => write!(w, "Integer8"),
            Type::Integer16 => write!(w, "Integer16"),
            Type::Integer32 => write!(w, "Integer32"),
            Type::Integer64 => write!(w, "Integer64"),
            Type::UnsignedInteger8 => write!(w, "UnsignedInteger8"),
            Type::UnsignedInteger16 => write!(w, "UnsignedInteger16"),
            Type::UnsignedInteger32 => write!(w, "UnsignedInteger32"),
            Type::UnsignedInteger64 => write!(w, "UnsignedInteger64"),
            Type::Float32 => write!(w, "Float32"),
            Type::Float64 => write!(w, "Float64"),
            Type::Product(fields) => {
                write!(w, "Product(")?;
                for idx in 0..fields.len() {
                    let field_ty_id = fields[idx];
                    self.write_type(field_ty_id, w)?;
                    if idx + 1 < fields.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")
            }
            Type::Summation(fields) => {
                write!(w, "Summation(")?;
                for idx in 0..fields.len() {
                    let field_ty_id = fields[idx];
                    self.write_type(field_ty_id, w)?;
                    if idx + 1 < fields.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")
            }
            Type::Array(elem, length) => {
                write!(w, "Array(")?;
                self.write_type(*elem, w)?;
                write!(w, ", ")?;
                self.write_dynamic_constant(*length, w)?;
                write!(w, ")")
            }
        }?;

        Ok(())
    }

    pub fn write_constant<W: Write>(&self, cons_id: ConstantID, w: &mut W) -> std::fmt::Result {
        match &self.constants[cons_id.idx()] {
            Constant::Boolean(val) => write!(w, "{}", val),
            Constant::Integer8(val) => write!(w, "{}", val),
            Constant::Integer16(val) => write!(w, "{}", val),
            Constant::Integer32(val) => write!(w, "{}", val),
            Constant::Integer64(val) => write!(w, "{}", val),
            Constant::UnsignedInteger8(val) => write!(w, "{}", val),
            Constant::UnsignedInteger16(val) => write!(w, "{}", val),
            Constant::UnsignedInteger32(val) => write!(w, "{}", val),
            Constant::UnsignedInteger64(val) => write!(w, "{}", val),
            Constant::Float32(val) => write!(w, "{}", val),
            Constant::Float64(val) => write!(w, "{}", val),
            Constant::Product(_, fields) => {
                write!(w, "(")?;
                for idx in 0..fields.len() {
                    let field_cons_id = fields[idx];
                    self.write_constant(field_cons_id, w)?;
                    if idx + 1 < fields.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")
            }
            Constant::Summation(_, variant, field) => {
                write!(w, "%{}(", variant)?;
                self.write_constant(*field, w)?;
                write!(w, ")")
            }
            Constant::Array(_, elems) => {
                write!(w, "[")?;
                for idx in 0..elems.len() {
                    let elem_cons_id = elems[idx];
                    self.write_constant(elem_cons_id, w)?;
                    if idx + 1 < elems.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, "]")
            }
        }?;

        Ok(())
    }

    pub fn write_dynamic_constant<W: Write>(
        &self,
        dc_id: DynamicConstantID,
        w: &mut W,
    ) -> std::fmt::Result {
        match &self.dynamic_constants[dc_id.idx()] {
            DynamicConstant::Constant(cons) => write!(w, "{}", cons),
            DynamicConstant::Parameter(param) => write!(w, "#{}", param),
        }?;

        Ok(())
    }

    /*
     * Create an iterator that traverses all the types in the module bottom up.
     * This uses a coroutine to make iteratively traversing the type DAGs
     * easier.
     */
    pub fn types_bottom_up(&self) -> impl Iterator<Item = TypeID> + '_ {
        let types = &self.types;
        let mut visited = bitvec![u8, Lsb0; 0; self.types.len()];
        let mut stack = (0..self.types.len())
            .map(TypeID::new)
            .collect::<Vec<TypeID>>();
        let coroutine = move || {
            while let Some(id) = stack.pop() {
                if visited[id.idx()] {
                    continue;
                }
                match &types[id.idx()] {
                    Type::Product(children) | Type::Summation(children) => {
                        let can_yield = children.iter().all(|x| visited[x.idx()]);
                        if can_yield {
                            visited.set(id.idx(), true);
                            yield id;
                        } else {
                            stack.push(id);
                            for id in children.iter() {
                                stack.push(*id);
                            }
                        }
                    }
                    Type::Array(child, _) => {
                        let can_yield = visited[child.idx()];
                        if can_yield {
                            visited.set(id.idx(), true);
                            yield id;
                        } else {
                            stack.push(id);
                            stack.push(*child);
                        }
                    }
                    _ => {
                        visited.set(id.idx(), true);
                        yield id;
                    }
                }
            }
        };
        TypesIterator {
            coroutine: Box::new(coroutine),
        }
    }
}

pub struct TypesIterator<G>
where
    G: Coroutine<Yield = TypeID, Return = ()> + Unpin,
{
    coroutine: G,
}

impl<G> Iterator for TypesIterator<G>
where
    G: Coroutine<Yield = TypeID, Return = ()> + Unpin,
{
    type Item = TypeID;

    fn next(&mut self) -> Option<Self::Item> {
        // Iterator corresponds to yields from coroutine.
        match Pin::new(&mut self.coroutine).resume(()) {
            CoroutineState::Yielded(ty) => Some(ty),
            CoroutineState::Complete(_) => None,
        }
    }
}

/*
 * A function has a name, a list of types for its parameters, a single return
 * type, a list of nodes in its sea-of-nodes style IR, and a number of dynamic
 * constants. When calling a function, arguments matching the parameter types
 * are required, as well as the correct number of dynamic constants. All
 * dynamic constants are 64-bit unsigned integers (usize / u64), so it is
 * sufficient to merely store how many of them the function takes as arguments.
 */
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub param_types: Vec<TypeID>,
    pub return_type: TypeID,
    pub nodes: Vec<Node>,
    pub num_dynamic_constants: u32,
}

impl Function {
    /*
     * Many transformations will delete nodes. There isn't strictly a gravestone
     * node value, so use the start node as a gravestone value (for IDs other
     * than 0). This function cleans up gravestoned nodes.
     */
    pub fn delete_gravestones(&mut self) {
        // Step 1: figure out which nodes are gravestones.
        let mut gravestones = (0..self.nodes.len())
            .filter(|x| *x != 0 && self.nodes[*x].is_start())
            .map(|x| NodeID::new(x));

        // Step 2: figure out the mapping between old node IDs and new node IDs.
        let mut node_mapping = Vec::with_capacity(self.nodes.len());
        let mut next_gravestone = gravestones.next();
        let mut num_gravestones_passed = 0;
        for idx in 0..self.nodes.len() {
            if Some(NodeID::new(idx)) == next_gravestone {
                node_mapping.push(NodeID::new(0));
                num_gravestones_passed += 1;
                next_gravestone = gravestones.next();
            } else {
                node_mapping.push(NodeID::new(idx - num_gravestones_passed));
            }
        }

        // Step 3: create new nodes vector. Along the way, update all uses.
        let mut old_nodes = vec![];
        std::mem::swap(&mut old_nodes, &mut self.nodes);

        let mut new_nodes = Vec::with_capacity(old_nodes.len() - num_gravestones_passed);
        for (idx, mut node) in old_nodes.into_iter().enumerate() {
            // Skip node if it's dead.
            if idx != 0 && node.is_start() {
                continue;
            }

            // Update uses.
            for u in get_uses_mut(&mut node).as_mut() {
                let old_id = **u;
                let new_id = node_mapping[old_id.idx()];
                if new_id == NodeID::new(0) && old_id != NodeID::new(0) {
                    panic!("While deleting gravestones, came across a use of a gravestoned node.");
                }
                **u = new_id;
            }

            // Add to new_nodes.
            new_nodes.push(node);
        }

        std::mem::swap(&mut new_nodes, &mut self.nodes);
    }

    /*
     * Checking if a node is control requires surrounding context, so this is a
     * member of Function, not Node.
     */
    pub fn is_control(&self, id: NodeID) -> bool {
        if self.nodes[id.idx()].is_strictly_control() {
            return true;
        }

        if let Node::ReadProd { prod, index: _ } = self.nodes[id.idx()] {
            return match self.nodes[prod.idx()] {
                // ReadProd nodes are control nodes if their predecessor is a
                // legal control node.
                Node::Match { control: _, sum: _ }
                | Node::If {
                    control: _,
                    cond: _,
                } => true,
                _ => false,
            };
        }

        false
    }
}

/*
 * Hercules IR has a fairly standard type system, with the exception of the
 * control type. Hercules IR is based off of the sea-of-nodes IR, the main
 * feature of which being a merged control and data flow graph. Thus, control
 * is a type of value, just like any other type. However, the type system is
 * very restrictive over what can be done with control values. A novel addition
 * in Hercules IR is that a control type is parameterized by a list of thread
 * spawning factors. This is the mechanism in Hercules IR for representing
 * parallelism. Summation types are an IR equivalent of Rust's enum types.
 * These are lowered into tagged unions during scheduling. Array types are one-
 * dimensional. Multi-dimensional arrays are represented by nesting array types.
 * An array extent is represented with a dynamic constant.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Control(Box<[NodeID]>),
    Boolean,
    Integer8,
    Integer16,
    Integer32,
    Integer64,
    UnsignedInteger8,
    UnsignedInteger16,
    UnsignedInteger32,
    UnsignedInteger64,
    Float32,
    Float64,
    Product(Box<[TypeID]>),
    Summation(Box<[TypeID]>),
    Array(TypeID, DynamicConstantID),
}

impl Type {
    pub fn is_control(&self) -> bool {
        if let Type::Control(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_bool(&self) -> bool {
        self == &Type::Boolean
    }

    pub fn is_unsigned(&self) -> bool {
        match self {
            Type::UnsignedInteger8 => true,
            Type::UnsignedInteger16 => true,
            Type::UnsignedInteger32 => true,
            Type::UnsignedInteger64 => true,
            _ => false,
        }
    }

    pub fn is_fixed(&self) -> bool {
        match self {
            Type::Integer8 => true,
            Type::Integer16 => true,
            Type::Integer32 => true,
            Type::Integer64 => true,
            Type::UnsignedInteger8 => true,
            Type::UnsignedInteger16 => true,
            Type::UnsignedInteger32 => true,
            Type::UnsignedInteger64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::Float32 => true,
            Type::Float64 => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        self.is_fixed() || self.is_float()
    }

    pub fn is_primitive(&self) -> bool {
        self.is_bool() || self.is_fixed() || self.is_float()
    }
}

/*
 * Constants are pretty standard in Hercules IR. Float constants used the
 * ordered_float crate so that constants can be keys in maps (used for
 * interning constants during IR construction). Product, summation, and array
 * constants all contain their own type. This is only strictly necessary for
 * summation types, but provides a nice mechanism for sanity checking for
 * product and array types as well.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Boolean(bool),
    Integer8(i8),
    Integer16(i16),
    Integer32(i32),
    Integer64(i64),
    UnsignedInteger8(u8),
    UnsignedInteger16(u16),
    UnsignedInteger32(u32),
    UnsignedInteger64(u64),
    Float32(ordered_float::OrderedFloat<f32>),
    Float64(ordered_float::OrderedFloat<f64>),
    Product(TypeID, Box<[ConstantID]>),
    Summation(TypeID, u32, ConstantID),
    Array(TypeID, Box<[ConstantID]>),
}

impl Constant {
    /*
     * Useful for GVN.
     */
    pub fn is_zero(&self) -> bool {
        match self {
            Constant::Integer8(0) => true,
            Constant::Integer16(0) => true,
            Constant::Integer32(0) => true,
            Constant::Integer64(0) => true,
            Constant::UnsignedInteger8(0) => true,
            Constant::UnsignedInteger16(0) => true,
            Constant::UnsignedInteger32(0) => true,
            Constant::UnsignedInteger64(0) => true,
            Constant::Float32(ord) => *ord == ordered_float::OrderedFloat::<f32>(0.0),
            Constant::Float64(ord) => *ord == ordered_float::OrderedFloat::<f64>(0.0),
            _ => false,
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            Constant::Integer8(1) => true,
            Constant::Integer16(1) => true,
            Constant::Integer32(1) => true,
            Constant::Integer64(1) => true,
            Constant::UnsignedInteger8(1) => true,
            Constant::UnsignedInteger16(1) => true,
            Constant::UnsignedInteger32(1) => true,
            Constant::UnsignedInteger64(1) => true,
            Constant::Float32(ord) => *ord == ordered_float::OrderedFloat::<f32>(1.0),
            Constant::Float64(ord) => *ord == ordered_float::OrderedFloat::<f64>(1.0),
            _ => false,
        }
    }
}

/*
 * Dynamic constants are unsigned 64-bit integers passed to a Hercules function
 * at runtime using the Hercules conductor API. They cannot be the result of
 * computations in Hercules IR. For a single execution of a Hercules function,
 * dynamic constants are constant throughout execution. This provides a
 * mechanism by which Hercules functions can operate on arrays with variable
 * length, while not needing Hercules functions to perform dynamic memory
 * allocation - by providing dynamic constants to the conductor API, the
 * conductor can allocate memory as necessary.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DynamicConstant {
    Constant(usize),
    Parameter(usize),
}

/*
 * Hercules IR is a combination of a possibly cylic control flow graph, and
 * many possibly cyclic data flow graphs. Each node represents some operation on
 * input values (including control), and produces some output value. Operations
 * that conceptually produce multiple outputs (such as an if node) produce a
 * product type instead. For example, the if node produces prod(control(N),
 * control(N)), where the first control token represents the false branch, and
 * the second control token represents the true branch. Functions are devoid of
 * side effects, so call nodes don't take as input or output control tokens.
 * There is also no global memory - use arrays.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Start,
    Region {
        preds: Box<[NodeID]>,
    },
    If {
        control: NodeID,
        cond: NodeID,
    },
    Fork {
        control: NodeID,
        factor: DynamicConstantID,
    },
    Join {
        control: NodeID,
    },
    Phi {
        control: NodeID,
        data: Box<[NodeID]>,
    },
    ThreadID {
        control: NodeID,
    },
    Collect {
        control: NodeID,
        data: NodeID,
    },
    Return {
        control: NodeID,
        data: NodeID,
    },
    Parameter {
        index: usize,
    },
    Constant {
        id: ConstantID,
    },
    DynamicConstant {
        id: DynamicConstantID,
    },
    Unary {
        input: NodeID,
        op: UnaryOperator,
    },
    Binary {
        left: NodeID,
        right: NodeID,
        op: BinaryOperator,
    },
    Call {
        function: FunctionID,
        dynamic_constants: Box<[DynamicConstantID]>,
        args: Box<[NodeID]>,
    },
    ReadProd {
        prod: NodeID,
        index: usize,
    },
    WriteProd {
        prod: NodeID,
        data: NodeID,
        index: usize,
    },
    ReadArray {
        array: NodeID,
        index: NodeID,
    },
    WriteArray {
        array: NodeID,
        data: NodeID,
        index: NodeID,
    },
    Match {
        control: NodeID,
        sum: NodeID,
    },
    BuildSum {
        data: NodeID,
        sum_ty: TypeID,
        variant: usize,
    },
    ExtractSum {
        data: NodeID,
        variant: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Not,
    Neg,
    Bitflip,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    LT,
    LTE,
    GT,
    GTE,
    EQ,
    NE,
    Or,
    And,
    Xor,
    LSh,
    RSh,
}

/*
 * Simple predicate functions on nodes take a lot of space, so use a macro.
 */

macro_rules! define_pattern_predicate {
    ($x: ident, $y: pat) => {
        pub fn $x(&self) -> bool {
            if let $y = self {
                true
            } else {
                false
            }
        }
    };
}

impl Node {
    define_pattern_predicate!(is_start, Node::Start);
    define_pattern_predicate!(is_region, Node::Region { preds: _ });
    define_pattern_predicate!(
        is_if,
        Node::If {
            control: _,
            cond: _,
        }
    );
    define_pattern_predicate!(
        is_fork,
        Node::Fork {
            control: _,
            factor: _,
        }
    );
    define_pattern_predicate!(is_join, Node::Join { control: _ });
    define_pattern_predicate!(
        is_phi,
        Node::Phi {
            control: _,
            data: _,
        }
    );
    define_pattern_predicate!(is_thread_id, Node::ThreadID { control: _ });
    define_pattern_predicate!(
        is_collect,
        Node::Collect {
            control: _,
            data: _,
        }
    );
    define_pattern_predicate!(
        is_return,
        Node::Return {
            control: _,
            data: _,
        }
    );
    define_pattern_predicate!(is_match, Node::Match { control: _, sum: _ });

    /*
     * ReadProd nodes can be considered control when following an if or match
     * node. However, it is sometimes useful to exclude such nodes when
     * considering control nodes.
     */
    pub fn is_strictly_control(&self) -> bool {
        self.is_start()
            || self.is_region()
            || self.is_if()
            || self.is_fork()
            || self.is_join()
            || self.is_return()
            || self.is_return()
    }

    pub fn upper_case_name(&self) -> &'static str {
        match self {
            Node::Start => "Start",
            Node::Region { preds: _ } => "Region",
            Node::If {
                control: _,
                cond: _,
            } => "If",
            Node::Fork {
                control: _,
                factor: _,
            } => "Fork",
            Node::Join { control: _ } => "Join",
            Node::Phi {
                control: _,
                data: _,
            } => "Phi",
            Node::ThreadID { control: _ } => "ThreadID",
            Node::Collect {
                control: _,
                data: _,
            } => "Collect",
            Node::Return {
                control: _,
                data: _,
            } => "Return",
            Node::Parameter { index: _ } => "Parameter",
            Node::DynamicConstant { id: _ } => "DynamicConstant",
            Node::Constant { id: _ } => "Constant",
            Node::Unary { input: _, op } => op.upper_case_name(),
            Node::Binary {
                left: _,
                right: _,
                op,
            } => op.upper_case_name(),
            Node::Call {
                function: _,
                dynamic_constants: _,
                args: _,
            } => "Unary",
            Node::ReadProd { prod: _, index: _ } => "ReadProd",
            Node::WriteProd {
                prod: _,
                data: _,
                index: _,
            } => "WriteProd",
            Node::ReadArray { array: _, index: _ } => "ReadArray",
            Node::WriteArray {
                array: _,
                data: _,
                index: _,
            } => "WriteArray",
            Node::Match { control: _, sum: _ } => "Match",
            Node::BuildSum {
                data: _,
                sum_ty: _,
                variant: _,
            } => "BuildSum",
            Node::ExtractSum {
                data: _,
                variant: _,
            } => "ExtractSum",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            Node::Start => "start",
            Node::Region { preds: _ } => "region",
            Node::If {
                control: _,
                cond: _,
            } => "if",
            Node::Fork {
                control: _,
                factor: _,
            } => "fork",
            Node::Join { control: _ } => "join",
            Node::Phi {
                control: _,
                data: _,
            } => "phi",
            Node::ThreadID { control: _ } => "thread_id",
            Node::Collect {
                control: _,
                data: _,
            } => "collect",
            Node::Return {
                control: _,
                data: _,
            } => "return",
            Node::Parameter { index: _ } => "parameter",
            Node::DynamicConstant { id: _ } => "dynamic_constant",
            Node::Constant { id: _ } => "constant",
            Node::Unary { input: _, op } => op.lower_case_name(),
            Node::Binary {
                left: _,
                right: _,
                op,
            } => op.lower_case_name(),
            Node::Call {
                function: _,
                dynamic_constants: _,
                args: _,
            } => "call",
            Node::ReadProd { prod: _, index: _ } => "read_prod",
            Node::WriteProd {
                prod: _,
                data: _,
                index: _,
            } => "write_prod ",
            Node::ReadArray { array: _, index: _ } => "read_array",
            Node::WriteArray {
                array: _,
                data: _,
                index: _,
            } => "write_array",
            Node::Match { control: _, sum: _ } => "match",
            Node::BuildSum {
                data: _,
                sum_ty: _,
                variant: _,
            } => "build_sum",
            Node::ExtractSum {
                data: _,
                variant: _,
            } => "extract_sum",
        }
    }
}

impl UnaryOperator {
    pub fn upper_case_name(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "Not",
            UnaryOperator::Neg => "Neg",
            UnaryOperator::Bitflip => "Bitflip",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "not",
            UnaryOperator::Neg => "neg",
            UnaryOperator::Bitflip => "bitflip",
        }
    }
}

impl BinaryOperator {
    pub fn upper_case_name(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "Add",
            BinaryOperator::Sub => "Sub",
            BinaryOperator::Mul => "Mul",
            BinaryOperator::Div => "Div",
            BinaryOperator::Rem => "Rem",
            BinaryOperator::LT => "LT",
            BinaryOperator::LTE => "LTE",
            BinaryOperator::GT => "GT",
            BinaryOperator::GTE => "GTE",
            BinaryOperator::EQ => "EQ",
            BinaryOperator::NE => "NE",
            BinaryOperator::Or => "Or",
            BinaryOperator::And => "And",
            BinaryOperator::Xor => "Xor",
            BinaryOperator::LSh => "LSh",
            BinaryOperator::RSh => "RSh",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "add",
            BinaryOperator::Sub => "sub",
            BinaryOperator::Mul => "mul",
            BinaryOperator::Div => "div",
            BinaryOperator::Rem => "rem",
            BinaryOperator::LT => "lt",
            BinaryOperator::LTE => "lte",
            BinaryOperator::GT => "gt",
            BinaryOperator::GTE => "gte",
            BinaryOperator::EQ => "eq",
            BinaryOperator::NE => "ne",
            BinaryOperator::Or => "or",
            BinaryOperator::And => "and",
            BinaryOperator::Xor => "xor",
            BinaryOperator::LSh => "lsh",
            BinaryOperator::RSh => "rsh",
        }
    }
}

/*
 * Rust things to make newtyped IDs usable.
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionID(u32);

impl FunctionID {
    pub fn new(x: usize) -> Self {
        FunctionID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeID(u32);

impl NodeID {
    pub fn new(x: usize) -> Self {
        NodeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ConstantID(u32);

impl ConstantID {
    pub fn new(x: usize) -> Self {
        ConstantID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeID(u32);

impl TypeID {
    pub fn new(x: usize) -> Self {
        TypeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DynamicConstantID(u32);

impl DynamicConstantID {
    pub fn new(x: usize) -> Self {
        DynamicConstantID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}
