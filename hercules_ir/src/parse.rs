extern crate nom;

use std::cell::RefCell;
use std::collections::HashMap;
use std::str::FromStr;

use crate::*;

/*
 * Top level parse function.
 */
pub fn parse(ir_test: &str) -> Result<Module, ()> {
    parse_module(ir_test, Context::default())
        .map(|x| x.1)
        .map_err(|_| ())
}

/*
 * This is a context sensitive parser. We parse directly into the graph data
 * structure inside ir::Module, so this is where we perform interning.
 * We intern function names, node names, types, constants, and dynamic
 * constants. Sometimes, types and dynamic constants need to be looked up, so
 * we also maintain reverse intern maps for that purpose. IDs are assigned
 * in increasing order, based on the intern map's size.
 */
#[derive(Default)]
struct Context<'a> {
    function_ids: HashMap<&'a str, FunctionID>,
    node_ids: HashMap<&'a str, NodeID>,
    interned_types: HashMap<Type, TypeID>,
    reverse_type_map: HashMap<TypeID, Type>,
    interned_constants: HashMap<Constant, ConstantID>,
    interned_dynamic_constants: HashMap<DynamicConstant, DynamicConstantID>,
    reverse_dynamic_constant_map: HashMap<DynamicConstantID, DynamicConstant>,
}

/*
 * Interning functions. In general, all modifications to intern maps should be
 * done through these functions.
 */
impl<'a> Context<'a> {
    fn get_function_id(&mut self, name: &'a str) -> FunctionID {
        if let Some(id) = self.function_ids.get(name) {
            *id
        } else {
            let id = FunctionID::new(self.function_ids.len());
            self.function_ids.insert(name, id);
            id
        }
    }

    fn get_node_id(&mut self, name: &'a str) -> NodeID {
        if let Some(id) = self.node_ids.get(name) {
            *id
        } else {
            let id = NodeID::new(self.node_ids.len());
            self.node_ids.insert(name, id);
            id
        }
    }

    fn get_type_id(&mut self, ty: Type) -> TypeID {
        if let Some(id) = self.interned_types.get(&ty) {
            *id
        } else {
            let id = TypeID::new(self.interned_types.len());
            self.interned_types.insert(ty.clone(), id);
            self.reverse_type_map.insert(id, ty);
            id
        }
    }

    fn get_constant_id(&mut self, constant: Constant) -> ConstantID {
        if let Some(id) = self.interned_constants.get(&constant) {
            *id
        } else {
            let id = ConstantID::new(self.interned_constants.len());
            self.interned_constants.insert(constant, id);
            id
        }
    }

    fn get_dynamic_constant_id(&mut self, dynamic_constant: DynamicConstant) -> DynamicConstantID {
        if let Some(id) = self.interned_dynamic_constants.get(&dynamic_constant) {
            *id
        } else {
            let id = DynamicConstantID::new(self.interned_dynamic_constants.len());
            self.interned_dynamic_constants
                .insert(dynamic_constant.clone(), id);
            self.reverse_dynamic_constant_map
                .insert(id, dynamic_constant);
            id
        }
    }
}

/*
 * A module is just a file with a list of functions.
 */
fn parse_module<'a>(ir_text: &'a str, context: Context<'a>) -> nom::IResult<&'a str, Module> {
    let context = RefCell::new(context);

    // If there is any text left after successfully parsing some functions,
    // treat that as an error.
    let (rest, functions) =
        nom::combinator::all_consuming(nom::multi::many0(|x| parse_function(x, &context)))(
            ir_text,
        )?;
    let mut context = context.into_inner();

    // functions, as returned by parsing, is in parse order, which may differ
    // from the order dictated by FunctionIDs in the function name intern map.
    let mut fixed_functions = vec![
        Function {
            name: String::from(""),
            param_types: vec![],
            return_type: TypeID::new(0),
            nodes: vec![],
            num_dynamic_constants: 0
        };
        context.function_ids.len()
    ];
    for function in functions {
        let function_name = function.name.clone();

        // We can remove items from context now, as it's going to be destroyed
        // anyway.
        let function_id = context.function_ids.remove(function_name.as_str()).unwrap();
        fixed_functions[function_id.idx()] = function;
    }

    // Assemble flat lists of interned goodies, now that we've figured out
    // everyones' IDs.
    let mut types = vec![Type::Control(Box::new([])); context.interned_types.len()];
    for (ty, id) in context.interned_types {
        types[id.idx()] = ty;
    }
    let mut constants = vec![Constant::Integer8(0); context.interned_constants.len()];
    for (constant, id) in context.interned_constants {
        constants[id.idx()] = constant;
    }
    let mut dynamic_constants =
        vec![DynamicConstant::Constant(0); context.interned_dynamic_constants.len()];
    for (dynamic_constant, id) in context.interned_dynamic_constants {
        dynamic_constants[id.idx()] = dynamic_constant;
    }
    let module = Module {
        functions: fixed_functions,
        types,
        constants,
        dynamic_constants,
    };
    Ok((rest, module))
}

/*
 * A function is a function declaration, followed by a list of node statements.
 */
fn parse_function<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Function> {
    // Each function contains its own list of interned nodes, so we need to
    // clear the node name intern map.
    context.borrow_mut().node_ids.clear();

    // This parser isn't split into lexing and parsing steps. So, we very
    // frequently need to eat whitespace. Is this ugly? Yes. Does it work? Also
    // yes.
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("fn")(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, function_name) = parse_identifier(ir_text)?;
    let parse_num_dynamic_constants = |ir_text: &'a str| -> nom::IResult<&'a str, u32> {
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char('<')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, num_dynamic_constants) = parse_prim::<u32>(ir_text, "1234567890")?;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char('>')(ir_text)?.0;
        Ok((ir_text, num_dynamic_constants))
    };
    let (ir_text, num_dynamic_constants) =
        nom::combinator::opt(parse_num_dynamic_constants)(ir_text)?;

    // If unspecified, assumed function has no dynamic constant arguments.
    let num_dynamic_constants = num_dynamic_constants.unwrap_or(0);
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let (ir_text, params) = nom::multi::separated_list0(
        nom::character::complete::char(','),
        nom::sequence::tuple((
            nom::character::complete::multispace0,
            parse_identifier,
            nom::character::complete::multispace0,
            nom::character::complete::char(':'),
            nom::character::complete::multispace0,
            |x| parse_type_id(x, context),
            nom::character::complete::multispace0,
        )),
    )(ir_text)?;

    // The start node is not explicitly specified in the textual IR, so create
    // it manually.
    context.borrow_mut().get_node_id("start");

    // Insert nodes for each parameter.
    for param in params.iter() {
        context.borrow_mut().get_node_id(param.1);
    }
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("->")(ir_text)?.0;
    let (ir_text, return_type) = parse_type_id(ir_text, context)?;
    let (ir_text, nodes) = nom::multi::many1(|x| parse_node(x, context))(ir_text)?;

    // nodes, as returned by parsing, is in parse order, which may differ from
    // the order dictated by NodeIDs in the node name intern map.
    let mut fixed_nodes = vec![Node::Start; context.borrow().node_ids.len()];
    for (name, node) in nodes {
        // We can remove items from the node name intern map now, as the map
        // will be cleared during the next iteration of parse_function.
        fixed_nodes[context.borrow_mut().node_ids.remove(name).unwrap().idx()] = node;
    }

    // The nodes removed from node_ids in the previous step are nodes that are
    // defined in statements parsed by parse_node. There are 2 kinds of nodes
    // that aren't defined in statements inside the function body: the start
    // node, and the parameter nodes. The node at ID 0 is already a start node,
    // by the initialization of fixed_nodes. Here, we set the other nodes to
    // parameter nodes. The node id in node_ids corresponds to the parameter
    // index + 1, because in parse_function, we add the parameter names to
    // node_ids (a.k.a. the node name intern map) in order, after adding the
    // start node.
    for (_, id) in context.borrow().node_ids.iter() {
        if id.idx() != 0 {
            fixed_nodes[id.idx()] = Node::Parameter {
                index: id.idx() - 1,
            }
        }
    }
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;

    // Intern function name.
    context.borrow_mut().get_function_id(function_name);
    Ok((
        ir_text,
        Function {
            name: String::from(function_name),
            param_types: params.into_iter().map(|x| x.5).collect(),
            return_type,
            nodes: fixed_nodes,
            num_dynamic_constants,
        },
    ))
}

/*
 * A node is a statement of the form a = b(c), where a is the name of the output
 * of the node, b is the node type, and c is a list of arguments.
 */
fn parse_node<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, (&'a str, Node)> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, node_name) = parse_identifier(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('=')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, node_kind) = parse_identifier(ir_text)?;
    let (ir_text, node) = match node_kind {
        "region" => parse_region(ir_text, context)?,
        "if" => parse_if(ir_text, context)?,
        "fork" => parse_fork(ir_text, context)?,
        "join" => parse_join(ir_text, context)?,
        "phi" => parse_phi(ir_text, context)?,
        "return" => parse_return(ir_text, context)?,
        "constant" => parse_constant_node(ir_text, context)?,
        "dynamic_constant" => parse_dynamic_constant_node(ir_text, context)?,
        // Unary and binary ops are spelled out in the textual format, but we
        // parse them into Unary or Binary node kinds.
        "not" => parse_unary(ir_text, context, UnaryOperator::Not)?,
        "neg" => parse_unary(ir_text, context, UnaryOperator::Neg)?,
        "bitflip" => parse_unary(ir_text, context, UnaryOperator::Bitflip)?,
        "add" => parse_binary(ir_text, context, BinaryOperator::Add)?,
        "sub" => parse_binary(ir_text, context, BinaryOperator::Sub)?,
        "mul" => parse_binary(ir_text, context, BinaryOperator::Mul)?,
        "div" => parse_binary(ir_text, context, BinaryOperator::Div)?,
        "rem" => parse_binary(ir_text, context, BinaryOperator::Rem)?,
        "lt" => parse_binary(ir_text, context, BinaryOperator::LT)?,
        "lte" => parse_binary(ir_text, context, BinaryOperator::LTE)?,
        "gt" => parse_binary(ir_text, context, BinaryOperator::GT)?,
        "gte" => parse_binary(ir_text, context, BinaryOperator::GTE)?,
        "eq" => parse_binary(ir_text, context, BinaryOperator::EQ)?,
        "ne" => parse_binary(ir_text, context, BinaryOperator::NE)?,
        "lsh" => parse_binary(ir_text, context, BinaryOperator::LSh)?,
        "rsh" => parse_binary(ir_text, context, BinaryOperator::RSh)?,
        "call" => parse_call(ir_text, context)?,
        "read_prod" => parse_read_prod(ir_text, context)?,
        "write_prod" => parse_write_prod(ir_text, context)?,
        "read_array" => parse_read_array(ir_text, context)?,
        "write_array" => parse_write_array(ir_text, context)?,
        "match" => parse_match(ir_text, context)?,
        "build_sum" => parse_build_sum(ir_text, context)?,
        _ => Err(nom::Err::Error(nom::error::Error {
            input: ir_text,
            code: nom::error::ErrorKind::IsNot,
        }))?,
    };

    // Intern node name.
    context.borrow_mut().get_node_id(node_name);
    Ok((ir_text, (node_name, node)))
}

fn parse_region<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    // Each of these parse node functions are very similar. The node name and
    // type have already been parsed, so here we just parse the node's
    // arguments. These are always in between parantheses and separated by
    // commas, so there are parse_tupleN utility functions that do this. If
    // there is a variable amount of arguments, then we need to represent that
    // explicitly using nom's separated list functionality. This example here
    // is a bit of an abuse of what parse_tupleN functions are meant for.
    let (ir_text, (preds,)) = parse_tuple1(nom::multi::separated_list1(
        nom::sequence::tuple((
            nom::character::complete::multispace0,
            nom::character::complete::char(','),
            nom::character::complete::multispace0,
        )),
        parse_identifier,
    ))(ir_text)?;

    // When the parsed arguments are node names, we need to look up their ID in
    // the node name intern map.
    let preds = preds
        .into_iter()
        .map(|x| context.borrow_mut().get_node_id(x))
        .collect();
    Ok((ir_text, Node::Region { preds }))
}

fn parse_if<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Node> {
    let (ir_text, (control, cond)) = parse_tuple2(parse_identifier, parse_identifier)(ir_text)?;
    let control = context.borrow_mut().get_node_id(control);
    let cond = context.borrow_mut().get_node_id(cond);
    Ok((ir_text, Node::If { control, cond }))
}

fn parse_fork<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Node> {
    let (ir_text, (control, factor)) =
        parse_tuple2(parse_identifier, |x| parse_dynamic_constant_id(x, context))(ir_text)?;
    let control = context.borrow_mut().get_node_id(control);

    // Because parse_dynamic_constant_id returned a DynamicConstantID directly,
    // we don't need to manually convert it here.
    Ok((ir_text, Node::Fork { control, factor }))
}

fn parse_join<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Node> {
    let (ir_text, (control, data)) = parse_tuple2(parse_identifier, parse_identifier)(ir_text)?;
    let control = context.borrow_mut().get_node_id(control);
    let data = context.borrow_mut().get_node_id(data);

    // A join node doesn't need to explicitly store a join factor. The join
    // factor is implicitly stored at the tail of the control token's type
    // level list of thread spawn factors. Intuitively, fork pushes to the end
    // of this list, while join just pops from the end of this list.
    Ok((ir_text, Node::Join { control, data }))
}

fn parse_phi<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Node> {
    let (ir_text, (control, data)) = parse_tuple2(
        parse_identifier,
        nom::multi::separated_list1(
            nom::sequence::tuple((
                nom::character::complete::multispace0,
                nom::character::complete::char(','),
                nom::character::complete::multispace0,
            )),
            parse_identifier,
        ),
    )(ir_text)?;
    let control = context.borrow_mut().get_node_id(control);
    let data = data
        .into_iter()
        .map(|x| context.borrow_mut().get_node_id(x))
        .collect();
    Ok((ir_text, Node::Phi { control, data }))
}

fn parse_return<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (control, value)) = parse_tuple2(parse_identifier, parse_identifier)(ir_text)?;
    let control = context.borrow_mut().get_node_id(control);
    let value = context.borrow_mut().get_node_id(value);
    Ok((ir_text, Node::Return { control, value }))
}

fn parse_constant_node<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    // Here, we don't use parse_tuple2 because there is a dependency between
    // the parse functions of the 2 arguments.
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, ty) = parse_type(ir_text, context)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(',')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, id) = parse_constant_id(ir_text, ty, context)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    Ok((ir_text, Node::Constant { id }))
}

fn parse_dynamic_constant_node<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (id,)) = parse_tuple1(|x| parse_dynamic_constant_id(x, context))(ir_text)?;
    Ok((ir_text, Node::DynamicConstant { id }))
}

fn parse_unary<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
    op: UnaryOperator,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (input,)) = parse_tuple1(parse_identifier)(ir_text)?;
    let input = context.borrow_mut().get_node_id(input);
    Ok((ir_text, Node::Unary { input, op }))
}

fn parse_binary<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
    op: BinaryOperator,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (left, right)) = parse_tuple2(parse_identifier, parse_identifier)(ir_text)?;
    let left = context.borrow_mut().get_node_id(left);
    let right = context.borrow_mut().get_node_id(right);
    Ok((ir_text, Node::Binary { left, right, op }))
}

fn parse_call<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Node> {
    // Call nodes are a bit complicated because they 1. optionally take dynamic
    // constants as "arguments" (though these are specified between <>), 2.
    // take a function name as an argument, and 3. take a variable number of
    // normal arguments.
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let parse_dynamic_constants =
        |ir_text: &'a str| -> nom::IResult<&'a str, Vec<DynamicConstantID>> {
            let ir_text = nom::character::complete::char('<')(ir_text)?.0;
            let ir_text = nom::character::complete::multispace0(ir_text)?.0;
            let (ir_text, dynamic_constants) = nom::multi::separated_list1(
                nom::sequence::tuple((
                    nom::character::complete::multispace0,
                    nom::character::complete::char(','),
                    nom::character::complete::multispace0,
                )),
                |x| parse_dynamic_constant_id(x, context),
            )(ir_text)?;
            let ir_text = nom::character::complete::multispace0(ir_text)?.0;
            let ir_text = nom::character::complete::char('>')(ir_text)?.0;
            Ok((ir_text, dynamic_constants))
        };
    let (ir_text, dynamic_constants) = nom::combinator::opt(parse_dynamic_constants)(ir_text)?;
    let dynamic_constants = dynamic_constants.unwrap_or(vec![]);
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, mut function_and_args) = nom::multi::separated_list1(
        nom::sequence::tuple((
            nom::character::complete::multispace0,
            nom::character::complete::char(','),
            nom::character::complete::multispace0,
        )),
        parse_identifier,
    )(ir_text)?;
    let function = function_and_args.remove(0);
    let args: Vec<NodeID> = function_and_args
        .into_iter()
        .map(|x| context.borrow_mut().get_node_id(x))
        .collect();
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    let function = context.borrow_mut().get_function_id(function);
    Ok((
        ir_text,
        Node::Call {
            function,
            dynamic_constants: dynamic_constants.into_boxed_slice(),
            args: args.into_boxed_slice(),
        },
    ))
}

fn parse_read_prod<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (prod, index)) =
        parse_tuple2(parse_identifier, |x| parse_prim::<usize>(x, "1234567890"))(ir_text)?;
    let prod = context.borrow_mut().get_node_id(prod);
    Ok((ir_text, Node::ReadProd { prod, index }))
}

fn parse_write_prod<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (prod, data, index)) = parse_tuple3(parse_identifier, parse_identifier, |x| {
        parse_prim::<usize>(x, "1234567890")
    })(ir_text)?;
    let prod = context.borrow_mut().get_node_id(prod);
    let data = context.borrow_mut().get_node_id(data);
    Ok((ir_text, Node::WriteProd { prod, data, index }))
}

fn parse_read_array<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (array, index)) = parse_tuple2(parse_identifier, parse_identifier)(ir_text)?;
    let array = context.borrow_mut().get_node_id(array);
    let index = context.borrow_mut().get_node_id(index);
    Ok((ir_text, Node::ReadArray { array, index }))
}

fn parse_write_array<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (array, data, index)) =
        parse_tuple3(parse_identifier, parse_identifier, parse_identifier)(ir_text)?;
    let array = context.borrow_mut().get_node_id(array);
    let data = context.borrow_mut().get_node_id(data);
    let index = context.borrow_mut().get_node_id(index);
    Ok((ir_text, Node::WriteArray { array, data, index }))
}

fn parse_type_id<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, TypeID> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, ty) = parse_type(ir_text, context)?;
    let id = context.borrow_mut().get_type_id(ty);
    Ok((ir_text, id))
}

fn parse_match<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (control, sum)) = parse_tuple2(parse_identifier, parse_identifier)(ir_text)?;
    let control = context.borrow_mut().get_node_id(control);
    let sum = context.borrow_mut().get_node_id(sum);
    Ok((ir_text, Node::Match { control, sum }))
}

fn parse_build_sum<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
    let (ir_text, (data, sum_ty, variant)) = parse_tuple3(
        parse_identifier,
        |x| parse_type_id(x, context),
        |x| parse_prim::<usize>(x, "1234567890"),
    )(ir_text)?;
    let data = context.borrow_mut().get_node_id(data);
    Ok((
        ir_text,
        Node::BuildSum {
            data,
            sum_ty,
            variant,
        },
    ))
}

fn parse_type<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Type> {
    // Parser combinators are very convenient, if a bit hard to read.
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, ty) = nom::branch::alt((
        // Control tokens are parameterized by a list of dynamic constants
        // representing their thread spawn factors.
        nom::combinator::map(
            nom::sequence::tuple((
                nom::bytes::complete::tag("ctrl"),
                nom::character::complete::multispace0,
                nom::character::complete::char('('),
                nom::character::complete::multispace0,
                nom::multi::separated_list1(
                    nom::sequence::tuple((
                        nom::character::complete::multispace0,
                        nom::character::complete::char(','),
                        nom::character::complete::multispace0,
                    )),
                    |x| parse_dynamic_constant_id(x, context),
                ),
                nom::character::complete::multispace0,
                nom::character::complete::char(')'),
            )),
            |(_, _, _, _, id, _, _)| Type::Control(id.into_boxed_slice()),
        ),
        // If no arguments are provided, assumed that no forks have occurred.
        nom::combinator::map(nom::bytes::complete::tag("ctrl"), |_| {
            Type::Control(Box::new([]))
        }),
        // Primitive types are written in Rust style.
        nom::combinator::map(nom::bytes::complete::tag("i8"), |_| Type::Integer8),
        nom::combinator::map(nom::bytes::complete::tag("i16"), |_| Type::Integer16),
        nom::combinator::map(nom::bytes::complete::tag("i32"), |_| Type::Integer32),
        nom::combinator::map(nom::bytes::complete::tag("i64"), |_| Type::Integer64),
        nom::combinator::map(nom::bytes::complete::tag("u8"), |_| Type::UnsignedInteger8),
        nom::combinator::map(nom::bytes::complete::tag("u16"), |_| {
            Type::UnsignedInteger16
        }),
        nom::combinator::map(nom::bytes::complete::tag("u32"), |_| {
            Type::UnsignedInteger32
        }),
        nom::combinator::map(nom::bytes::complete::tag("u64"), |_| {
            Type::UnsignedInteger64
        }),
        nom::combinator::map(nom::bytes::complete::tag("f32"), |_| Type::Float32),
        nom::combinator::map(nom::bytes::complete::tag("f64"), |_| Type::Float64),
        // Product types are parsed as a list of their element types.
        nom::combinator::map(
            nom::sequence::tuple((
                nom::bytes::complete::tag("prod"),
                nom::character::complete::multispace0,
                nom::character::complete::char('('),
                nom::character::complete::multispace0,
                nom::multi::separated_list1(
                    nom::sequence::tuple((
                        nom::character::complete::multispace0,
                        nom::character::complete::char(','),
                        nom::character::complete::multispace0,
                    )),
                    |x| parse_type_id(x, context),
                ),
                nom::character::complete::multispace0,
                nom::character::complete::char(')'),
            )),
            |(_, _, _, _, ids, _, _)| Type::Product(ids.into_boxed_slice()),
        ),
        // Sum types are parsed as a list of their variant types.
        nom::combinator::map(
            nom::sequence::tuple((
                nom::bytes::complete::tag("sum"),
                nom::character::complete::multispace0,
                nom::character::complete::char('('),
                nom::character::complete::multispace0,
                nom::multi::separated_list1(
                    nom::sequence::tuple((
                        nom::character::complete::multispace0,
                        nom::character::complete::char(','),
                        nom::character::complete::multispace0,
                    )),
                    |x| parse_type_id(x, context),
                ),
                nom::character::complete::multispace0,
                nom::character::complete::char(')'),
            )),
            |(_, _, _, _, ids, _, _)| Type::Summation(ids.into_boxed_slice()),
        ),
        // Array types are just a pair between an element type and a dynamic
        // constant representing its extent.
        nom::combinator::map(
            nom::sequence::tuple((
                nom::bytes::complete::tag("array"),
                nom::character::complete::multispace0,
                nom::character::complete::char('('),
                nom::character::complete::multispace0,
                |x| parse_type_id(x, context),
                nom::character::complete::multispace0,
                nom::character::complete::char(','),
                nom::character::complete::multispace0,
                |x| parse_dynamic_constant_id(x, context),
                nom::character::complete::multispace0,
                nom::character::complete::char(')'),
            )),
            |(_, _, _, _, ty_id, _, _, _, dc_id, _, _)| Type::Array(ty_id, dc_id),
        ),
    ))(ir_text)?;
    Ok((ir_text, ty))
}

// For types, constants, and dynamic constant parse functions, there is a
// variant parsing the object itself, and a variant that parses the object and
// returns the interned ID.
fn parse_dynamic_constant_id<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, DynamicConstantID> {
    let (ir_text, dynamic_constant) = parse_dynamic_constant(ir_text)?;
    let id = context
        .borrow_mut()
        .get_dynamic_constant_id(dynamic_constant);
    Ok((ir_text, id))
}

fn parse_dynamic_constant<'a>(ir_text: &'a str) -> nom::IResult<&'a str, DynamicConstant> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, dc) = nom::branch::alt((
        nom::combinator::map(
            |x| parse_prim::<usize>(x, "1234567890"),
            |x| DynamicConstant::Constant(x),
        ),
        // Parameter dynamic constants of a function are written by preprending
        // a '#' to the parameter's number.
        nom::combinator::map(
            nom::sequence::tuple((nom::character::complete::char('#'), |x| {
                parse_prim::<usize>(x, "1234567890")
            })),
            |(_, x)| DynamicConstant::Parameter(x),
        ),
    ))(ir_text)?;
    Ok((ir_text, dc))
}

fn parse_constant_id<'a>(
    ir_text: &'a str,
    ty: Type,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, ConstantID> {
    let (ir_text, constant) = parse_constant(ir_text, ty, context)?;
    let id = context.borrow_mut().get_constant_id(constant);
    Ok((ir_text, id))
}

/*
 * parse_constant requires a type argument so that we know what we're parsing
 * upfront. Not having this would make parsing primitive constants much harder.
 * This is a bad requirement to have for a source language, but for a verbose
 * textual format for an IR, it's fine and simplifies the parser, typechecking,
 * and the IR itself.
 */
fn parse_constant<'a>(
    ir_text: &'a str,
    ty: Type,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Constant> {
    let (ir_text, constant) = match ty.clone() {
        // There are not control constants.
        Type::Control(_) => Err(nom::Err::Error(nom::error::Error {
            input: ir_text,
            code: nom::error::ErrorKind::IsNot,
        }))?,
        Type::Integer8 => parse_integer8(ir_text)?,
        Type::Integer16 => parse_integer16(ir_text)?,
        Type::Integer32 => parse_integer32(ir_text)?,
        Type::Integer64 => parse_integer64(ir_text)?,
        Type::UnsignedInteger8 => parse_unsigned_integer8(ir_text)?,
        Type::UnsignedInteger16 => parse_unsigned_integer16(ir_text)?,
        Type::UnsignedInteger32 => parse_unsigned_integer32(ir_text)?,
        Type::UnsignedInteger64 => parse_unsigned_integer64(ir_text)?,
        Type::Float32 => parse_float32(ir_text)?,
        Type::Float64 => parse_float64(ir_text)?,
        Type::Product(tys) => parse_product_constant(
            ir_text,
            context.borrow_mut().get_type_id(ty.clone()),
            tys,
            context,
        )?,
        Type::Summation(tys) => parse_summation_constant(
            ir_text,
            context.borrow_mut().get_type_id(ty.clone()),
            tys,
            context,
        )?,
        Type::Array(elem_ty, _) => parse_array_constant(
            ir_text,
            context.borrow_mut().get_type_id(ty.clone()),
            elem_ty,
            context,
        )?,
    };
    context.borrow_mut().get_type_id(ty);
    Ok((ir_text, constant))
}

/*
 * Utility for parsing types implementing FromStr.
 */
fn parse_prim<'a, T: FromStr>(ir_text: &'a str, chars: &'static str) -> nom::IResult<&'a str, T> {
    let (ir_text, x_text) = nom::bytes::complete::is_a(chars)(ir_text)?;
    let x = x_text.parse::<T>().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: x_text,
            code: nom::error::ErrorKind::IsNot,
        })
    })?;
    Ok((ir_text, x))
}

fn parse_integer8<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "-1234567890")?;
    Ok((ir_text, Constant::Integer8(num)))
}

fn parse_integer16<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "-1234567890")?;
    Ok((ir_text, Constant::Integer16(num)))
}

fn parse_integer32<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "-1234567890")?;
    Ok((ir_text, Constant::Integer32(num)))
}

fn parse_integer64<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "-1234567890")?;
    Ok((ir_text, Constant::Integer64(num)))
}

fn parse_unsigned_integer8<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "1234567890")?;
    Ok((ir_text, Constant::UnsignedInteger8(num)))
}

fn parse_unsigned_integer16<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "1234567890")?;
    Ok((ir_text, Constant::UnsignedInteger16(num)))
}

fn parse_unsigned_integer32<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "1234567890")?;
    Ok((ir_text, Constant::UnsignedInteger32(num)))
}

fn parse_unsigned_integer64<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = parse_prim(ir_text, "1234567890")?;
    Ok((ir_text, Constant::UnsignedInteger64(num)))
}

fn parse_float32<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = nom::number::complete::float(ir_text)?;
    Ok((
        ir_text,
        Constant::Float32(ordered_float::OrderedFloat::<f32>(num)),
    ))
}

fn parse_float64<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num) = nom::number::complete::double(ir_text)?;
    Ok((
        ir_text,
        Constant::Float64(ordered_float::OrderedFloat::<f64>(num)),
    ))
}

fn parse_product_constant<'a>(
    ir_text: &'a str,
    prod_ty: TypeID,
    tys: Box<[TypeID]>,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Constant> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("prod")(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let mut ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let mut subconstants = vec![];

    // There should be one constant for each element type.
    for ty in tys.iter() {
        if !subconstants.is_empty() {
            ir_text = nom::character::complete::multispace0(ir_text)?.0;
            ir_text = nom::character::complete::char(',')(ir_text)?.0;
        }
        ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text_tmp, id) = parse_constant_id(
            ir_text,
            context.borrow().reverse_type_map.get(ty).unwrap().clone(),
            context,
        )?;
        subconstants.push(id);
        ir_text = ir_text_tmp;
    }
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    Ok((
        ir_text,
        Constant::Product(prod_ty, subconstants.into_boxed_slice()),
    ))
}

fn parse_summation_constant<'a>(
    ir_text: &'a str,
    sum_ty: TypeID,
    tys: Box<[TypeID]>,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Constant> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("sum")(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;

    // Sum constants need to specify their variant number.
    let (ir_text, variant) = parse_prim::<u32>(ir_text, "1234567890")?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(',')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, id) = parse_constant_id(
        ir_text,
        context
            .borrow()
            .reverse_type_map
            .get(&tys[variant as usize])
            .unwrap()
            .clone(),
        context,
    )?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    Ok((ir_text, Constant::Summation(sum_ty, variant, id)))
}

fn parse_array_constant<'a>(
    ir_text: &'a str,
    array_ty: TypeID,
    elem_ty: TypeID,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Constant> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('[')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, entries) = nom::multi::separated_list1(
        nom::sequence::tuple((
            nom::character::complete::multispace0,
            nom::character::complete::char(','),
            nom::character::complete::multispace0,
        )),
        |x| {
            parse_constant_id(
                x,
                context
                    .borrow()
                    .reverse_type_map
                    .get(&elem_ty)
                    .unwrap()
                    .clone(),
                context,
            )
        },
    )(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(']')(ir_text)?.0;

    // Will check that entries is the correct size during typechecking.
    Ok((
        ir_text,
        Constant::Array(array_ty, entries.into_boxed_slice()),
    ))
}

fn parse_identifier<'a>(ir_text: &'a str) -> nom::IResult<&'a str, &'a str> {
    // Here's the set of characters that can be in an identifier. Must be non-
    // empty.
    nom::combinator::verify(
        nom::bytes::complete::is_a(
            "1234567890_@ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
        ),
        |s: &str| s.len() > 0,
    )(ir_text)
}

/*
 * Helper function for parsing tuples of arguments in the textual format.
 */
fn parse_tuple1<'a, A, AF>(mut parse_a: AF) -> impl FnMut(&'a str) -> nom::IResult<&'a str, (A,)>
where
    AF: nom::Parser<&'a str, A, nom::error::Error<&'a str>>,
{
    move |ir_text: &'a str| {
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char('(')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, a) = parse_a.parse(ir_text)?;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char(')')(ir_text)?.0;
        Ok((ir_text, (a,)))
    }
}

fn parse_tuple2<'a, A, B, AF, BF>(
    mut parse_a: AF,
    mut parse_b: BF,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, (A, B)>
where
    AF: nom::Parser<&'a str, A, nom::error::Error<&'a str>>,
    BF: nom::Parser<&'a str, B, nom::error::Error<&'a str>>,
{
    move |ir_text: &'a str| {
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char('(')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, a) = parse_a.parse(ir_text)?;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char(',')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, b) = parse_b.parse(ir_text)?;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char(')')(ir_text)?.0;
        Ok((ir_text, (a, b)))
    }
}

fn parse_tuple3<'a, A, B, C, AF, BF, CF>(
    mut parse_a: AF,
    mut parse_b: BF,
    mut parse_c: CF,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, (A, B, C)>
where
    AF: nom::Parser<&'a str, A, nom::error::Error<&'a str>>,
    BF: nom::Parser<&'a str, B, nom::error::Error<&'a str>>,
    CF: nom::Parser<&'a str, C, nom::error::Error<&'a str>>,
{
    move |ir_text: &'a str| {
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char('(')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, a) = parse_a.parse(ir_text)?;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char(',')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, b) = parse_b.parse(ir_text)?;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char(',')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, c) = parse_c.parse(ir_text)?;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char(')')(ir_text)?.0;
        Ok((ir_text, (a, b, c)))
    }
}

/*
 * Some tests that demonstrate what the textual format looks like.
 */
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn parse_ir1() {
        parse(
            "
fn myfunc(x: i32) -> i32
  y = call<0>(add, x, x)
  r = return(start, y)

fn add<1>(x: i32, y: i32) -> i32
  c = constant(i8, 5)
  r = return(start, w)
  w = add(z, c)
  z = add(x, y)
",
        )
        .unwrap();
    }
}
