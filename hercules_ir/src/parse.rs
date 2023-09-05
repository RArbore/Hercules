extern crate nom;

use std::collections::HashMap;

use crate::*;

fn parse(ir_test: &str) -> Module {
    parse_module(ir_test, Context::default()).unwrap().1
}

#[derive(Default)]
struct Context<'a> {
    function_ids: HashMap<&'a str, FunctionID>,
    node_ids: HashMap<&'a str, NodeID>,
    interned_types: HashMap<Type, TypeID>,
    interned_constants: HashMap<Constant, ConstantID>,
}

fn parse_module<'a>(ir_text: &'a str, mut context: Context<'a>) -> nom::IResult<&'a str, Module> {
    let (rest, functions) =
        nom::combinator::all_consuming(nom::multi::many0(|x| parse_function(x, &mut context)))(
            ir_text,
        )?;
    let mut types = vec![Type::Control(0); context.interned_types.len()];
    for (ty, id) in context.interned_types {
        types[id.idx()] = ty;
    }
    let mut constants = vec![Constant::Integer8(0); context.interned_constants.len()];
    for (constant, id) in context.interned_constants {
        constants[id.idx()] = constant;
    }
    Ok((
        rest,
        Module {
            functions,
            types,
            constants,
        },
    ))
}

fn parse_function<'a>(
    ir_text: &'a str,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, Function> {
    context.node_ids.clear();
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("fn")(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, function_name) = nom::character::complete::alphanumeric0(ir_text)?;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let (ir_text, params) = nom::multi::separated_list0(
        nom::character::complete::char(','),
        nom::sequence::tuple((
            nom::character::complete::alphanumeric1,
            nom::character::complete::multispace0,
            nom::character::complete::char(':'),
            nom::character::complete::multispace0,
            |x| parse_type(x, context),
        )),
    )(ir_text)?;
    for param in params.iter() {
        context
            .node_ids
            .insert(param.0, NodeID::new(context.node_ids.len()));
    }
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("->")(ir_text)?.0;
    let (ir_text, return_type) = parse_type(ir_text, context)?;
    let (ir_text, nodes) = nom::multi::many1(|x| parse_node(x, context))(ir_text)?;
    let mut fixed_nodes = vec![Node::Start; context.node_ids.len()];
    for (name, node) in nodes {
        fixed_nodes[context.node_ids.remove(name).unwrap().idx()] = node;
    }
    for (_, id) in context.node_ids.iter() {
        fixed_nodes[id.idx()] = Node::Parameter { index: id.idx() }
    }
    Ok((
        ir_text,
        Function {
            name: String::from(function_name),
            param_types: params.into_iter().map(|x| x.4).collect(),
            return_type,
            nodes: fixed_nodes,
        },
    ))
}

fn parse_node<'a>(
    ir_text: &'a str,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, (&'a str, Node)> {
    todo!()
}

fn parse_type<'a>(ir_text: &'a str, context: &mut Context<'a>) -> nom::IResult<&'a str, TypeID> {
    let (ir_text, ty) =
        nom::combinator::map(nom::bytes::complete::tag("i32"), |_| Type::Integer32)(ir_text)?;
    let id = if let Some(id) = context.interned_types.get(&ty) {
        *id
    } else {
        let id = TypeID::new(context.interned_types.len());
        context.interned_types.insert(ty, id);
        id
    };
    Ok((ir_text, id))
}

fn parse_constant<'a>(
    ir_text: &'a str,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, ConstantID> {
    todo!()
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn parse_ir1() {
        parse("fn add(x: i32, y: i32) -> i32 r = return(z) z = add(start, x, y)");
    }
}
