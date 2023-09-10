extern crate nom;

use std::collections::HashMap;

use crate::*;

pub fn parse(ir_test: &str) -> Module {
    parse_module(ir_test, Context::default()).unwrap().1
}

#[derive(Default)]
struct Context<'a> {
    function_ids: HashMap<&'a str, FunctionID>,
    node_ids: HashMap<&'a str, NodeID>,
    interned_types: HashMap<Type, TypeID>,
    interned_constants: HashMap<Constant, ConstantID>,
    interned_dynamic_constants: HashMap<DynamicConstant, DynamicConstantID>,
}

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
            self.interned_types.insert(ty, id);
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
            self.interned_dynamic_constants.insert(dynamic_constant, id);
            id
        }
    }
}

fn parse_module<'a>(ir_text: &'a str, mut context: Context<'a>) -> nom::IResult<&'a str, Module> {
    let (rest, functions) =
        nom::combinator::all_consuming(nom::multi::many0(|x| parse_function(x, &mut context)))(
            ir_text,
        )?;
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
        let function_id = context.function_ids.remove(function_name.as_str()).unwrap();
        fixed_functions[function_id.idx()] = function;
    }
    let mut types = vec![Type::Control(DynamicConstantID::new(0)); context.interned_types.len()];
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
    Ok((
        rest,
        Module {
            functions: fixed_functions,
            types,
            constants,
            dynamic_constants,
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
    let (ir_text, function_name) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let (ir_text, params) = nom::multi::separated_list0(
        nom::character::complete::char(','),
        nom::sequence::tuple((
            nom::character::complete::multispace0,
            nom::character::complete::alphanumeric1,
            nom::character::complete::multispace0,
            nom::character::complete::char(':'),
            nom::character::complete::multispace0,
            |x| parse_type_id(x, context),
            nom::character::complete::multispace0,
        )),
    )(ir_text)?;
    context.node_ids.insert("start", NodeID::new(0));
    for param in params.iter() {
        context
            .node_ids
            .insert(param.1, NodeID::new(context.node_ids.len()));
    }
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("->")(ir_text)?.0;
    let (ir_text, return_type) = parse_type_id(ir_text, context)?;
    let (ir_text, nodes) = nom::multi::many1(|x| parse_node(x, context))(ir_text)?;
    let mut fixed_nodes = vec![Node::Start; context.node_ids.len()];
    for (name, node) in nodes {
        fixed_nodes[context.node_ids.remove(name).unwrap().idx()] = node;
    }
    for (_, id) in context.node_ids.iter() {
        if id.idx() != 0 {
            fixed_nodes[id.idx()] = Node::Parameter { index: id.idx() }
        }
    }
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    context.get_function_id(function_name);
    Ok((
        ir_text,
        Function {
            name: String::from(function_name),
            param_types: params.into_iter().map(|x| x.5).collect(),
            return_type,
            nodes: fixed_nodes,
            num_dynamic_constants: 0,
        },
    ))
}

fn parse_node<'a>(
    ir_text: &'a str,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, (&'a str, Node)> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, node_name) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('=')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, node_kind) = nom::character::complete::alphanumeric1(ir_text)?;
    let (ir_text, node) = match node_kind {
        "return" => parse_return(ir_text, context)?,
        "constant" => parse_constant_node(ir_text, context)?,
        "add" => parse_add(ir_text, context)?,
        "call" => parse_call(ir_text, context)?,
        _ => todo!(),
    };
    context.get_node_id(node_name);
    Ok((ir_text, (node_name, node)))
}

fn parse_return<'a>(ir_text: &'a str, context: &mut Context<'a>) -> nom::IResult<&'a str, Node> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, control) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(',')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, value) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    Ok((
        ir_text,
        Node::Return {
            control: context.get_node_id(control),
            value: context.get_node_id(value),
        },
    ))
}

fn parse_constant_node<'a>(
    ir_text: &'a str,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, Node> {
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

fn parse_add<'a>(ir_text: &'a str, context: &mut Context<'a>) -> nom::IResult<&'a str, Node> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, control) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(',')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, left) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(',')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, right) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    Ok((
        ir_text,
        Node::Add {
            control: context.get_node_id(control),
            left: context.get_node_id(left),
            right: context.get_node_id(right),
        },
    ))
}

fn parse_call<'a>(ir_text: &'a str, context: &mut Context<'a>) -> nom::IResult<&'a str, Node> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, control) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(',')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, mut function_and_args) = nom::multi::separated_list1(
        nom::sequence::tuple((
            nom::character::complete::multispace0,
            nom::character::complete::char(','),
            nom::character::complete::multispace0,
        )),
        nom::character::complete::alphanumeric1,
    )(ir_text)?;
    let function = function_and_args.remove(0);
    let args: Vec<NodeID> = function_and_args
        .into_iter()
        .map(|x| context.get_node_id(x))
        .collect();
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    Ok((
        ir_text,
        Node::Call {
            control: context.get_node_id(control),
            function: context.get_function_id(function),
            args: args.into_boxed_slice(),
        },
    ))
}

fn parse_type_id<'a>(ir_text: &'a str, context: &mut Context<'a>) -> nom::IResult<&'a str, TypeID> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, ty) = parse_type(ir_text, context)?;
    let id = context.get_type_id(ty);
    Ok((ir_text, id))
}

fn parse_type<'a>(ir_text: &'a str, context: &mut Context<'a>) -> nom::IResult<&'a str, Type> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, ty) = nom::branch::alt((
        nom::combinator::map(
            nom::sequence::tuple((
                nom::bytes::complete::tag("ctrl"),
                nom::character::complete::multispace0,
                nom::character::complete::char('('),
                |x| parse_dynamic_constant_id(x, context),
                nom::character::complete::multispace0,
                nom::character::complete::char(')'),
            )),
            |(_, _, _, id, _, _)| Type::Control(id),
        ),
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
    ))(ir_text)?;
    Ok((ir_text, ty))
}

fn parse_dynamic_constant_id<'a>(
    ir_text: &'a str,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, DynamicConstantID> {
    let (ir_text, dynamic_constant) = parse_dynamic_constant(ir_text)?;
    let id = context.get_dynamic_constant_id(dynamic_constant);
    Ok((ir_text, id))
}

fn parse_dynamic_constant<'a>(ir_text: &'a str) -> nom::IResult<&'a str, DynamicConstant> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let parse_usize = |x: &'a str| -> nom::IResult<&'a str, usize> {
        let (ir_text, num_text) = nom::bytes::complete::is_a("-1234567890")(x)?;
        let num = num_text.parse::<usize>().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input: num_text,
                code: nom::error::ErrorKind::IsNot,
            })
        })?;
        Ok((ir_text, num))
    };
    let (ir_text, dc) = nom::branch::alt((
        nom::combinator::map(parse_usize, |x| DynamicConstant::Constant(x)),
        nom::combinator::map(
            nom::sequence::tuple((nom::character::complete::char('#'), parse_usize)),
            |(_, x)| DynamicConstant::Parameter(x),
        ),
    ))(ir_text)?;
    Ok((ir_text, dc))
}

fn parse_constant_id<'a>(
    ir_text: &'a str,
    ty: Type,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, ConstantID> {
    let (ir_text, constant) = parse_constant(ir_text, ty, context)?;
    let id = context.get_constant_id(constant);
    Ok((ir_text, id))
}

fn parse_constant<'a>(
    ir_text: &'a str,
    ty: Type,
    context: &mut Context<'a>,
) -> nom::IResult<&'a str, Constant> {
    let (ir_text, constant) = match ty {
        Type::Integer8 => parse_integer8(ir_text)?,
        Type::Integer32 => parse_integer32(ir_text)?,
        _ => todo!(),
    };
    context.get_type_id(ty);
    Ok((ir_text, constant))
}

fn parse_integer8<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num_text) = nom::bytes::complete::is_a("-1234567890")(ir_text)?;
    let num = num_text.parse::<i8>().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: num_text,
            code: nom::error::ErrorKind::IsNot,
        })
    })?;
    Ok((ir_text, Constant::Integer8(num)))
}

fn parse_integer32<'a>(ir_text: &'a str) -> nom::IResult<&'a str, Constant> {
    let (ir_text, num_text) = nom::bytes::complete::is_a("-1234567890")(ir_text)?;
    let num = num_text.parse::<i32>().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: num_text,
            code: nom::error::ErrorKind::IsNot,
        })
    })?;
    Ok((ir_text, Constant::Integer32(num)))
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn parse_ir1() {
        let module = parse(
            "
fn myfunc(x: i32) -> i32
  y = call(start, add, x, x)
  r = return(start, y)

fn add(x: i32, y: i32) -> i32
  c = constant(i8, 5)
  r = return(start, w)
  w = add(start, z, c)
  z = add(start, x, y)
",
        );
    }
}
