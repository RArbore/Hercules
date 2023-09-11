extern crate nom;

use std::cell::RefCell;
use std::collections::HashMap;
use std::str::FromStr;

use crate::*;

pub fn parse(ir_test: &str) -> Module {
    parse_module(ir_test, Context::default()).unwrap().1
}

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

fn parse_module<'a>(ir_text: &'a str, context: Context<'a>) -> nom::IResult<&'a str, Module> {
    let context = RefCell::new(context);
    let (rest, functions) =
        nom::combinator::all_consuming(nom::multi::many0(|x| parse_function(x, &context)))(
            ir_text,
        )?;
    let mut context = context.into_inner();
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
    let module = Module {
        functions: fixed_functions,
        types,
        constants,
        dynamic_constants,
    };
    Ok((rest, module))
}

fn parse_function<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Function> {
    context.borrow_mut().node_ids.clear();
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("fn")(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, function_name) = nom::character::complete::alphanumeric1(ir_text)?;
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
    let num_dynamic_constants = num_dynamic_constants.unwrap_or(0);
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
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
    context
        .borrow_mut()
        .node_ids
        .insert("start", NodeID::new(0));
    for param in params.iter() {
        let id = NodeID::new(context.borrow().node_ids.len());
        context.borrow_mut().node_ids.insert(param.1, id);
    }
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::bytes::complete::tag("->")(ir_text)?.0;
    let (ir_text, return_type) = parse_type_id(ir_text, context)?;
    let (ir_text, nodes) = nom::multi::many1(|x| parse_node(x, context))(ir_text)?;
    let mut fixed_nodes = vec![Node::Start; context.borrow().node_ids.len()];
    for (name, node) in nodes {
        fixed_nodes[context.borrow_mut().node_ids.remove(name).unwrap().idx()] = node;
    }
    for (_, id) in context.borrow().node_ids.iter() {
        if id.idx() != 0 {
            fixed_nodes[id.idx()] = Node::Parameter { index: id.idx() }
        }
    }
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
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

fn parse_node<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
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
    context.borrow_mut().get_node_id(node_name);
    Ok((ir_text, (node_name, node)))
}

fn parse_return<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Node> {
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
    let control = context.borrow_mut().get_node_id(control);
    let value = context.borrow_mut().get_node_id(value);
    Ok((ir_text, Node::Return { control, value }))
}

fn parse_constant_node<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
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

fn parse_add<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Node> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char('(')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, left) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(',')(ir_text)?.0;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, right) = nom::character::complete::alphanumeric1(ir_text)?;
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let ir_text = nom::character::complete::char(')')(ir_text)?.0;
    let left = context.borrow_mut().get_node_id(left);
    let right = context.borrow_mut().get_node_id(right);
    Ok((ir_text, Node::Add { left, right }))
}

fn parse_call<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Node> {
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
        nom::character::complete::alphanumeric1,
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

fn parse_type_id<'a>(
    ir_text: &'a str,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, TypeID> {
    let ir_text = nom::character::complete::multispace0(ir_text)?.0;
    let (ir_text, ty) = parse_type(ir_text, context)?;
    let id = context.borrow_mut().get_type_id(ty);
    Ok((ir_text, id))
}

fn parse_type<'a>(ir_text: &'a str, context: &RefCell<Context<'a>>) -> nom::IResult<&'a str, Type> {
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
            |(_, _, _, _, ty_id, _, _, _, dc_ids, _, _)| {
                Type::Array(ty_id, dc_ids.into_boxed_slice())
            },
        ),
    ))(ir_text)?;
    Ok((ir_text, ty))
}

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

fn parse_constant<'a>(
    ir_text: &'a str,
    ty: Type,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Constant> {
    let (ir_text, constant) = match ty.clone() {
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
        Type::Array(elem_ty, dc_bounds) => parse_array_constant(
            ir_text,
            context.borrow_mut().get_type_id(ty.clone()),
            elem_ty,
            dc_bounds,
            context,
        )?,
    };
    context.borrow_mut().get_type_id(ty);
    Ok((ir_text, constant))
}

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
    dc_bounds: Box<[DynamicConstantID]>,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, Constant> {
    let mut bounds = vec![];
    let borrow = context.borrow();
    let mut total_elems = 1;
    for dc in dc_bounds.iter() {
        let dc = borrow.reverse_dynamic_constant_map.get(dc).unwrap();
        match dc {
            DynamicConstant::Constant(b) => {
                if *b == 0 {
                    Err(nom::Err::Error(nom::error::Error {
                        input: ir_text,
                        code: nom::error::ErrorKind::IsNot,
                    }))?
                }
                total_elems *= b;
                bounds.push(*b);
            }
            _ => Err(nom::Err::Error(nom::error::Error {
                input: ir_text,
                code: nom::error::ErrorKind::IsNot,
            }))?,
        }
    }
    let mut contents = vec![];
    let ir_text =
        parse_array_constant_helper(ir_text, elem_ty, bounds.as_slice(), &mut contents, context)?.0;
    Ok((
        ir_text,
        Constant::Array(array_ty, contents.into_boxed_slice()),
    ))
}

fn parse_array_constant_helper<'a>(
    ir_text: &'a str,
    elem_ty: TypeID,
    bounds: &[usize],
    contents: &mut Vec<ConstantID>,
    context: &RefCell<Context<'a>>,
) -> nom::IResult<&'a str, ()> {
    if bounds.len() > 0 {
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char('[')(ir_text)?.0;
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let (ir_text, empties) = nom::multi::separated_list1(
            nom::sequence::tuple((
                nom::character::complete::multispace0,
                nom::character::complete::char(','),
                nom::character::complete::multispace0,
            )),
            |x| parse_array_constant_helper(x, elem_ty, bounds, contents, context),
        )(ir_text)?;
        if empties.len() != bounds[0] {
            Err(nom::Err::Error(nom::error::Error {
                input: ir_text,
                code: nom::error::ErrorKind::IsNot,
            }))?
        }
        let ir_text = nom::character::complete::multispace0(ir_text)?.0;
        let ir_text = nom::character::complete::char(']')(ir_text)?.0;
        Ok((ir_text, ()))
    } else {
        let (ir_text, id) = parse_constant_id(
            ir_text,
            context
                .borrow()
                .reverse_type_map
                .get(&elem_ty)
                .unwrap()
                .clone(),
            context,
        )?;
        contents.push(id);
        Ok((ir_text, ()))
    }
}

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
        );
    }
}
