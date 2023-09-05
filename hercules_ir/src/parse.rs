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
    let (rest, functions) = nom::multi::many0(|x| parse_function(x, &mut context))(ir_text)?;
    nom::combinator::eof(rest)?;
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
    todo!()
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn parse_ir1() {
        parse("fn add(x: i32, y: i32) -> i32 return(z) z = add(start, x, y)");
    }
}
