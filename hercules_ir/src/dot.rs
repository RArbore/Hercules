use crate::*;

use std::collections::HashMap;

pub fn write_dot<W: std::fmt::Write>(module: &Module, w: &mut W) -> std::fmt::Result {
    write!(w, "digraph \"Module\" {{\n")?;
    write!(w, "compound=true\n")?;
    for i in 0..module.functions.len() {
        write_function(i, module, &module.constants, w)?;
    }
    write!(w, "}}\n")?;
    Ok(())
}

fn write_function<W: std::fmt::Write>(
    i: usize,
    module: &Module,
    constants: &Vec<Constant>,
    w: &mut W,
) -> std::fmt::Result {
    write!(w, "subgraph {} {{\n", module.functions[i].name)?;
    write!(w, "label=\"{}\"\n", module.functions[i].name)?;
    write!(w, "bgcolor=ivory4\n")?;
    write!(w, "cluster=true\n")?;
    let mut visited = HashMap::default();
    let function = &module.functions[i];
    for j in 0..function.nodes.len() {
        visited = write_node(i, j, module, constants, visited, w)?.1;
    }
    write!(w, "}}\n")?;
    Ok(())
}

fn write_node<W: std::fmt::Write>(
    i: usize,
    j: usize,
    module: &Module,
    constants: &Vec<Constant>,
    mut visited: HashMap<NodeID, String>,
    w: &mut W,
) -> Result<(String, HashMap<NodeID, String>), std::fmt::Error> {
    let id = NodeID::new(j);
    if visited.contains_key(&id) {
        Ok((visited.get(&id).unwrap().clone(), visited))
    } else {
        let node = &module.functions[i].nodes[j];
        let name = format!("{}_{}_{}", get_string_node_kind(node), i, j);
        visited.insert(NodeID::new(j), name.clone());
        let visited = match node {
            Node::Start => {
                write!(w, "{} [label=\"start\"];\n", name)?;
                visited
            }
            Node::Return { control, value } => {
                let (control_name, visited) =
                    write_node(i, control.idx(), module, constants, visited, w)?;
                let (value_name, visited) =
                    write_node(i, value.idx(), module, constants, visited, w)?;
                write!(w, "{} [label=\"return\"];\n", name)?;
                write!(w, "{} -> {} [style=\"dashed\"];\n", control_name, name)?;
                write!(w, "{} -> {};\n", value_name, name)?;
                visited
            }
            Node::Parameter { index } => {
                write!(w, "{} [label=\"param #{}\"];\n", name, index)?;
                visited
            }
            Node::Constant { id } => {
                write!(w, "{} [label=\"{:?}\"];\n", name, constants[id.idx()])?;
                visited
            }
            Node::Add { left, right } => {
                let (left_name, visited) =
                    write_node(i, left.idx(), module, constants, visited, w)?;
                let (right_name, visited) =
                    write_node(i, right.idx(), module, constants, visited, w)?;
                write!(w, "{} [label=\"add\"];\n", name)?;
                write!(w, "{} -> {};\n", left_name, name)?;
                write!(w, "{} -> {};\n", right_name, name)?;
                visited
            }
            Node::Call {
                function,
                dynamic_constants,
                args,
            } => {
                for arg in args.iter() {
                    let (arg_name, tmp_visited) =
                        write_node(i, arg.idx(), module, constants, visited, w)?;
                    visited = tmp_visited;
                    write!(w, "{} -> {};\n", arg_name, name)?;
                }
                write!(
                    w,
                    "{} [label=\"call({})\"];\n",
                    name,
                    module.functions[function.idx()].name
                )?;
                write!(
                    w,
                    "{} -> start_{}_0 [lhead={}];\n",
                    name,
                    function.idx(),
                    module.functions[function.idx()].name
                )?;
                visited
            }
            _ => todo!(),
        };
        Ok((visited.get(&id).unwrap().clone(), visited))
    }
}

fn get_string_node_kind(node: &Node) -> &'static str {
    match node {
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
        Node::Join {
            control: _,
            factor: _,
        } => "join",
        Node::Phi {
            control: _,
            data: _,
        } => "phi",
        Node::Return {
            control: _,
            value: _,
        } => "return",
        Node::Parameter { index: _ } => "parameter",
        Node::DynamicConstant { id: _ } => "dynamic_constant",
        Node::Constant { id: _ } => "constant",
        Node::Add { left: _, right: _ } => "add",
        Node::Sub { left: _, right: _ } => "sub",
        Node::Mul { left: _, right: _ } => "mul",
        Node::Div { left: _, right: _ } => "div",
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
    }
}
