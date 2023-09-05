use crate::*;

use std::collections::HashMap;

pub fn write_dot<W: std::fmt::Write>(module: &Module, w: &mut W) -> std::fmt::Result {
    write!(w, "digraph \"Module\" {{\n")?;
    for i in 0..module.functions.len() {
        let function = &module.functions[i];
        write_function(i, function, &module.constants, w)?;
    }
    write!(w, "}}")?;
    Ok(())
}

fn write_function<W: std::fmt::Write>(
    i: usize,
    function: &Function,
    constants: &Vec<Constant>,
    w: &mut W,
) -> std::fmt::Result {
    let mut visited = HashMap::default();
    for j in 0..function.nodes.len() {
        visited = write_node(i, j, &function.nodes, constants, visited, w)?.1;
    }
    Ok(())
}

fn write_node<W: std::fmt::Write>(
    i: usize,
    j: usize,
    nodes: &Vec<Node>,
    constants: &Vec<Constant>,
    mut visited: HashMap<NodeID, String>,
    w: &mut W,
) -> Result<(String, HashMap<NodeID, String>), std::fmt::Error> {
    let id = NodeID::new(j);
    if visited.contains_key(&id) {
        Ok((visited.get(&id).unwrap().clone(), visited))
    } else {
        let node = &nodes[j];
        let name = format!("{}_{}_{}", get_string_node_kind(node), i, j);
        visited.insert(NodeID::new(j), name.clone());
        let visited = match node {
            Node::Start => {
                write!(w, "{} [label=\"start\"];\n", name)?;
                visited
            }
            Node::Return { control, value } => {
                let (control_name, visited) =
                    write_node(i, control.idx(), nodes, constants, visited, w)?;
                let (value_name, visited) =
                    write_node(i, value.idx(), nodes, constants, visited, w)?;
                write!(w, "{} [label=\"return\"];\n", name)?;
                write!(w, "{} -> {};\n", control_name, name)?;
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
            Node::Add {
                control,
                left,
                right,
            } => {
                let (control_name, visited) =
                    write_node(i, control.idx(), nodes, constants, visited, w)?;
                let (left_name, visited) = write_node(i, left.idx(), nodes, constants, visited, w)?;
                let (right_name, visited) =
                    write_node(i, right.idx(), nodes, constants, visited, w)?;
                write!(w, "{} [label=\"add\"];\n", name)?;
                write!(w, "{} -> {};\n", control_name, name)?;
                write!(w, "{} -> {};\n", left_name, name)?;
                write!(w, "{} -> {};\n", right_name, name)?;
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
        Node::Constant { id: _ } => "constant",
        Node::Add {
            control: _,
            left: _,
            right: _,
        } => "add",
        Node::Sub {
            control: _,
            left: _,
            right: _,
        } => "sub",
        Node::Mul {
            control: _,
            left: _,
            right: _,
        } => "mul",
        Node::Div {
            control: _,
            left: _,
            right: _,
        } => "div",
        Node::Call { args: _ } => "call",
    }
}
