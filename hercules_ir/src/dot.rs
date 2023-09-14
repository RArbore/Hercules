use crate::*;

use std::collections::HashMap;

pub fn write_dot<W: std::fmt::Write>(module: &Module, w: &mut W) -> std::fmt::Result {
    write!(w, "digraph \"Module\" {{\n")?;
    write!(w, "compound=true\n")?;
    for i in 0..module.functions.len() {
        write_function(i, module, w)?;
    }
    write!(w, "}}\n")?;
    Ok(())
}

fn write_function<W: std::fmt::Write>(i: usize, module: &Module, w: &mut W) -> std::fmt::Result {
    write!(w, "subgraph {} {{\n", module.functions[i].name)?;
    if module.functions[i].num_dynamic_constants > 0 {
        write!(
            w,
            "label=\"{}<{}>\"\n",
            module.functions[i].name, module.functions[i].num_dynamic_constants
        )?;
    } else {
        write!(w, "label=\"{}\"\n", module.functions[i].name)?;
    }
    write!(w, "bgcolor=ivory4\n")?;
    write!(w, "cluster=true\n")?;
    let mut visited = HashMap::default();
    let function = &module.functions[i];
    for j in 0..function.nodes.len() {
        visited = write_node(i, j, module, visited, w)?.1;
    }
    write!(w, "}}\n")?;
    Ok(())
}

fn write_node<W: std::fmt::Write>(
    i: usize,
    j: usize,
    module: &Module,
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
            Node::Region { preds } => {
                write!(w, "{} [label=\"region\"];\n", name)?;
                for (idx, pred) in preds.iter().enumerate() {
                    let (pred_name, tmp_visited) = write_node(i, pred.idx(), module, visited, w)?;
                    visited = tmp_visited;
                    write!(
                        w,
                        "{} -> {} [label=\"pred {}\", style=\"dashed\"];\n",
                        pred_name, name, idx
                    )?;
                }
                visited
            }
            Node::If { control, cond } => {
                write!(w, "{} [label=\"if\"];\n", name)?;
                let (control_name, visited) = write_node(i, control.idx(), module, visited, w)?;
                let (cond_name, visited) = write_node(i, cond.idx(), module, visited, w)?;
                write!(
                    w,
                    "{} -> {} [label=\"control\", style=\"dashed\"];\n",
                    control_name, name
                )?;
                write!(w, "{} -> {} [label=\"cond\"];\n", cond_name, name)?;
                visited
            }
            Node::Fork { control, factor } => {
                write!(
                    w,
                    "{} [label=\"fork<{:?}>\"];\n",
                    name,
                    module.dynamic_constants[factor.idx()]
                )?;
                let (control_name, visited) = write_node(i, control.idx(), module, visited, w)?;
                write!(
                    w,
                    "{} -> {} [label=\"control\", style=\"dashed\"];\n",
                    control_name, name
                )?;
                visited
            }
            Node::Join { control, data } => {
                write!(w, "{} [label=\"join\"];\n", name,)?;
                let (control_name, visited) = write_node(i, control.idx(), module, visited, w)?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(
                    w,
                    "{} -> {} [label=\"control\", style=\"dashed\"];\n",
                    control_name, name
                )?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                visited
            }
            Node::Phi { control, data } => {
                write!(w, "{} [label=\"phi\"];\n", name)?;
                let (control_name, mut visited) = write_node(i, control.idx(), module, visited, w)?;
                write!(
                    w,
                    "{} -> {} [label=\"control\", style=\"dashed\"];\n",
                    control_name, name
                )?;
                for (idx, data) in data.iter().enumerate() {
                    let (data_name, tmp_visited) = write_node(i, data.idx(), module, visited, w)?;
                    visited = tmp_visited;
                    write!(w, "{} -> {} [label=\"data {}\"];\n", data_name, name, idx)?;
                }
                visited
            }
            Node::Return { control, value } => {
                let (control_name, visited) = write_node(i, control.idx(), module, visited, w)?;
                let (value_name, visited) = write_node(i, value.idx(), module, visited, w)?;
                write!(w, "{} [label=\"return\"];\n", name)?;
                write!(
                    w,
                    "{} -> {} [label=\"control\", style=\"dashed\"];\n",
                    control_name, name
                )?;
                write!(w, "{} -> {} [label=\"value\"];\n", value_name, name)?;
                visited
            }
            Node::Parameter { index } => {
                write!(w, "{} [label=\"param #{}\"];\n", name, index + 1)?;
                visited
            }
            Node::Constant { id } => {
                write!(
                    w,
                    "{} [label=\"{:?}\"];\n",
                    name,
                    module.constants[id.idx()]
                )?;
                visited
            }
            Node::DynamicConstant { id } => {
                write!(
                    w,
                    "{} [label=\"dynamic_constant({:?})\"];\n",
                    name,
                    module.dynamic_constants[id.idx()]
                )?;
                visited
            }
            Node::Add { left, right } => {
                write!(w, "{} [label=\"add\"];\n", name)?;
                let (left_name, visited) = write_node(i, left.idx(), module, visited, w)?;
                let (right_name, visited) = write_node(i, right.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"left\"];\n", left_name, name)?;
                write!(w, "{} -> {} [label=\"right\"];\n", right_name, name)?;
                visited
            }
            Node::Sub { left, right } => {
                write!(w, "{} [label=\"sub\"];\n", name)?;
                let (left_name, visited) = write_node(i, left.idx(), module, visited, w)?;
                let (right_name, visited) = write_node(i, right.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"left\"];\n", left_name, name)?;
                write!(w, "{} -> {} [label=\"right\"];\n", right_name, name)?;
                visited
            }
            Node::Mul { left, right } => {
                write!(w, "{} [label=\"mul\"];\n", name)?;
                let (left_name, visited) = write_node(i, left.idx(), module, visited, w)?;
                let (right_name, visited) = write_node(i, right.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"left\"];\n", left_name, name)?;
                write!(w, "{} -> {} [label=\"right\"];\n", right_name, name)?;
                visited
            }
            Node::Div { left, right } => {
                write!(w, "{} [label=\"div\"];\n", name)?;
                let (left_name, visited) = write_node(i, left.idx(), module, visited, w)?;
                let (right_name, visited) = write_node(i, right.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"left\"];\n", left_name, name)?;
                write!(w, "{} -> {} [label=\"right\"];\n", right_name, name)?;
                visited
            }
            Node::LessThan { left, right } => {
                write!(w, "{} [label=\"less_than\"];\n", name)?;
                let (left_name, visited) = write_node(i, left.idx(), module, visited, w)?;
                let (right_name, visited) = write_node(i, right.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"left\"];\n", left_name, name)?;
                write!(w, "{} -> {} [label=\"right\"];\n", right_name, name)?;
                visited
            }
            Node::Call {
                function,
                dynamic_constants,
                args,
            } => {
                write!(w, "{} [label=\"call<", name,)?;
                for (idx, id) in dynamic_constants.iter().enumerate() {
                    let dc = &module.dynamic_constants[id.idx()];
                    if idx == 0 {
                        write!(w, "{:?}", dc)?;
                    } else {
                        write!(w, ", {:?}", dc)?;
                    }
                }
                write!(w, ">({})\"];\n", module.functions[function.idx()].name)?;
                for (idx, arg) in args.iter().enumerate() {
                    let (arg_name, tmp_visited) = write_node(i, arg.idx(), module, visited, w)?;
                    visited = tmp_visited;
                    write!(w, "{} -> {} [label=\"arg {}\"];\n", arg_name, name, idx)?;
                }
                write!(
                    w,
                    "{} -> start_{}_0 [label=\"call\", lhead={}];\n",
                    name,
                    function.idx(),
                    module.functions[function.idx()].name
                )?;
                visited
            }
            Node::ReadProd { prod, index } => {
                write!(w, "{} [label=\"read_prod({})\"];\n", name, index)?;
                let (prod_name, visited) = write_node(i, prod.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"prod\"];\n", prod_name, name)?;
                visited
            }
            Node::WriteProd { prod, data, index } => {
                write!(w, "{} [label=\"write_prod({})\"];\n", name, index)?;
                let (prod_name, visited) = write_node(i, prod.idx(), module, visited, w)?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"prod\"];\n", prod_name, name)?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                visited
            }
            Node::ReadArray { array, index } => {
                write!(w, "{} [label=\"read_array\"];\n", name)?;
                let (array_name, visited) = write_node(i, array.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"array\"];\n", array_name, name)?;
                let (index_name, visited) = write_node(i, index.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"index\"];\n", index_name, name)?;
                visited
            }
            Node::WriteArray { array, data, index } => {
                write!(w, "{} [label=\"write_array\"];\n", name)?;
                let (array_name, visited) = write_node(i, array.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"array\"];\n", array_name, name)?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                let (index_name, visited) = write_node(i, index.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"index\"];\n", index_name, name)?;
                visited
            }
            Node::Match { control, sum } => {
                write!(w, "{} [label=\"match\"];\n", name)?;
                let (control_name, visited) = write_node(i, control.idx(), module, visited, w)?;
                write!(
                    w,
                    "{} -> {} [label=\"control\", style=\"dashed\"];\n",
                    control_name, name
                )?;
                let (sum_name, visited) = write_node(i, sum.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"sum\"];\n", sum_name, name)?;
                visited
            }
            Node::BuildSum {
                data,
                sum_ty,
                variant,
            } => {
                write!(
                    w,
                    "{} [label=\"build_sum({:?}, {})\"];\n",
                    name,
                    module.types[sum_ty.idx()],
                    variant
                )?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                visited
            }
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
            data: _,
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
        Node::LessThan { left: _, right: _ } => "less_than",
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
