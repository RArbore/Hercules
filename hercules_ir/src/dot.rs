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
        let name = format!("{}_{}_{}", node.lower_case_name(), i, j);
        visited.insert(NodeID::new(j), name.clone());
        let visited = match node {
            Node::Start => {
                write!(w, "{} [xlabel={}, label=\"start\"];\n", name, j)?;
                visited
            }
            Node::Region { preds } => {
                write!(w, "{} [xlabel={}, label=\"region\"];\n", name, j)?;
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
                write!(w, "{} [xlabel={}, label=\"if\"];\n", name, j)?;
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
                    "{} [xlabel={}, label=\"fork<{:?}>\"];\n",
                    name,
                    j,
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
                write!(w, "{} [xlabel={}, label=\"join\"];\n", name, j)?;
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
                write!(w, "{} [xlabel={}, label=\"phi\"];\n", name, j)?;
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
                write!(w, "{} [xlabel={}, label=\"return\"];\n", name, j)?;
                write!(
                    w,
                    "{} -> {} [label=\"control\", style=\"dashed\"];\n",
                    control_name, name
                )?;
                write!(w, "{} -> {} [label=\"value\"];\n", value_name, name)?;
                visited
            }
            Node::Parameter { index } => {
                write!(
                    w,
                    "{} [xlabel={}, label=\"param #{}\"];\n",
                    name,
                    j,
                    index + 1
                )?;
                visited
            }
            Node::Constant { id } => {
                write!(
                    w,
                    "{} [xlabel={}, label=\"{:?}\"];\n",
                    name,
                    j,
                    module.constants[id.idx()]
                )?;
                visited
            }
            Node::DynamicConstant { id } => {
                write!(
                    w,
                    "{} [xlabel={}, label=\"dynamic_constant({:?})\"];\n",
                    name,
                    j,
                    module.dynamic_constants[id.idx()]
                )?;
                visited
            }
            Node::Unary { input, op } => {
                write!(
                    w,
                    "{} [xlabel={}, label=\"{}\"];\n",
                    name,
                    j,
                    op.lower_case_name()
                )?;
                let (input_name, visited) = write_node(i, input.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"input\"];\n", input_name, name)?;
                visited
            }
            Node::Binary { left, right, op } => {
                write!(
                    w,
                    "{} [xlabel={}, label=\"{}\"];\n",
                    name,
                    j,
                    op.lower_case_name()
                )?;
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
                write!(w, "{} [xlabel={}, label=\"call<", name, j)?;
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
                write!(
                    w,
                    "{} [xlabel={}, label=\"read_prod({})\"];\n",
                    name, j, index
                )?;
                let (prod_name, visited) = write_node(i, prod.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"prod\"];\n", prod_name, name)?;
                visited
            }
            Node::WriteProd { prod, data, index } => {
                write!(
                    w,
                    "{} [xlabel={}, label=\"write_prod({})\"];\n",
                    name, j, index
                )?;
                let (prod_name, visited) = write_node(i, prod.idx(), module, visited, w)?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"prod\"];\n", prod_name, name)?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                visited
            }
            Node::ReadArray { array, index } => {
                write!(w, "{} [xlabel={}, label=\"read_array\"];\n", name, j)?;
                let (array_name, visited) = write_node(i, array.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"array\"];\n", array_name, name)?;
                let (index_name, visited) = write_node(i, index.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"index\"];\n", index_name, name)?;
                visited
            }
            Node::WriteArray { array, data, index } => {
                write!(w, "{} [xlabel={}, label=\"write_array\"];\n", name, j)?;
                let (array_name, visited) = write_node(i, array.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"array\"];\n", array_name, name)?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                let (index_name, visited) = write_node(i, index.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"index\"];\n", index_name, name)?;
                visited
            }
            Node::Match { control, sum } => {
                write!(w, "{} [xlabel={}, label=\"match\"];\n", name, j)?;
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
                    "{} [xlabel={}, label=\"build_sum({:?}, {})\"];\n",
                    name,
                    j,
                    module.types[sum_ty.idx()],
                    variant
                )?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                visited
            }
            Node::ExtractSum { data, variant } => {
                write!(
                    w,
                    "{} [xlabel={}, label=\"extract_sum({})\"];\n",
                    name, j, variant
                )?;
                let (data_name, visited) = write_node(i, data.idx(), module, visited, w)?;
                write!(w, "{} -> {} [label=\"data\"];\n", data_name, name)?;
                visited
            }
        };
        Ok((visited.get(&id).unwrap().clone(), visited))
    }
}
