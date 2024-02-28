extern crate rand;

use std::collections::HashMap;
use std::env::temp_dir;
use std::fmt::Write;
use std::fs::File;
use std::io::Write as _;
use std::process::Command;

use self::rand::Rng;

use crate::*;

/*
 * Top level function to compute a dot graph for a module, and immediately
 * render it using xdot.
 */
pub fn xdot_module(
    module: &ir::Module,
    reverse_postorders: &Vec<Vec<NodeID>>,
    doms: Option<&Vec<DomTree>>,
    fork_join_maps: Option<&Vec<HashMap<NodeID, NodeID>>>,
    plans: Option<&Vec<Plan>>,
) {
    let mut tmp_path = temp_dir();
    let mut rng = rand::thread_rng();
    let num: u64 = rng.gen();
    tmp_path.push(format!("hercules_dot_{}.dot", num));
    let mut file = File::create(tmp_path.clone()).expect("PANIC: Unable to open output file.");
    let mut contents = String::new();
    write_dot(
        &module,
        &reverse_postorders,
        doms,
        fork_join_maps,
        plans,
        &mut contents,
    )
    .expect("PANIC: Unable to generate output file contents.");
    file.write_all(contents.as_bytes())
        .expect("PANIC: Unable to write output file contents.");
    Command::new("xdot")
        .args([tmp_path])
        .output()
        .expect("PANIC: Couldn't execute xdot.");
}

/*
 * Top level function to write a module out as a dot graph. Optionally takes
 * references to many analysis results to generate a more informative dot graph.
 */
pub fn write_dot<W: Write>(
    module: &ir::Module,
    reverse_postorders: &Vec<Vec<NodeID>>,
    doms: Option<&Vec<DomTree>>,
    fork_join_maps: Option<&Vec<HashMap<NodeID, NodeID>>>,
    plans: Option<&Vec<Plan>>,
    w: &mut W,
) -> std::fmt::Result {
    write_digraph_header(w)?;

    for function_id in (0..module.functions.len()).map(FunctionID::new) {
        let function = &module.functions[function_id.idx()];
        let reverse_postorder = &reverse_postorders[function_id.idx()];
        let plan = plans.map(|plans| &plans[function_id.idx()]);
        let mut reverse_postorder_node_numbers = vec![0; function.nodes.len()];
        for (idx, id) in reverse_postorder.iter().enumerate() {
            reverse_postorder_node_numbers[id.idx()] = idx;
        }
        write_subgraph_header(function_id, module, w)?;

        let mut partition_to_node_map = plan.map(|plan| plan.invert_partition_map());

        // Step 1: draw IR graph itself. This includes all IR nodes and all edges
        // between IR nodes.
        for partition_idx in 0..plan.map_or(1, |plan| plan.num_partitions) {
            let partition_color = plan.map(|plan| match plan.partition_devices[partition_idx] {
                Device::CPU => "lightblue",
                Device::GPU => "darkseagreen",
            });
            if let Some(partition_color) = partition_color {
                write_partition_header(function_id, partition_idx, module, partition_color, w)?;
            }

            let nodes_ids = if let Some(partition_to_node_map) = &mut partition_to_node_map {
                let mut empty = vec![];
                std::mem::swap(&mut partition_to_node_map[partition_idx], &mut empty);
                empty
            } else {
                (0..function.nodes.len())
                    .map(NodeID::new)
                    .collect::<Vec<_>>()
            };
            for node_id in nodes_ids.iter() {
                let node = &function.nodes[node_id.idx()];
                let dst_control = node.is_control();

                // Control nodes are dark red, data nodes are dark blue.
                let color = if dst_control { "darkred" } else { "darkblue" };

                write_node(
                    *node_id,
                    function_id,
                    color,
                    module,
                    plan.map_or(&vec![], |plan| &plan.schedules[node_id.idx()]),
                    w,
                )?;

                for u in def_use::get_uses(&node).as_ref() {
                    let src_control = function.nodes[u.idx()].is_control();

                    // An edge between control nodes is dashed. An edge between data
                    // nodes is filled. An edge between a control node and a data
                    // node is dotted.
                    let style = if dst_control && src_control {
                        "dashed"
                    } else if !dst_control && !src_control {
                        ""
                    } else {
                        "dotted"
                    };

                    // To have a consistent layout, we will add "back edges" in the
                    // IR graph as backward facing edges in the graphviz output, so
                    // that they don't mess up the layout. There isn't necessarily a
                    // precise definition of a "back edge" in Hercules IR. I've
                    // found what makes for the most clear output graphs is treating
                    // edges to phi nodes as back edges when the phi node appears
                    // before the use in the reverse postorder, and treating a
                    // control edge a back edge when the destination appears before
                    // the source in the reverse postorder.
                    let is_back_edge = reverse_postorder_node_numbers[node_id.idx()]
                        < reverse_postorder_node_numbers[u.idx()]
                        && (node.is_phi()
                            || (function.nodes[node_id.idx()].is_control()
                                && function.nodes[u.idx()].is_control()));
                    write_edge(
                        *node_id,
                        function_id,
                        *u,
                        function_id,
                        !is_back_edge,
                        "black",
                        style,
                        module,
                        w,
                    )?;
                }
            }
            write_graph_footer(w)?;
        }

        // Step 2: draw dominance edges in dark green. Don't draw post dominance
        // edges because then xdot lays out the graph strangely.
        if let Some(doms) = doms {
            let dom = &doms[function_id.idx()];
            for (child_id, (_, parent_id)) in dom.get_underlying_map() {
                write_edge(
                    *child_id,
                    function_id,
                    *parent_id,
                    function_id,
                    true,
                    "darkgreen",
                    "dotted",
                    &module,
                    w,
                )?;
            }
        }

        // Step 3: draw fork join edges in dark magenta.
        if let Some(fork_join_maps) = fork_join_maps {
            let fork_join_map = &fork_join_maps[function_id.idx()];
            for (fork_id, join_id) in fork_join_map {
                write_edge(
                    *join_id,
                    function_id,
                    *fork_id,
                    function_id,
                    true,
                    "darkmagenta",
                    "dotted",
                    &module,
                    w,
                )?;
            }
        }

        write_graph_footer(w)?;
    }

    write_graph_footer(w)?;
    Ok(())
}

fn write_digraph_header<W: Write>(w: &mut W) -> std::fmt::Result {
    write!(w, "digraph \"Module\" {{\n")?;
    write!(w, "compound=true\n")?;
    Ok(())
}

fn write_subgraph_header<W: Write>(
    function_id: FunctionID,
    module: &Module,
    w: &mut W,
) -> std::fmt::Result {
    let function = &module.functions[function_id.idx()];
    write!(w, "subgraph {} {{\n", function.name)?;

    // Write number of dynamic constants in brackets.
    if function.num_dynamic_constants > 0 {
        write!(
            w,
            "label=\"{}<{}>\"\n",
            function.name, function.num_dynamic_constants
        )?;
    } else {
        write!(w, "label=\"{}\"\n", function.name)?;
    }
    write!(w, "bgcolor=ivory4\n")?;
    write!(w, "cluster=true\n")?;
    Ok(())
}

fn write_partition_header<W: Write>(
    function_id: FunctionID,
    partition_idx: usize,
    module: &Module,
    color: &str,
    w: &mut W,
) -> std::fmt::Result {
    let function = &module.functions[function_id.idx()];
    write!(w, "subgraph {}_{} {{\n", function.name, partition_idx)?;
    write!(w, "label=\"\"\n")?;
    write!(w, "style=rounded\n")?;
    write!(w, "bgcolor={}\n", color)?;
    write!(w, "cluster=true\n")?;
    Ok(())
}

fn write_graph_footer<W: Write>(w: &mut W) -> std::fmt::Result {
    write!(w, "}}\n")?;
    Ok(())
}

fn write_node<W: Write>(
    node_id: NodeID,
    function_id: FunctionID,
    color: &str,
    module: &Module,
    schedules: &Vec<Schedule>,
    w: &mut W,
) -> std::fmt::Result {
    let node = &module.functions[function_id.idx()].nodes[node_id.idx()];

    // Some nodes have additional information that need to get written after the
    // node label.
    let mut suffix = String::new();
    match node {
        Node::Fork { control: _, factor } => module.write_dynamic_constant(*factor, &mut suffix)?,
        Node::Parameter { index } => write!(&mut suffix, "#{}", index)?,
        Node::Constant { id } => module.write_constant(*id, &mut suffix)?,
        Node::DynamicConstant { id } => module.write_dynamic_constant(*id, &mut suffix)?,
        Node::Call {
            function,
            dynamic_constants,
            args: _,
        } => {
            write!(&mut suffix, "{}", module.functions[function.idx()].name)?;
            for dc_id in dynamic_constants.iter() {
                write!(&mut suffix, ", ")?;
                module.write_dynamic_constant(*dc_id, &mut suffix)?;
            }
        }
        Node::Read {
            collect: _,
            indices,
        } => {
            let mut iter = indices.iter();
            if let Some(first) = iter.next() {
                write!(&mut suffix, "{}", first.lower_case_name())?;
                for next in iter {
                    write!(&mut suffix, ", {}", next.lower_case_name())?;
                }
            }
        }
        Node::Write {
            collect: _,
            data: _,
            indices,
        } => {
            let mut iter = indices.iter();
            if let Some(first) = iter.next() {
                write!(&mut suffix, "{}", first.lower_case_name())?;
                for next in iter {
                    write!(&mut suffix, ", {}", next.lower_case_name())?;
                }
            }
        }
        _ => {}
    };

    // If this is a node with additional information, add that to the node
    // label.
    let label = if suffix.is_empty() {
        node.lower_case_name().to_owned()
    } else {
        format!("{} ({})", node.lower_case_name(), suffix)
    };

    let mut iter = schedules.into_iter();
    if let Some(first) = iter.next() {
        let subtitle = iter.fold(format!("{:?}", first), |b, i| format!("{}, {:?}", b, i));
        write!(
            w,
            "{}_{}_{} [xlabel={}, label=<{}<BR /><FONT POINT-SIZE=\"8\">{}</FONT>>, color={}];\n",
            node.lower_case_name(),
            function_id.idx(),
            node_id.idx(),
            node_id.idx(),
            label,
            subtitle,
            color
        )?;
    } else {
        write!(
            w,
            "{}_{}_{} [xlabel={}, label=\"{}\", color={}];\n",
            node.lower_case_name(),
            function_id.idx(),
            node_id.idx(),
            node_id.idx(),
            label,
            color
        )?;
    }
    Ok(())
}

fn write_edge<W: Write>(
    dst_node_id: NodeID,
    dst_function_id: FunctionID,
    src_node_id: NodeID,
    src_function_id: FunctionID,
    forward: bool,
    color: &str,
    style: &str,
    module: &Module,
    w: &mut W,
) -> std::fmt::Result {
    let dst_node = &module.functions[dst_function_id.idx()].nodes[dst_node_id.idx()];
    let src_node = &module.functions[src_function_id.idx()].nodes[src_node_id.idx()];
    if forward {
        write!(
            w,
            "{}_{}_{} -> {}_{}_{} [color={}, style=\"{}\"];\n",
            src_node.lower_case_name(),
            src_function_id.idx(),
            src_node_id.idx(),
            dst_node.lower_case_name(),
            dst_function_id.idx(),
            dst_node_id.idx(),
            color,
            style,
        )?;
    } else {
        write!(
            w,
            "{}_{}_{} -> {}_{}_{} [dir=back, color={}, style=\"{}\"];\n",
            dst_node.lower_case_name(),
            dst_function_id.idx(),
            dst_node_id.idx(),
            src_node.lower_case_name(),
            src_function_id.idx(),
            src_node_id.idx(),
            color,
            style,
        )?;
    }
    Ok(())
}
