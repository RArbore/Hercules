extern crate hercules_ir;

use std::collections::HashMap;

use self::hercules_ir::*;

/*
 * Top level function to write a module out as a dot graph. Takes references to
 * many analysis results to generate a more informative dot graph.
 */
pub fn write_dot<W: std::fmt::Write>(
    module: &ir::Module,
    typing: &ModuleTyping,
    doms: &Vec<DomTree>,
    fork_join_maps: &Vec<HashMap<NodeID, NodeID>>,
    w: &mut W,
) -> std::fmt::Result {
    write_digraph_header(w)?;

    for function_id in (0..module.functions.len()).map(FunctionID::new) {
        let function = &module.functions[function_id.idx()];
        write_subgraph_header(function_id, module, w)?;

        // Step 1: draw IR graph itself. This includes all IR nodes and all edges
        // between IR nodes.
        for node_id in (0..function.nodes.len()).map(NodeID::new) {
            let node = &function.nodes[node_id.idx()];
            let dst_ty = &module.types[typing[function_id.idx()][node_id.idx()].idx()];
            let dst_strictly_control = node.is_strictly_control();
            let dst_control = dst_ty.is_control() || dst_strictly_control;

            // Control nodes are dark red, data nodes are dark blue.
            let color = if dst_control { "darkred" } else { "darkblue" };

            write_node(node_id, function_id, color, module, w)?;

            for u in def_use::get_uses(&node).as_ref() {
                let src_ty = &module.types[typing[function_id.idx()][u.idx()].idx()];
                let src_strictly_control = function.nodes[u.idx()].is_strictly_control();
                let src_control = src_ty.is_control() || src_strictly_control;

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

                write_edge(
                    node_id,
                    function_id,
                    *u,
                    function_id,
                    "black",
                    style,
                    module,
                    w,
                )?;
            }
        }

        // Step 2: draw dominance edges in dark green. Don't draw post dominance
        // edges because then xdot lays out the graph strangely.
        let dom = &doms[function_id.idx()];
        for (child_id, parent_id) in dom.get_underlying_map() {
            write_edge(
                *child_id,
                function_id,
                *parent_id,
                function_id,
                "darkgreen",
                "dotted",
                &module,
                w,
            )?;
        }

        // Step 3: draw fork join edges in dark magenta.
        let fork_join_map = &fork_join_maps[function_id.idx()];
        for (fork_id, join_id) in fork_join_map {
            write_edge(
                *join_id,
                function_id,
                *fork_id,
                function_id,
                "darkmagenta",
                "dotted",
                &module,
                w,
            )?;
        }

        write_graph_footer(w)?;
    }

    write_graph_footer(w)?;
    Ok(())
}

fn write_digraph_header<W: std::fmt::Write>(w: &mut W) -> std::fmt::Result {
    write!(w, "digraph \"Module\" {{\n")?;
    write!(w, "compound=true\n")?;
    Ok(())
}

fn write_subgraph_header<W: std::fmt::Write>(
    function_id: FunctionID,
    module: &Module,
    w: &mut W,
) -> std::fmt::Result {
    let function = &module.functions[function_id.idx()];
    write!(w, "subgraph {} {{\n", function.name)?;
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

fn write_graph_footer<W: std::fmt::Write>(w: &mut W) -> std::fmt::Result {
    write!(w, "}}\n")?;
    Ok(())
}

fn write_node<W: std::fmt::Write>(
    node_id: NodeID,
    function_id: FunctionID,
    color: &str,
    module: &Module,
    w: &mut W,
) -> std::fmt::Result {
    let node = &module.functions[function_id.idx()].nodes[node_id.idx()];
    write!(
        w,
        "{}_{}_{} [xlabel={}, label=\"{}\", color={}];\n",
        node.lower_case_name(),
        function_id.idx(),
        node_id.idx(),
        node_id.idx(),
        node.lower_case_name(),
        color
    )?;
    Ok(())
}

fn write_edge<W: std::fmt::Write>(
    dst_node_id: NodeID,
    dst_function_id: FunctionID,
    src_node_id: NodeID,
    src_function_id: FunctionID,
    color: &str,
    style: &str,
    module: &Module,
    w: &mut W,
) -> std::fmt::Result {
    let dst_node = &module.functions[dst_function_id.idx()].nodes[dst_node_id.idx()];
    let src_node = &module.functions[src_function_id.idx()].nodes[src_node_id.idx()];
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
    Ok(())
}
