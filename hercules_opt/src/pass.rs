extern crate hercules_ir;

use std::collections::HashMap;

use self::hercules_ir::def_use::*;
use self::hercules_ir::dom::*;
use self::hercules_ir::ir::*;
use self::hercules_ir::loops::*;
use self::hercules_ir::subgraph::*;
use self::hercules_ir::typecheck::*;

/*
 * Passes that can be run on a module.
 */
#[derive(Debug, Clone)]
pub enum Pass {
    DCE,
    CCP,
    GVN,
    Forkify,
}

/*
 * Manages passes to be run on an IR module. Transparently handles analysis
 * requirements for optimizations.
 */
#[derive(Debug, Clone)]
pub struct PassManager {
    module: Module,

    // Passes to run.
    passes: Vec<Pass>,

    // Cached analysis results.
    def_uses: Option<Vec<ImmutableDefUseMap>>,
    reverse_postorders: Option<Vec<Vec<NodeID>>>,
    typing: Option<ModuleTyping>,
    control_subgraphs: Option<Vec<Subgraph>>,
    doms: Option<Vec<DomTree>>,
    postdoms: Option<Vec<DomTree>>,
    fork_join_maps: Option<Vec<HashMap<NodeID, NodeID>>>,
    loops: Option<Vec<LoopTree>>,
}

impl PassManager {
    pub fn new(module: Module) -> Self {
        PassManager {
            module,
            passes: vec![],
            def_uses: None,
            reverse_postorders: None,
            typing: None,
            control_subgraphs: None,
            doms: None,
            postdoms: None,
            fork_join_maps: None,
            loops: None,
        }
    }

    pub fn add_pass(&mut self, pass: Pass) {
        self.passes.push(pass);
    }
}
