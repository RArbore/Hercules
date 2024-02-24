extern crate hercules_ir;

use std::cell::Ref;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::zip;

use self::hercules_ir::dataflow::*;
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
    pub fn new(module: Module, typing: ModuleTyping) -> Self {
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

    fn make_def_uses(&mut self) {
        if self.def_uses.is_none() {
            self.def_uses = Some(self.module.functions.iter().map(def_use).collect());
        }
    }

    fn get_def_uses(&mut self) -> &Vec<ImmutableDefUseMap> {
        self.make_def_uses();
        self.def_uses.as_ref().unwrap()
    }

    fn make_reverse_postorders(&mut self) {
        if self.reverse_postorders.is_none() {
            self.reverse_postorders =
                Some(self.get_def_uses().iter().map(reverse_postorder).collect());
        }
    }

    fn get_reverse_postorders(&mut self) -> &Vec<Vec<NodeID>> {
        self.make_reverse_postorders();
        self.reverse_postorders.as_ref().unwrap()
    }

    fn make_typing(&mut self) {
        self.make_reverse_postorders();
        let PassManager {
            module,
            reverse_postorders,
            ..
        } = self;
        if self.typing.is_none() {
            self.typing = Some(typecheck(module, reverse_postorders.as_ref().unwrap()).unwrap());
        }
    }

    fn get_typing(&mut self) -> &ModuleTyping {
        self.make_typing();
        self.typing.as_ref().unwrap()
    }

    fn make_control_subgraphs(&mut self) {
        self.make_def_uses();
        if self.control_subgraphs.is_none() {
            self.control_subgraphs = Some(
                zip(&self.module.functions, self.def_uses.as_ref().unwrap())
                    .map(|(function, def_use)| control_subgraph(function, def_use))
                    .collect(),
            );
        }
    }

    fn get_control_subgraphs(&mut self) -> &Vec<Subgraph> {
        self.make_control_subgraphs();
        self.control_subgraphs.as_ref().unwrap()
    }
}
