extern crate hercules_ir;

use std::collections::HashMap;
use std::iter::zip;

use self::hercules_ir::dataflow::*;
use self::hercules_ir::def_use::*;
use self::hercules_ir::dom::*;
use self::hercules_ir::ir::*;
use self::hercules_ir::loops::*;
use self::hercules_ir::subgraph::*;
use self::hercules_ir::typecheck::*;

use crate::*;

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

    fn make_def_uses(&mut self) {
        if self.def_uses.is_none() {
            self.def_uses = Some(self.module.functions.iter().map(def_use).collect());
        }
    }

    fn make_reverse_postorders(&mut self) {
        if self.reverse_postorders.is_none() {
            self.make_def_uses();
            self.reverse_postorders = Some(
                self.def_uses
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(reverse_postorder)
                    .collect(),
            );
        }
    }

    fn make_typing(&mut self) {
        if self.typing.is_none() {
            self.make_reverse_postorders();
            self.typing = Some(
                typecheck(&mut self.module, self.reverse_postorders.as_ref().unwrap()).unwrap(),
            );
        }
    }

    fn make_control_subgraphs(&mut self) {
        if self.control_subgraphs.is_none() {
            self.make_def_uses();
            self.control_subgraphs = Some(
                zip(&self.module.functions, self.def_uses.as_ref().unwrap())
                    .map(|(function, def_use)| control_subgraph(function, def_use))
                    .collect(),
            );
        }
    }

    fn make_doms(&mut self) {
        if self.doms.is_none() {
            self.make_control_subgraphs();
            self.doms = Some(
                self.control_subgraphs
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|subgraph| dominator(subgraph, NodeID::new(0)))
                    .collect(),
            );
        }
    }

    fn make_postdoms(&mut self) {
        if self.postdoms.is_none() {
            self.make_control_subgraphs();
            self.postdoms = Some(
                zip(
                    self.control_subgraphs.as_ref().unwrap().iter(),
                    self.module.functions.iter(),
                )
                .map(|(subgraph, function)| dominator(subgraph, NodeID::new(function.nodes.len())))
                .collect(),
            );
        }
    }

    fn make_fork_join_maps(&mut self) {
        if self.fork_join_maps.is_none() {
            self.make_typing();
            self.fork_join_maps = Some(
                zip(
                    self.module.functions.iter(),
                    self.typing.as_ref().unwrap().iter(),
                )
                .map(|(function, typing)| fork_join_map(function, typing, &self.module.types))
                .collect(),
            );
        }
    }

    fn make_loops(&mut self) {
        if self.loops.is_none() {
            self.make_control_subgraphs();
            self.make_doms();
            self.make_fork_join_maps();
            let control_subgraphs = self.control_subgraphs.as_ref().unwrap().iter();
            let doms = self.doms.as_ref().unwrap().iter();
            let fork_join_maps = self.fork_join_maps.as_ref().unwrap().iter();
            self.loops = Some(
                zip(control_subgraphs, zip(doms, fork_join_maps))
                    .map(|(control_subgraph, (dom, fork_join_map))| {
                        loops(control_subgraph, NodeID::new(0), dom, fork_join_map)
                    })
                    .collect(),
            );
        }
    }

    pub fn run_passes(mut self) -> Module {
        for pass in self.passes.clone().iter() {
            match pass {
                Pass::DCE => {
                    for idx in 0..self.module.functions.len() {
                        dce(&mut self.module.functions[idx]);
                        self.module.functions[idx].delete_gravestones();
                    }
                }
                Pass::CCP => {
                    self.make_def_uses();
                    self.make_reverse_postorders();
                    let def_uses = self.def_uses.as_ref().unwrap();
                    let reverse_postorders = self.reverse_postorders.as_ref().unwrap();
                    for idx in 0..self.module.functions.len() {
                        ccp(
                            &mut self.module.functions[idx],
                            &self.module.types,
                            &mut self.module.constants,
                            &def_uses[idx],
                            &reverse_postorders[idx],
                        );
                        self.module.functions[idx].delete_gravestones();
                    }
                    self.def_uses = None;
                    self.reverse_postorders = None;
                }
                Pass::GVN => {
                    self.make_def_uses();
                    let def_uses = self.def_uses.as_ref().unwrap();
                    for idx in 0..self.module.functions.len() {
                        gvn(
                            &mut self.module.functions[idx],
                            &self.module.constants,
                            &def_uses[idx],
                        );
                        self.module.functions[idx].delete_gravestones();
                    }
                    self.def_uses = None;
                }
                Pass::Forkify => {}
            }
        }
        self.module
    }
}
