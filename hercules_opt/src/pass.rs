extern crate hercules_ir;
extern crate take_mut;

use std::collections::HashMap;
use std::iter::zip;

use self::hercules_ir::antideps::*;
use self::hercules_ir::dataflow::*;
use self::hercules_ir::def_use::*;
use self::hercules_ir::dom::*;
use self::hercules_ir::dot::*;
use self::hercules_ir::gcm::*;
use self::hercules_ir::ir::*;
use self::hercules_ir::loops::*;
use self::hercules_ir::schedule::*;
use self::hercules_ir::subgraph::*;
use self::hercules_ir::typecheck::*;
use self::hercules_ir::verify::*;

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
    Predication,
    Verify,
    Xdot(bool),
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
    pub def_uses: Option<Vec<ImmutableDefUseMap>>,
    pub reverse_postorders: Option<Vec<Vec<NodeID>>>,
    pub typing: Option<ModuleTyping>,
    pub control_subgraphs: Option<Vec<Subgraph>>,
    pub doms: Option<Vec<DomTree>>,
    pub postdoms: Option<Vec<DomTree>>,
    pub fork_join_maps: Option<Vec<HashMap<NodeID, NodeID>>>,
    pub loops: Option<Vec<LoopTree>>,
    pub antideps: Option<Vec<Vec<(NodeID, NodeID)>>>,
    pub bbs: Option<Vec<Vec<NodeID>>>,

    // Current plan. Keep track of the last time the plan was updated.
    pub plans: Option<Vec<Plan>>,
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
            antideps: None,
            bbs: None,
            plans: None,
        }
    }

    pub fn add_pass(&mut self, pass: Pass) {
        self.passes.push(pass);
    }

    pub fn make_def_uses(&mut self) {
        if self.def_uses.is_none() {
            self.def_uses = Some(self.module.functions.iter().map(def_use).collect());
        }
    }

    pub fn make_reverse_postorders(&mut self) {
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

    pub fn make_typing(&mut self) {
        if self.typing.is_none() {
            self.make_reverse_postorders();
            self.typing = Some(
                typecheck(&mut self.module, self.reverse_postorders.as_ref().unwrap()).unwrap(),
            );
        }
    }

    pub fn make_control_subgraphs(&mut self) {
        if self.control_subgraphs.is_none() {
            self.make_def_uses();
            self.control_subgraphs = Some(
                zip(&self.module.functions, self.def_uses.as_ref().unwrap())
                    .map(|(function, def_use)| control_subgraph(function, def_use))
                    .collect(),
            );
        }
    }

    pub fn make_doms(&mut self) {
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

    pub fn make_postdoms(&mut self) {
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

    pub fn make_fork_join_maps(&mut self) {
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

    pub fn make_loops(&mut self) {
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

    pub fn make_antideps(&mut self) {
        if self.antideps.is_none() {
            self.make_def_uses();
            self.antideps = Some(
                zip(
                    self.def_uses.as_ref().unwrap().iter(),
                    self.module.functions.iter(),
                )
                .map(|(def_use, function)| antideps(function, def_use))
                .collect(),
            );
        }
    }

    pub fn make_bbs(&mut self) {
        if self.antideps.is_none() {
            self.make_def_uses();
            self.make_reverse_postorders();
            self.make_doms();
            self.make_antideps();
            self.make_loops();
            let def_uses = self.def_uses.as_ref().unwrap().iter();
            let reverse_postorders = self.reverse_postorders.as_ref().unwrap().iter();
            let doms = self.doms.as_ref().unwrap().iter();
            let antideps = self.antideps.as_ref().unwrap().iter();
            let loops = self.loops.as_ref().unwrap().iter();
            self.bbs = Some(
                zip(
                    self.module.functions.iter(),
                    zip(
                        def_uses,
                        zip(reverse_postorders, zip(doms, zip(antideps, loops))),
                    ),
                )
                .map(
                    |(function, (def_use, (reverse_postorder, (dom, (antideps, loops)))))| {
                        gcm(function, def_use, reverse_postorder, dom, antideps, loops)
                    },
                )
                .collect(),
            );
        }
    }

    pub fn make_plans(&mut self) {
        if self.plans.is_none() {
            self.make_reverse_postorders();
            self.make_fork_join_maps();
            self.make_bbs();
            let reverse_postorders = self.reverse_postorders.as_ref().unwrap().iter();
            let fork_join_maps = self.fork_join_maps.as_ref().unwrap().iter();
            let bbs = self.bbs.as_ref().unwrap().iter();
            self.plans = Some(
                zip(
                    self.module.functions.iter(),
                    zip(reverse_postorders, zip(fork_join_maps, bbs)),
                )
                .map(|(function, (reverse_postorder, (fork_join_map, bb)))| {
                    default_plan(function, reverse_postorder, fork_join_map, bb)
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
                    }
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
                    }
                }
                Pass::Forkify => {
                    self.make_def_uses();
                    self.make_loops();
                    let def_uses = self.def_uses.as_ref().unwrap();
                    let loops = self.loops.as_ref().unwrap();
                    for idx in 0..self.module.functions.len() {
                        forkify(
                            &mut self.module.functions[idx],
                            &self.module.constants,
                            &mut self.module.dynamic_constants,
                            &def_uses[idx],
                            &loops[idx],
                        )
                    }
                }
                Pass::Predication => {
                    self.make_def_uses();
                    self.make_reverse_postorders();
                    self.make_doms();
                    self.make_fork_join_maps();
                    self.make_plans();
                    let def_uses = self.def_uses.as_ref().unwrap();
                    let reverse_postorders = self.reverse_postorders.as_ref().unwrap();
                    let doms = self.doms.as_ref().unwrap();
                    let fork_join_maps = self.fork_join_maps.as_ref().unwrap();
                    let plans = self.plans.as_ref().unwrap();
                    for idx in 0..self.module.functions.len() {
                        predication(
                            &mut self.module.functions[idx],
                            &def_uses[idx],
                            &reverse_postorders[idx],
                            &doms[idx],
                            &fork_join_maps[idx],
                            &plans[idx].schedules,
                        )
                    }
                }
                Pass::Verify => {
                    let (
                        def_uses,
                        reverse_postorders,
                        typing,
                        subgraphs,
                        doms,
                        postdoms,
                        fork_join_maps,
                    ) = verify(&mut self.module)
                        .expect("PANIC: Failed to verify Hercules IR module.");

                    // Verification produces a bunch of analysis results that
                    // may be useful for later passes.
                    self.def_uses = Some(def_uses);
                    self.reverse_postorders = Some(reverse_postorders);
                    self.typing = Some(typing);
                    self.control_subgraphs = Some(subgraphs);
                    self.doms = Some(doms);
                    self.postdoms = Some(postdoms);
                    self.fork_join_maps = Some(fork_join_maps);

                    // Verify doesn't require clearing analysis results.
                    continue;
                }
                Pass::Xdot(force_analyses) => {
                    self.make_reverse_postorders();
                    if *force_analyses {
                        self.make_doms();
                        self.make_fork_join_maps();
                        self.make_plans();
                    }
                    xdot_module(
                        &self.module,
                        self.reverse_postorders.as_ref().unwrap(),
                        self.doms.as_ref(),
                        self.fork_join_maps.as_ref(),
                        self.plans.as_ref(),
                    );

                    // Xdot doesn't require clearing analysis results.
                    continue;
                }
            }

            // Cleanup the module after passes. Delete gravestone nodes. Repair
            // the plans. Clear out-of-date analyses.
            for idx in 0..self.module.functions.len() {
                let grave_mapping = self.module.functions[idx].delete_gravestones();
                let plans = &mut self.plans;
                let functions = &self.module.functions;
                if let Some(plans) = plans.as_mut() {
                    take_mut::take(&mut plans[idx], |plan| {
                        plan.repair(&functions[idx], &grave_mapping)
                    });
                }
            }
            self.clear_analyses();
        }

        self.module
    }

    fn clear_analyses(&mut self) {
        self.def_uses = None;
        self.reverse_postorders = None;
        self.typing = None;
        self.control_subgraphs = None;
        self.doms = None;
        self.postdoms = None;
        self.fork_join_maps = None;
        self.loops = None;
        self.antideps = None;
        self.bbs = None;
    }
}
