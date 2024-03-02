/* The data structure and algorithm described in
 *  Braun, M., Buchwald, S., Hack, S., Leißa, R., Mallon, C., Zwinkau, A. (2013). Simple and
 *  Efficient Construction of Static Single Assignment Form. In: Jhala, R., De Bosschere, K. (eds)
 *  Compiler Construction. CC 2013. Lecture Notes in Computer Science, vol 7791. Springer, Berlin,
 *  Heidelberg. https://doi.org/10.1007/978-3-642-37051-9_6
 */
extern crate hercules_ir;

use std::collections::{HashMap, HashSet};

use self::hercules_ir::ir::*;
use self::hercules_ir::build::*;

pub struct SSA {
    // Map from variable (usize) to build (NodeID) to definition (NodeID)
    current_def     : HashMap<usize, HashMap<NodeID, NodeID>>,
    sealed_blocks   : HashSet<NodeID>,
    incomplete_phis : HashMap<NodeID, HashMap<usize, NodeBuilder>>,

    function        : FunctionID,
    block_preds     : HashMap<NodeID, Vec<NodeID>>,
    unsealed_blocks : HashMap<NodeID, NodeBuilder>,
}

impl SSA {
    pub fn new(func : FunctionID, entry : NodeID) -> SSA {
        SSA { current_def     : HashMap::new(),
              sealed_blocks   : HashSet::from([entry]),
              incomplete_phis : HashMap::new(),
              function        : func,
              block_preds     : HashMap::from([(entry, vec![])]),
              unsealed_blocks : HashMap::new() }
    }

    pub fn create_cond<'a>(&mut self, builder : &mut Builder<'a>,
                           pred : NodeID) -> (NodeBuilder, NodeID, NodeID) {
        let if_builder = builder.allocate_node(self.function);
        let mut left_builder = builder.allocate_node(self.function);
        let mut right_builder = builder.allocate_node(self.function);

        let left_proj = left_builder.id();
        let right_proj = right_builder.id();

        let proj_left = builder.create_control_index(0);
        left_builder.build_read(if_builder.id(), vec![proj_left].into());

        let proj_right = builder.create_control_index(1);
        right_builder.build_read(if_builder.id(), vec![proj_right].into());

        let _ = builder.add_node(left_builder);
        let _ = builder.add_node(right_builder);

        self.sealed_blocks.insert(if_builder.id());
        self.block_preds.insert(if_builder.id(), vec![pred]);

        self.sealed_blocks.insert(left_proj);
        self.block_preds.insert(left_proj, vec![if_builder.id()]);

        self.sealed_blocks.insert(right_proj);
        self.block_preds.insert(right_proj, vec![if_builder.id()]);

        (if_builder, left_proj, right_proj)
    }

    pub fn create_block<'a>(&mut self, builder : &mut Builder<'a>) -> NodeID {
        let node_builder = builder.allocate_node(self.function);
        let block = node_builder.id();
        self.unsealed_blocks.insert(block, node_builder);
        self.block_preds.insert(block, vec![]);
        self.incomplete_phis.insert(block, HashMap::new());
        block
    }

    // Add "pred" as a predecessor of "block"
    pub fn add_pred(&mut self, block : NodeID, pred : NodeID) {
        assert!(self.unsealed_blocks.contains_key(&block),
                "Block must be unsealed to add predecessors");
        self.block_preds.get_mut(&block)
                        .expect("Block was created")
                        .push(pred);
    }

    pub fn seal_block<'a>(&mut self, block : NodeID, builder : &mut Builder<'a>) {
        let mut block_builder = self.unsealed_blocks.remove(&block)
                                    .expect("A block must be unsealed to seal it");

        let preds = self.block_preds.get(&block)
                                    .expect("A block must be created to seal it")
                                    .clone();
        let mut phis =
            match self.incomplete_phis.remove(&block) {
                None => HashMap::new(),
                Some(phis) => phis,
            };

        for (variable, phi) in phis.drain() {
            self.add_phi_operands(variable, block, phi, builder);
        }

        self.sealed_blocks.insert(block);
        block_builder.build_region(preds.into());
        let _ = builder.add_node(block_builder);
    }

    pub fn write_variable(&mut self, variable : usize, block : NodeID, value : NodeID) {
        match self.current_def.get_mut(&variable) {
            Some(m) => {
                m.insert(block, value);
            },
            None => {
                self.current_def.insert(variable, HashMap::from([(block, value)]));
            },
        }
    }

    pub fn read_variable<'a>(&mut self, variable : usize, block : NodeID,
                             builder : &mut Builder<'a>) -> NodeID {
        match self.current_def.get(&variable) {
            Some(var) => {
                match var.get(&block) {
                    Some(val) => *val,
                    None => self.read_variable_recursive(variable, block, builder),
                }
            },
            None => {
                panic!("ERROR: Variable in read_variable never written")
            },
        }
    }

    fn read_variable_recursive<'a>(&mut self, variable : usize, block : NodeID,
                                   builder : &mut Builder<'a>) -> NodeID {
        let val = 
            if !self.sealed_blocks.contains(&block) {
                let node = builder.allocate_node(self.function);
                let node_id = node.id();
                self.incomplete_phis.get_mut(&block)
                    .expect("Unsealed block has been added")
                    .insert(variable, node);
                node_id
            } else if self.block_preds.get(&block)
                                      .expect("Sealed block has preds").len() == 1 {
                self.read_variable(variable,
                                   self.block_preds.get(&block)
                                       .expect("Sealed block has preds")[0],
                                   builder)
            } else {
                let node = builder.allocate_node(self.function);
                let node_id = node.id();
                self.write_variable(variable, block, node_id);
                self.add_phi_operands(variable, block, node, builder);
                node_id
            };

        self.write_variable(variable, block, val);
        val
    }

    fn add_phi_operands<'a>(&mut self, variable : usize, block : NodeID,
                            mut phi : NodeBuilder, builder : &mut Builder<'a>) {
        let mut vals = vec![];
        let preds = self.block_preds.get(&block).expect("Block exists").clone();
        for pred in preds {
            vals.push(self.read_variable(variable, pred, builder));
        }
        phi.build_phi(block, vals.into());
        let _ = builder.add_node(phi);
    }
}
