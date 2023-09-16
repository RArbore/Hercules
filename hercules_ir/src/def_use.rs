use crate::*;

/*
 * Custom type for an immutable def_use map. This is a relatively efficient
 * storage of def_use edges, requiring 2 heap allocations.
 */
#[derive(Debug, Clone)]
pub struct ImmutableDefUseMap {
    first_edges: Vec<u32>,
    users: Vec<NodeID>,
}

impl ImmutableDefUseMap {
    pub fn num_edges(&self, id: NodeID) -> u32 {
        if id.idx() + 1 < self.first_edges.len() {
            self.first_edges[id.idx() + 1] - self.first_edges[id.idx()]
        } else {
            self.first_edges.len() as u32 - self.first_edges[id.idx()]
        }
    }

    pub fn get_uses(&self, id: NodeID) -> &[NodeID] {
        let first_edge = self.first_edges[id.idx()] as usize;
        let num_edges = self.num_edges(id) as usize;
        &self.users[first_edge..first_edge + num_edges]
    }

    pub fn num_nodes(&self) -> usize {
        self.first_edges.len()
    }
}

/*
 * Top level def_use function.
 */
pub fn def_use(function: &Function) -> ImmutableDefUseMap {
    // Step 1: get uses for each node.
    let node_uses: Vec<NodeUses> = function.nodes.iter().map(|node| get_uses(node)).collect();

    // Step 2: count number of users per node.
    let mut num_users: Vec<u32> = vec![0; node_uses.len()];
    for uses in node_uses.iter() {
        for u in uses.as_ref() {
            num_users[u.idx()] += 1;
        }
    }

    // Step 3: assemble first_edges vector.
    let mut first_edges: Vec<u32> = vec![];
    let mut num_edges = 0;
    for num_users in num_users {
        first_edges.push(num_edges);
        num_edges += num_users;
    }

    // Step 4: assemble users vector.
    let mut users: Vec<NodeID> = vec![NodeID::new(0); num_edges as usize];
    let mut num_users_per_node: Vec<u32> = vec![0; node_uses.len()];
    for (idx, uses) in node_uses.iter().enumerate() {
        for u in uses.as_ref() {
            let first_edge = first_edges[u.idx()];
            let num_users_so_far = num_users_per_node[u.idx()];
            users[first_edge as usize + num_users_so_far as usize] = NodeID::new(idx);
            num_users_per_node[u.idx()] = num_users_so_far + 1;
        }
    }

    // Step 5: pack and return.
    ImmutableDefUseMap { first_edges, users }
}

/*
 * Enum for storing uses of node. Using get_uses, one can easily iterate over
 * the defs that a node uses.
 */
#[derive(Debug, Clone)]
pub enum NodeUses<'a> {
    Zero,
    One([NodeID; 1]),
    Two([NodeID; 2]),
    Three([NodeID; 3]),
    Variable(&'a Box<[NodeID]>),
    // Phi nodes are special, and store both a NodeID locally *and* many in a
    // boxed slice. Since these NodeIDs are not stored contiguously, we have to
    // construct a new contiguous slice by copying. Sigh.
    Phi(Box<[NodeID]>),
}

impl<'a> AsRef<[NodeID]> for NodeUses<'a> {
    fn as_ref(&self) -> &[NodeID] {
        match self {
            NodeUses::Zero => &[],
            NodeUses::One(x) => x,
            NodeUses::Two(x) => x,
            NodeUses::Three(x) => x,
            NodeUses::Variable(x) => x,
            NodeUses::Phi(x) => x,
        }
    }
}

/*
 * Construct NodeUses for a Node.
 */
pub fn get_uses<'a>(node: &'a Node) -> NodeUses<'a> {
    match node {
        Node::Start => NodeUses::Zero,
        Node::Region { preds } => NodeUses::Variable(preds),
        Node::If { control, cond } => NodeUses::Two([*control, *cond]),
        Node::Fork { control, factor: _ } => NodeUses::One([*control]),
        Node::Join { control, data } => NodeUses::Two([*control, *data]),
        Node::Phi { control, data } => {
            let mut uses: Vec<NodeID> = Vec::from(&data[..]);
            uses.push(*control);
            NodeUses::Phi(uses.into_boxed_slice())
        }
        Node::Return { control, value } => NodeUses::Two([*control, *value]),
        Node::Parameter { index: _ } => todo!(),
        Node::Constant { id: _ } => todo!(),
        Node::DynamicConstant { id: _ } => todo!(),
        Node::Unary { input, op: _ } => NodeUses::One([*input]),
        Node::Binary { left, right, op: _ } => NodeUses::Two([*left, *right]),
        Node::Call {
            function: _,
            dynamic_constants: _,
            args,
        } => NodeUses::Variable(args),
        Node::ReadProd { prod, index: _ } => NodeUses::One([*prod]),
        Node::WriteProd {
            prod,
            data,
            index: _,
        } => NodeUses::Two([*prod, *data]),
        Node::ReadArray { array, index } => NodeUses::Two([*array, *index]),
        Node::WriteArray { array, data, index } => NodeUses::Three([*array, *data, *index]),
        Node::Match { control, sum } => NodeUses::Two([*control, *sum]),
        Node::BuildSum {
            data,
            sum_ty: _,
            variant: _,
        } => NodeUses::One([*data]),
    }
}
