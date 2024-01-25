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
            self.users.len() as u32 - self.first_edges[id.idx()]
        }
    }

    pub fn get_users(&self, id: NodeID) -> &[NodeID] {
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
    Owned(Box<[NodeID]>),
}

/*
 * Enum for storing mutable uses of node. Using get_uses_mut, one can easily
 * modify the defs that a node uses.
 */
#[derive(Debug)]
pub enum NodeUsesMut<'a> {
    Zero,
    One([&'a mut NodeID; 1]),
    Two([&'a mut NodeID; 2]),
    Three([&'a mut NodeID; 3]),
    Variable(Box<[&'a mut NodeID]>),
}

impl<'a> AsRef<[NodeID]> for NodeUses<'a> {
    fn as_ref(&self) -> &[NodeID] {
        match self {
            NodeUses::Zero => &[],
            NodeUses::One(x) => x,
            NodeUses::Two(x) => x,
            NodeUses::Three(x) => x,
            NodeUses::Variable(x) => x,
            NodeUses::Owned(x) => x,
        }
    }
}

impl<'a> AsMut<[&'a mut NodeID]> for NodeUsesMut<'a> {
    fn as_mut(&mut self) -> &mut [&'a mut NodeID] {
        match self {
            NodeUsesMut::Zero => &mut [],
            NodeUsesMut::One(x) => x,
            NodeUsesMut::Two(x) => x,
            NodeUsesMut::Three(x) => x,
            NodeUsesMut::Variable(x) => x,
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
        Node::Match { control, sum } => NodeUses::Two([*control, *sum]),
        Node::Fork { control, factor: _ } => NodeUses::One([*control]),
        Node::Join { control } => NodeUses::One([*control]),
        Node::Phi { control, data } => {
            let mut uses: Vec<NodeID> = Vec::from(&data[..]);
            uses.push(*control);
            NodeUses::Owned(uses.into_boxed_slice())
        }
        Node::ThreadID { control } => NodeUses::One([*control]),
        Node::Reduce {
            control,
            init,
            reduct,
        } => NodeUses::Three([*control, *init, *reduct]),
        Node::Return { control, data } => NodeUses::Two([*control, *data]),
        Node::Parameter { index: _ } => NodeUses::One([NodeID::new(0)]),
        Node::Constant { id: _ } => NodeUses::One([NodeID::new(0)]),
        Node::DynamicConstant { id: _ } => NodeUses::One([NodeID::new(0)]),
        Node::Unary { input, op: _ } => NodeUses::One([*input]),
        Node::Binary { left, right, op: _ } => NodeUses::Two([*left, *right]),
        Node::Call {
            function: _,
            dynamic_constants: _,
            args,
        } => NodeUses::Variable(args),
        Node::Read { collect, indices } => {
            let mut uses = vec![];
            for index in indices.iter() {
                if let Index::Position(pos) = index {
                    uses.append(&mut pos.clone().into_vec());
                }
            }
            if uses.len() > 0 {
                uses.push(*collect);
                NodeUses::Owned(uses.into_boxed_slice())
            } else {
                NodeUses::One([*collect])
            }
        }
        Node::Write {
            collect,
            indices,
            data,
        } => {
            let mut uses = vec![];
            for index in indices.iter() {
                if let Index::Position(pos) = index {
                    uses.append(&mut pos.clone().into_vec());
                }
            }
            if uses.len() > 0 {
                uses.push(*collect);
                uses.push(*data);
                NodeUses::Owned(uses.into_boxed_slice())
            } else {
                NodeUses::Two([*collect, *data])
            }
        }
    }
}

/*
 * Construct NodeUsesMut for a node. Note, this is not a one-to-one mutable
 * analog of NodeUses. In particular, constant, dynamic constant, and parameter
 * nodes all implicitly take as input the start node. However, this is not
 * stored (it is an implict use), and thus can't be modified. Thus, those uses
 * are not represented in NodeUsesMut, but are in NodeUses.
 */
pub fn get_uses_mut<'a>(node: &'a mut Node) -> NodeUsesMut<'a> {
    match node {
        Node::Start => NodeUsesMut::Zero,
        Node::Region { preds } => NodeUsesMut::Variable(preds.iter_mut().collect()),
        Node::If { control, cond } => NodeUsesMut::Two([control, cond]),
        Node::Match { control, sum } => NodeUsesMut::Two([control, sum]),
        Node::Fork { control, factor: _ } => NodeUsesMut::One([control]),
        Node::Join { control } => NodeUsesMut::One([control]),
        Node::Phi { control, data } => {
            NodeUsesMut::Variable(std::iter::once(control).chain(data.iter_mut()).collect())
        }
        Node::ThreadID { control } => NodeUsesMut::One([control]),
        Node::Reduce {
            control,
            init,
            reduct,
        } => NodeUsesMut::Three([control, init, reduct]),
        Node::Return { control, data } => NodeUsesMut::Two([control, data]),
        Node::Parameter { index: _ } => NodeUsesMut::Zero,
        Node::Constant { id: _ } => NodeUsesMut::Zero,
        Node::DynamicConstant { id: _ } => NodeUsesMut::Zero,
        Node::Unary { input, op: _ } => NodeUsesMut::One([input]),
        Node::Binary { left, right, op: _ } => NodeUsesMut::Two([left, right]),
        Node::Call {
            function: _,
            dynamic_constants: _,
            args,
        } => NodeUsesMut::Variable(args.iter_mut().collect()),
        Node::Read { collect, indices } => {
            let mut uses = vec![];
            for index in indices.iter_mut() {
                if let Index::Position(pos) = index {
                    for d in pos.iter_mut() {
                        uses.push(d);
                    }
                }
            }
            if uses.len() > 0 {
                uses.push(collect);
                NodeUsesMut::Variable(uses.into_boxed_slice())
            } else {
                NodeUsesMut::One([collect])
            }
        }
        Node::Write {
            collect,
            indices,
            data,
        } => {
            let mut uses = vec![];
            for index in indices.iter_mut() {
                if let Index::Position(pos) = index {
                    for d in pos.iter_mut() {
                        uses.push(d);
                    }
                }
            }
            if uses.len() > 0 {
                uses.push(collect);
                uses.push(data);
                NodeUsesMut::Variable(uses.into_boxed_slice())
            } else {
                NodeUsesMut::Two([collect, data])
            }
        }
    }
}
