pub struct FunctionID(u32);

pub struct NodeID(u32);
const NULL_NODE: NodeID = NodeID(0xFFFFFFFF);

pub struct ConstantID(u32);

pub struct TypeID(u32);

pub struct Module {
    functions: Vec<Function>,
    typed: Vec<Type>,
    constants: Vec<Constant>,
}

pub struct Function {
    name: String,
    nodes: Vec<Node>,
}

pub enum Type {
    Control,
    Integer8,
    Integer16,
    Integer32,
    Integer64,
    Float32,
    Float64,
}

pub enum Constant {
    Integer8(u8),
    Integer16(u16),
    Integer32(u32),
    Integer64(u64),
    Float32(f32),
    Float64(f64),
}

pub enum Node {
    Start,
    Region {
        preds: Box<[NodeID]>,
    },
    If {
        control: NodeID,
        cond: NodeID,
    },
    Phi {
        control: NodeID,
        data: Box<[NodeID]>,
    },
    Return {
        control: NodeID,
        value: NodeID,
    },
    Constant {
        id: ConstantID,
    },
    Add {
        control: NodeID,
        left: NodeID,
        right: NodeID,
    },
    Sub {
        control: NodeID,
        left: NodeID,
        right: NodeID,
    },
    Mul {
        control: NodeID,
        left: NodeID,
        right: NodeID,
    },
    Div {
        control: NodeID,
        left: NodeID,
        right: NodeID,
    },
    Call {
        args: Box<[NodeID]>,
    },
}

impl From<u32> for FunctionID {
    fn from(v: u32) -> Self {
        FunctionID(v)
    }
}

impl From<u64> for FunctionID {
    fn from(v: u64) -> Self {
        FunctionID(v as u32)
    }
}

impl From<usize> for FunctionID {
    fn from(v: usize) -> Self {
        FunctionID(v as u32)
    }
}

impl From<u32> for NodeID {
    fn from(v: u32) -> Self {
        NodeID(v)
    }
}

impl From<u64> for NodeID {
    fn from(v: u64) -> Self {
        NodeID(v as u32)
    }
}

impl From<usize> for NodeID {
    fn from(v: usize) -> Self {
        NodeID(v as u32)
    }
}

impl From<u32> for ConstantID {
    fn from(v: u32) -> Self {
        ConstantID(v)
    }
}

impl From<u64> for ConstantID {
    fn from(v: u64) -> Self {
        ConstantID(v as u32)
    }
}

impl From<usize> for ConstantID {
    fn from(v: usize) -> Self {
        ConstantID(v as u32)
    }
}
