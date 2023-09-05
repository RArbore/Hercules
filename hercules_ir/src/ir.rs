#[derive(Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
}

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub param_types: Vec<TypeID>,
    pub return_type: TypeID,
    pub nodes: Vec<Node>,
}

#[derive(Clone)]
pub enum Type {
    Control(u64),
    Integer8,
    Integer16,
    Integer32,
    Integer64,
    Float32,
    Float64,
}

#[derive(Clone)]
pub enum Constant {
    Integer8(u8),
    Integer16(u16),
    Integer32(u32),
    Integer64(u64),
    Float32(f32),
    Float64(f64),
}

#[derive(Clone)]
pub enum Node {
    Start,
    Region {
        preds: Box<[NodeID]>,
    },
    If {
        control: NodeID,
        cond: NodeID,
    },
    Fork {
        control: NodeID,
        factor: usize,
    },
    Join {
        control: NodeID,
        factor: usize,
    },
    Phi {
        control: NodeID,
        data: Box<[NodeID]>,
    },
    Return {
        control: NodeID,
        value: NodeID,
    },
    Parameter {
        index: usize,
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

#[derive(Clone)]
pub struct FunctionID(u32);

impl FunctionID {
    pub fn new(x: usize) -> Self {
        FunctionID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone)]
pub struct NodeID(u32);

impl NodeID {
    pub fn new(x: usize) -> Self {
        NodeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone)]
pub struct ConstantID(u32);

impl ConstantID {
    pub fn new(x: usize) -> Self {
        ConstantID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone)]
pub struct TypeID(u32);

impl TypeID {
    pub fn new(x: usize) -> Self {
        TypeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}
