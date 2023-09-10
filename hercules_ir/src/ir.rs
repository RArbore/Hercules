extern crate ordered_float;

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
    pub dynamic_constants: Vec<DynamicConstant>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub param_types: Vec<TypeID>,
    pub return_type: TypeID,
    pub nodes: Vec<Node>,
    pub num_dynamic_constants: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Control(DynamicConstantID),
    Integer8,
    Integer16,
    Integer32,
    Integer64,
    UnsignedInteger8,
    UnsignedInteger16,
    UnsignedInteger32,
    UnsignedInteger64,
    Float32,
    Float64,
    Product(Box<[TypeID]>),
    Summation(Box<[TypeID]>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Integer8(i8),
    Integer16(i16),
    Integer32(i32),
    Integer64(i64),
    UnsignedInteger8(u8),
    UnsignedInteger16(u16),
    UnsignedInteger32(u32),
    UnsignedInteger64(u64),
    Float32(ordered_float::OrderedFloat<f32>),
    Float64(ordered_float::OrderedFloat<f64>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DynamicConstant {
    Constant(usize),
    Parameter(usize),
}

#[derive(Debug, Clone)]
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
        factor: DynamicConstantID,
    },
    Join {
        control: NodeID,
        factor: DynamicConstantID,
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
        control: NodeID,
        function: FunctionID,
        args: Box<[NodeID]>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionID(u32);

impl FunctionID {
    pub fn new(x: usize) -> Self {
        FunctionID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeID(u32);

impl NodeID {
    pub fn new(x: usize) -> Self {
        NodeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantID(u32);

impl ConstantID {
    pub fn new(x: usize) -> Self {
        ConstantID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeID(u32);

impl TypeID {
    pub fn new(x: usize) -> Self {
        TypeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DynamicConstantID(u32);

impl DynamicConstantID {
    pub fn new(x: usize) -> Self {
        DynamicConstantID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}
