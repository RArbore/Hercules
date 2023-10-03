extern crate ordered_float;

/*
 * A module is a list of functions. Functions contain types, constants, and
 * dynamic constants, which are interned at the module level. Thus, if one
 * wants to run an intraprocedural pass in parallel, it is advised to first
 * destruct the module, then reconstruct it once finished.
 */
#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<Type>,
    pub constants: Vec<Constant>,
    pub dynamic_constants: Vec<DynamicConstant>,
}

/*
 * A function has a name, a list of types for its parameters, a single return
 * type, a list of nodes in its sea-of-nodes style IR, and a number of dynamic
 * constants. When calling a function, arguments matching the parameter types
 * are required, as well as the correct number of dynamic constants. All
 * dynamic constants are 64-bit unsigned integers (usize / u64), so it is
 * sufficient to merely store how many of them the function takes as arguments.
 */
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub param_types: Vec<TypeID>,
    pub return_type: TypeID,
    pub nodes: Vec<Node>,
    pub num_dynamic_constants: u32,
}

/*
 * Hercules IR has a fairly standard type system, with the exception of the
 * control type. Hercules IR is based off of the sea-of-nodes IR, the main
 * feature of which being a merged control and data flow graph. Thus, control
 * is a type of value, just like any other type. However, the type system is
 * very restrictive over what can be done with control values. A novel addition
 * in Hercules IR is that a control type is parameterized by a list of thread
 * spawning factors. This is the mechanism in Hercules IR for representing
 * parallelism. Summation types are an IR equivalent of Rust's enum types.
 * These are lowered into tagged unions during scheduling. Array types are one-
 * dimensional. Multi-dimensional arrays are represented by nesting array types.
 * An array extent is represented with a dynamic constant.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Control(Box<[DynamicConstantID]>),
    Boolean,
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
    Array(TypeID, DynamicConstantID),
}

impl Type {
    pub fn is_control(&self) -> bool {
        if let Type::Control(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_bool(&self) -> bool {
        self == &Type::Boolean
    }

    pub fn is_unsigned(&self) -> bool {
        match self {
            Type::UnsignedInteger8 => true,
            Type::UnsignedInteger16 => true,
            Type::UnsignedInteger32 => true,
            Type::UnsignedInteger64 => true,
            _ => false,
        }
    }

    pub fn is_fixed(&self) -> bool {
        match self {
            Type::Integer8 => true,
            Type::Integer16 => true,
            Type::Integer32 => true,
            Type::Integer64 => true,
            Type::UnsignedInteger8 => true,
            Type::UnsignedInteger16 => true,
            Type::UnsignedInteger32 => true,
            Type::UnsignedInteger64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::Float32 => true,
            Type::Float64 => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        self.is_fixed() || self.is_float()
    }

    pub fn is_primitive(&self) -> bool {
        self.is_bool() || self.is_fixed() || self.is_float()
    }
}

/*
 * Constants are pretty standard in Hercules IR. Float constants used the
 * ordered_float crate so that constants can be keys in maps (used for
 * interning constants during IR construction). Product, summation, and array
 * constants all contain their own type. This is only strictly necessary for
 * summation types, but provides a nice mechanism for sanity checking for
 * product and array types as well.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Boolean(bool),
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
    Product(TypeID, Box<[ConstantID]>),
    Summation(TypeID, u32, ConstantID),
    Array(TypeID, Box<[ConstantID]>),
}

/*
 * Dynamic constants are unsigned 64-bit integers passed to a Hercules function
 * at runtime using the Hercules runtime API. They cannot be the result of
 * computations in Hercules IR. For a single execution of a Hercules function,
 * dynamic constants are constant throughout execution. This provides a
 * mechanism by which Hercules functions can operate on arrays with variable
 * length, while not needing Hercules functions to perform dynamic memory
 * allocation - by providing dynamic constants to the runtime API, the runtime
 * can allocate memory as necessary.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DynamicConstant {
    Constant(usize),
    Parameter(usize),
}

/*
 * Hercules IR is a combination of a possibly cylic control flow graph, and
 * many acyclic data flow graphs. Each node represents some operation on input
 * values (including control), and produces some output value. Operations that
 * conceptually produce multiple outputs (such as an if node) produce a product
 * type instead. For example, the if node produces prod(control(N),
 * control(N)), where the first control token represents the false branch, and
 * the second control token represents the true branch. Another example is the
 * fork node, which produces prod(control(N, K), u64), where the u64 is the
 * thread ID. Functions are devoid of side effects, so call nodes don't take as
 * input or output control tokens. There is also no global memory - use arrays.
 */
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
        data: NodeID,
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
    DynamicConstant {
        id: DynamicConstantID,
    },
    Unary {
        input: NodeID,
        op: UnaryOperator,
    },
    Binary {
        left: NodeID,
        right: NodeID,
        op: BinaryOperator,
    },
    Call {
        function: FunctionID,
        dynamic_constants: Box<[DynamicConstantID]>,
        args: Box<[NodeID]>,
    },
    ReadProd {
        prod: NodeID,
        index: usize,
    },
    WriteProd {
        prod: NodeID,
        data: NodeID,
        index: usize,
    },
    ReadArray {
        array: NodeID,
        index: NodeID,
    },
    WriteArray {
        array: NodeID,
        data: NodeID,
        index: NodeID,
    },
    Match {
        control: NodeID,
        sum: NodeID,
    },
    BuildSum {
        data: NodeID,
        sum_ty: TypeID,
        variant: usize,
    },
    ExtractSum {
        data: NodeID,
        variant: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Not,
    Neg,
    Bitflip,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    LT,
    LTE,
    GT,
    GTE,
    EQ,
    NE,
    Or,
    And,
    Xor,
    LSh,
    RSh,
}

impl Node {
    pub fn is_return(&self) -> bool {
        if let Node::Return {
            control: _,
            value: _,
        } = self
        {
            true
        } else {
            false
        }
    }

    pub fn upper_case_name(&self) -> &'static str {
        match self {
            Node::Start => "Start",
            Node::Region { preds: _ } => "Region",
            Node::If {
                control: _,
                cond: _,
            } => "If",
            Node::Fork {
                control: _,
                factor: _,
            } => "Fork",
            Node::Join {
                control: _,
                data: _,
            } => "Join",
            Node::Phi {
                control: _,
                data: _,
            } => "Phi",
            Node::Return {
                control: _,
                value: _,
            } => "Return",
            Node::Parameter { index: _ } => "Parameter",
            Node::DynamicConstant { id: _ } => "DynamicConstant",
            Node::Constant { id: _ } => "Constant",
            Node::Unary { input: _, op } => op.upper_case_name(),
            Node::Binary {
                left: _,
                right: _,
                op,
            } => op.upper_case_name(),
            Node::Call {
                function: _,
                dynamic_constants: _,
                args: _,
            } => "Unary",
            Node::ReadProd { prod: _, index: _ } => "ReadProd",
            Node::WriteProd {
                prod: _,
                data: _,
                index: _,
            } => "WriteProd",
            Node::ReadArray { array: _, index: _ } => "ReadArray",
            Node::WriteArray {
                array: _,
                data: _,
                index: _,
            } => "WriteArray",
            Node::Match { control: _, sum: _ } => "Match",
            Node::BuildSum {
                data: _,
                sum_ty: _,
                variant: _,
            } => "BuildSum",
            Node::ExtractSum {
                data: _,
                variant: _,
            } => "ExtractSum",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            Node::Start => "start",
            Node::Region { preds: _ } => "region",
            Node::If {
                control: _,
                cond: _,
            } => "if",
            Node::Fork {
                control: _,
                factor: _,
            } => "fork",
            Node::Join {
                control: _,
                data: _,
            } => "join",
            Node::Phi {
                control: _,
                data: _,
            } => "phi",
            Node::Return {
                control: _,
                value: _,
            } => "return",
            Node::Parameter { index: _ } => "parameter",
            Node::DynamicConstant { id: _ } => "dynamic_constant",
            Node::Constant { id: _ } => "constant",
            Node::Unary { input: _, op } => op.lower_case_name(),
            Node::Binary {
                left: _,
                right: _,
                op,
            } => op.lower_case_name(),
            Node::Call {
                function: _,
                dynamic_constants: _,
                args: _,
            } => "call",
            Node::ReadProd { prod: _, index: _ } => "read_prod",
            Node::WriteProd {
                prod: _,
                data: _,
                index: _,
            } => "write_prod ",
            Node::ReadArray { array: _, index: _ } => "read_array",
            Node::WriteArray {
                array: _,
                data: _,
                index: _,
            } => "write_array",
            Node::Match { control: _, sum: _ } => "match",
            Node::BuildSum {
                data: _,
                sum_ty: _,
                variant: _,
            } => "build_sum",
            Node::ExtractSum {
                data: _,
                variant: _,
            } => "extract_sum",
        }
    }
}

impl UnaryOperator {
    pub fn upper_case_name(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "Not",
            UnaryOperator::Neg => "Neg",
            UnaryOperator::Bitflip => "Bitflip",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "not",
            UnaryOperator::Neg => "neg",
            UnaryOperator::Bitflip => "bitflip",
        }
    }
}

impl BinaryOperator {
    pub fn upper_case_name(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "Add",
            BinaryOperator::Sub => "Sub",
            BinaryOperator::Mul => "Mul",
            BinaryOperator::Div => "Div",
            BinaryOperator::Rem => "Rem",
            BinaryOperator::LT => "LT",
            BinaryOperator::LTE => "LTE",
            BinaryOperator::GT => "GT",
            BinaryOperator::GTE => "GTE",
            BinaryOperator::EQ => "EQ",
            BinaryOperator::NE => "NE",
            BinaryOperator::Or => "Or",
            BinaryOperator::And => "And",
            BinaryOperator::Xor => "Xor",
            BinaryOperator::LSh => "LSh",
            BinaryOperator::RSh => "RSh",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "add",
            BinaryOperator::Sub => "sub",
            BinaryOperator::Mul => "mul",
            BinaryOperator::Div => "div",
            BinaryOperator::Rem => "rem",
            BinaryOperator::LT => "lt",
            BinaryOperator::LTE => "lte",
            BinaryOperator::GT => "gt",
            BinaryOperator::GTE => "gte",
            BinaryOperator::EQ => "eq",
            BinaryOperator::NE => "ne",
            BinaryOperator::Or => "or",
            BinaryOperator::And => "and",
            BinaryOperator::Xor => "xor",
            BinaryOperator::LSh => "lsh",
            BinaryOperator::RSh => "rsh",
        }
    }
}

/*
 * Rust things to make newtyped IDs usable.
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionID(u32);

impl FunctionID {
    pub fn new(x: usize) -> Self {
        FunctionID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeID(u32);

impl NodeID {
    pub fn new(x: usize) -> Self {
        NodeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ConstantID(u32);

impl ConstantID {
    pub fn new(x: usize) -> Self {
        ConstantID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeID(u32);

impl TypeID {
    pub fn new(x: usize) -> Self {
        TypeID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DynamicConstantID(u32);

impl DynamicConstantID {
    pub fn new(x: usize) -> Self {
        DynamicConstantID(x as u32)
    }

    pub fn idx(&self) -> usize {
        self.0 as usize
    }
}
