extern crate bitvec;
extern crate ordered_float;

use std::fmt::Write;
use std::ops::Coroutine;
use std::ops::CoroutineState;
use std::pin::Pin;

use self::bitvec::prelude::*;

use crate::*;

/*
 * A module is a list of functions. Functions contain types, constants, and
 * dynamic constants, which are interned at the module level. Thus, if one
 * wants to run an intraprocedural pass in parallel, it is advised to first
 * destruct the module, then reconstruct it once finished.
 */
#[derive(Debug, Default, Clone)]
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
 * very restrictive over what can be done with control values. An addition in
 * Hercules IR is that a control type is parameterized by a list of thread
 * spawning factors. This is the mechanism in Hercules IR for representing
 * parallelism. Summation types are an IR equivalent of Rust's enum types.
 * These are lowered into tagged unions during scheduling.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Control(Box<[NodeID]>),
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
    Array(TypeID, Box<[DynamicConstantID]>),
}

/*
 * Constants are pretty standard in Hercules IR. Float constants used the
 * ordered_float crate so that constants can be keys in maps (used for
 * interning constants during IR construction). Product, summation, and array
 * constants all contain their own type. This is only strictly necessary for
 * summation types, but provides a nice mechanism for sanity checking for
 * product and array types as well. There is also a zero initializer constant,
 * which stores its own type as well. The zero value of a summation is defined
 * as the zero value of the first variant.
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
    Zero(TypeID),
}

/*
 * Dynamic constants are unsigned 64-bit integers passed to a Hercules function
 * at runtime using the Hercules conductor API. They cannot be the result of
 * computations in Hercules IR. For a single execution of a Hercules function,
 * dynamic constants are constant throughout execution. This provides a
 * mechanism by which Hercules functions can operate on arrays with variable
 * length, while not needing Hercules functions to perform dynamic memory
 * allocation - by providing dynamic constants to the conductor API, the
 * conductor can allocate memory as necessary.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DynamicConstant {
    Constant(usize),
    Parameter(usize),
}

/*
 * Hercules has a single node for reading, Read, and a single node for writing,
 * Write, that are both used for modifying product, sum, and array structures.
 * However, each of these types are indexed differently. Thus, these two nodes
 * operate on an index list, composing indices at different levels in a type
 * tree. Each type that can be indexed has a unique variant in the index enum.
 * Read nodes are overloaded to select between control successors of if and
 * match nodes.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Index {
    Field(usize),
    Variant(usize),
    Position(Box<[NodeID]>),
    Control(usize),
}

/*
 * Hercules IR is a combination of a possibly cylic control flow graph, and
 * many possibly cyclic data flow graphs. Each node represents some operation on
 * input values (including control), and produces some output value. Operations
 * that conceptually produce multiple outputs (such as an if node) produce a
 * product type instead. For example, the if node produces prod(control(N),
 * control(N)), where the first control token represents the false branch, and
 * the second control token represents the true branch. Functions are devoid of
 * side effects, so call nodes don't take as input or output control tokens.
 * There is also no global memory - use arrays.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Start,
    Region {
        preds: Box<[NodeID]>,
    },
    If {
        control: NodeID,
        cond: NodeID,
    },
    Match {
        control: NodeID,
        sum: NodeID,
    },
    Fork {
        control: NodeID,
        factor: DynamicConstantID,
    },
    Join {
        control: NodeID,
    },
    Phi {
        control: NodeID,
        data: Box<[NodeID]>,
    },
    ThreadID {
        control: NodeID,
    },
    Reduce {
        control: NodeID,
        init: NodeID,
        reduct: NodeID,
    },
    Return {
        control: NodeID,
        data: NodeID,
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
    Ternary {
        first: NodeID,
        second: NodeID,
        third: NodeID,
        op: TernaryOperator,
    },
    Call {
        function: FunctionID,
        dynamic_constants: Box<[DynamicConstantID]>,
        args: Box<[NodeID]>,
    },
    Read {
        collect: NodeID,
        indices: Box<[Index]>,
    },
    Write {
        collect: NodeID,
        data: NodeID,
        indices: Box<[Index]>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Not,
    Neg,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TernaryOperator {
    Select,
}

impl Module {
    /*
     * There are many transformations that need to iterate over the functions
     * in a module, while having mutable access to the interned types,
     * constants, and dynamic constants in a module. This code is really ugly,
     * so write it once.
     */
    pub fn map<F>(self, mut func: F) -> Self
    where
        F: FnMut(
            (Function, FunctionID),
            (Vec<Type>, Vec<Constant>, Vec<DynamicConstant>),
        ) -> (Function, (Vec<Type>, Vec<Constant>, Vec<DynamicConstant>)),
    {
        let Module {
            functions,
            types,
            constants,
            dynamic_constants,
        } = self;
        let mut stuff = (types, constants, dynamic_constants);
        let functions = functions
            .into_iter()
            .enumerate()
            .map(|(idx, function)| {
                let mut new_stuff = (vec![], vec![], vec![]);
                std::mem::swap(&mut stuff, &mut new_stuff);
                let (function, mut new_stuff) = func((function, FunctionID::new(idx)), new_stuff);
                std::mem::swap(&mut stuff, &mut new_stuff);
                function
            })
            .collect();
        let (types, constants, dynamic_constants) = stuff;
        Module {
            functions,
            types,
            constants,
            dynamic_constants,
        }
    }

    /*
     * Printing out types, constants, and dynamic constants fully requires a
     * reference to the module, since references to other types, constants, and
     * dynamic constants are done using IDs.
     */
    pub fn write_type<W: Write>(&self, ty_id: TypeID, w: &mut W) -> std::fmt::Result {
        match &self.types[ty_id.idx()] {
            Type::Control(_) => write!(w, "Control"),
            Type::Boolean => write!(w, "Boolean"),
            Type::Integer8 => write!(w, "Integer8"),
            Type::Integer16 => write!(w, "Integer16"),
            Type::Integer32 => write!(w, "Integer32"),
            Type::Integer64 => write!(w, "Integer64"),
            Type::UnsignedInteger8 => write!(w, "UnsignedInteger8"),
            Type::UnsignedInteger16 => write!(w, "UnsignedInteger16"),
            Type::UnsignedInteger32 => write!(w, "UnsignedInteger32"),
            Type::UnsignedInteger64 => write!(w, "UnsignedInteger64"),
            Type::Float32 => write!(w, "Float32"),
            Type::Float64 => write!(w, "Float64"),
            Type::Product(fields) => {
                write!(w, "Product(")?;
                for idx in 0..fields.len() {
                    let field_ty_id = fields[idx];
                    self.write_type(field_ty_id, w)?;
                    if idx + 1 < fields.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")
            }
            Type::Summation(fields) => {
                write!(w, "Summation(")?;
                for idx in 0..fields.len() {
                    let field_ty_id = fields[idx];
                    self.write_type(field_ty_id, w)?;
                    if idx + 1 < fields.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")
            }
            Type::Array(elem, extents) => {
                write!(w, "Array(")?;
                self.write_type(*elem, w)?;
                for extent in extents.iter() {
                    write!(w, ", ")?;
                    self.write_dynamic_constant(*extent, w)?;
                }
                write!(w, ")")
            }
        }?;

        Ok(())
    }

    pub fn write_constant<W: Write>(&self, cons_id: ConstantID, w: &mut W) -> std::fmt::Result {
        match &self.constants[cons_id.idx()] {
            Constant::Boolean(val) => write!(w, "{}", val),
            Constant::Integer8(val) => write!(w, "{}", val),
            Constant::Integer16(val) => write!(w, "{}", val),
            Constant::Integer32(val) => write!(w, "{}", val),
            Constant::Integer64(val) => write!(w, "{}", val),
            Constant::UnsignedInteger8(val) => write!(w, "{}", val),
            Constant::UnsignedInteger16(val) => write!(w, "{}", val),
            Constant::UnsignedInteger32(val) => write!(w, "{}", val),
            Constant::UnsignedInteger64(val) => write!(w, "{}", val),
            Constant::Float32(val) => write!(w, "{}", val),
            Constant::Float64(val) => write!(w, "{}", val),
            Constant::Product(_, fields) => {
                write!(w, "(")?;
                for idx in 0..fields.len() {
                    let field_cons_id = fields[idx];
                    self.write_constant(field_cons_id, w)?;
                    if idx + 1 < fields.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")
            }
            Constant::Summation(_, variant, field) => {
                write!(w, "%{}(", variant)?;
                self.write_constant(*field, w)?;
                write!(w, ")")
            }
            Constant::Array(_, elems) => {
                write!(w, "[")?;
                for idx in 0..elems.len() {
                    let elem_cons_id = elems[idx];
                    self.write_constant(elem_cons_id, w)?;
                    if idx + 1 < elems.len() {
                        write!(w, ", ")?;
                    }
                }
                write!(w, "]")
            }
            Constant::Zero(_) => write!(w, "zero"),
        }?;

        Ok(())
    }

    pub fn write_dynamic_constant<W: Write>(
        &self,
        dc_id: DynamicConstantID,
        w: &mut W,
    ) -> std::fmt::Result {
        match &self.dynamic_constants[dc_id.idx()] {
            DynamicConstant::Constant(cons) => write!(w, "{}", cons),
            DynamicConstant::Parameter(param) => write!(w, "#{}", param),
        }?;

        Ok(())
    }

    /*
     * Create an iterator that traverses all the types in the module bottom up.
     * This uses a coroutine to make iteratively traversing the type DAGs
     * easier.
     */
    pub fn types_bottom_up(&self) -> impl Iterator<Item = TypeID> + '_ {
        let types = &self.types;
        let mut visited = bitvec![u8, Lsb0; 0; self.types.len()];
        let mut stack = (0..self.types.len())
            .map(TypeID::new)
            .collect::<Vec<TypeID>>();
        let coroutine = move || {
            // Since this is a coroutine, handle recursion manually.
            while let Some(id) = stack.pop() {
                if visited[id.idx()] {
                    continue;
                }
                match &types[id.idx()] {
                    Type::Product(children) | Type::Summation(children) => {
                        // We have to yield the children of this node before
                        // this node itself. We keep track of which nodes have
                        // yielded using visited.
                        let can_yield = children.iter().all(|x| visited[x.idx()]);
                        if can_yield {
                            visited.set(id.idx(), true);
                            yield id;
                        } else {
                            // Push ourselves, then children, so that children
                            // get popped first.
                            stack.push(id);
                            for id in children.iter() {
                                stack.push(*id);
                            }
                        }
                    }
                    Type::Array(child, _) => {
                        // Same idea as product / summation, but there's only
                        // one child.
                        let can_yield = visited[child.idx()];
                        if can_yield {
                            visited.set(id.idx(), true);
                            yield id;
                        } else {
                            stack.push(id);
                            stack.push(*child);
                        }
                    }
                    _ => {
                        visited.set(id.idx(), true);
                        yield id;
                    }
                }
            }
        };
        CoroutineIterator {
            coroutine: Box::new(coroutine),
        }
    }

    /*
     * Create an iterator that traverses all the constants in the module bottom up.
     * This uses a coroutine to make iteratively traversing the constant DAGs
     * easier.
     */
    pub fn constants_bottom_up(&self) -> impl Iterator<Item = ConstantID> + '_ {
        let constants = &self.constants;
        let mut visited = bitvec![u8, Lsb0; 0; self.constants.len()];
        let mut stack = (0..self.constants.len())
            .map(ConstantID::new)
            .collect::<Vec<ConstantID>>();
        let coroutine = move || {
            // Since this is a coroutine, handle recursion manually.
            while let Some(id) = stack.pop() {
                if visited[id.idx()] {
                    continue;
                }
                match &constants[id.idx()] {
                    Constant::Product(_, children) | Constant::Array(_, children) => {
                        // We have to yield the children of this node before
                        // this node itself. We keep track of which nodes have
                        // yielded using visited.
                        let can_yield = children.iter().all(|x| visited[x.idx()]);
                        if can_yield {
                            visited.set(id.idx(), true);
                            yield id;
                        } else {
                            // Push ourselves, then children, so that children
                            // get popped first.
                            stack.push(id);
                            for id in children.iter() {
                                stack.push(*id);
                            }
                        }
                    }
                    Constant::Summation(_, _, child) => {
                        // Same idea as product / summation, but there's only
                        // one child.
                        let can_yield = visited[child.idx()];
                        if can_yield {
                            visited.set(id.idx(), true);
                            yield id;
                        } else {
                            stack.push(id);
                            stack.push(*child);
                        }
                    }
                    _ => {
                        visited.set(id.idx(), true);
                        yield id;
                    }
                }
            }
        };
        CoroutineIterator {
            coroutine: Box::new(coroutine),
        }
    }

    /*
     * Unfortunately, determining if a constant is an array requires both
     * knowledge of constants and types, due to zero initializer constants.
     */
    pub fn is_array_constant(&self, cons_id: ConstantID) -> bool {
        if let Constant::Zero(ty_id) = self.constants[cons_id.idx()] {
            self.types[ty_id.idx()].is_array()
        } else {
            self.constants[cons_id.idx()].is_strictly_array()
        }
    }
}

struct CoroutineIterator<G, I>
where
    G: Coroutine<Yield = I, Return = ()> + Unpin,
{
    coroutine: G,
}

impl<G, I> Iterator for CoroutineIterator<G, I>
where
    G: Coroutine<Yield = I, Return = ()> + Unpin,
{
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        // Iterator corresponds to yields from coroutine.
        match Pin::new(&mut self.coroutine).resume(()) {
            CoroutineState::Yielded(item) => Some(item),
            CoroutineState::Complete(_) => None,
        }
    }
}

impl Function {
    /*
     * Many transformations will delete nodes. There isn't strictly a gravestone
     * node value, so use the start node as a gravestone value (for IDs other
     * than 0). This function cleans up gravestoned nodes.
     */
    pub fn delete_gravestones(&mut self) {
        // Step 1: figure out which nodes are gravestones.
        let mut gravestones = (0..self.nodes.len())
            .filter(|x| *x != 0 && self.nodes[*x].is_start())
            .map(|x| NodeID::new(x));

        // Step 2: figure out the mapping between old node IDs and new node IDs.
        let mut node_mapping = Vec::with_capacity(self.nodes.len());
        let mut next_gravestone = gravestones.next();
        let mut num_gravestones_passed = 0;
        for idx in 0..self.nodes.len() {
            if Some(NodeID::new(idx)) == next_gravestone {
                node_mapping.push(NodeID::new(0));
                num_gravestones_passed += 1;
                next_gravestone = gravestones.next();
            } else {
                node_mapping.push(NodeID::new(idx - num_gravestones_passed));
            }
        }

        // Step 3: create new nodes vector. Along the way, update all uses.
        let mut old_nodes = vec![];
        std::mem::swap(&mut old_nodes, &mut self.nodes);

        let mut new_nodes = Vec::with_capacity(old_nodes.len() - num_gravestones_passed);
        for (idx, mut node) in old_nodes.into_iter().enumerate() {
            // Skip node if it's dead.
            if idx != 0 && node.is_start() {
                continue;
            }

            // Update uses.
            for u in get_uses_mut(&mut node).as_mut() {
                let old_id = **u;
                let new_id = node_mapping[old_id.idx()];
                if new_id == NodeID::new(0) && old_id != NodeID::new(0) {
                    panic!("While deleting gravestones, came across a use of a gravestoned node.");
                }
                **u = new_id;
            }

            // Add to new_nodes.
            new_nodes.push(node);
        }

        std::mem::swap(&mut new_nodes, &mut self.nodes);
    }
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

    pub fn is_array(&self) -> bool {
        if let Type::Array(_, _) = self {
            true
        } else {
            false
        }
    }

    pub fn try_element_type(&self) -> Option<TypeID> {
        if let Type::Array(elem, _) = self {
            Some(*elem)
        } else {
            None
        }
    }

    pub fn try_extents(&self) -> Option<&[DynamicConstantID]> {
        if let Type::Array(_, extents) = self {
            Some(extents)
        } else {
            None
        }
    }
}

pub fn element_type(mut ty: TypeID, types: &Vec<Type>) -> TypeID {
    while let Type::Array(elem, _) = types[ty.idx()] {
        ty = elem;
    }
    ty
}

impl Constant {
    pub fn is_strictly_array(&self) -> bool {
        if let Constant::Array(_, _) = self {
            true
        } else {
            false
        }
    }

    /*
     * Useful for GVN.
     */
    pub fn is_zero(&self) -> bool {
        match self {
            Constant::Integer8(0) => true,
            Constant::Integer16(0) => true,
            Constant::Integer32(0) => true,
            Constant::Integer64(0) => true,
            Constant::UnsignedInteger8(0) => true,
            Constant::UnsignedInteger16(0) => true,
            Constant::UnsignedInteger32(0) => true,
            Constant::UnsignedInteger64(0) => true,
            Constant::Float32(ord) => *ord == ordered_float::OrderedFloat::<f32>(0.0),
            Constant::Float64(ord) => *ord == ordered_float::OrderedFloat::<f64>(0.0),
            Constant::Zero(_) => true,
            _ => false,
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            Constant::Integer8(1) => true,
            Constant::Integer16(1) => true,
            Constant::Integer32(1) => true,
            Constant::Integer64(1) => true,
            Constant::UnsignedInteger8(1) => true,
            Constant::UnsignedInteger16(1) => true,
            Constant::UnsignedInteger32(1) => true,
            Constant::UnsignedInteger64(1) => true,
            Constant::Float32(ord) => *ord == ordered_float::OrderedFloat::<f32>(1.0),
            Constant::Float64(ord) => *ord == ordered_float::OrderedFloat::<f64>(1.0),
            _ => false,
        }
    }
}

/*
 * Simple predicate functions on nodes take a lot of space, so use a macro.
 */
macro_rules! define_pattern_predicate {
    ($x: ident, $y: pat) => {
        pub fn $x(&self) -> bool {
            if let $y = self {
                true
            } else {
                false
            }
        }
    };
}

impl Index {
    define_pattern_predicate!(is_control, Index::Control(_));

    pub fn try_field(&self) -> Option<usize> {
        if let Index::Field(field) = self {
            Some(*field)
        } else {
            None
        }
    }

    pub fn try_position(&self) -> Option<&[NodeID]> {
        if let Index::Position(indices) = self {
            Some(&indices)
        } else {
            None
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            Index::Field(_) => "field",
            Index::Variant(_) => "variant",
            Index::Position(_) => "position",
            Index::Control(_) => "control",
        }
    }
}

impl Node {
    define_pattern_predicate!(is_start, Node::Start);
    define_pattern_predicate!(is_region, Node::Region { preds: _ });
    define_pattern_predicate!(
        is_if,
        Node::If {
            control: _,
            cond: _,
        }
    );
    define_pattern_predicate!(
        is_fork,
        Node::Fork {
            control: _,
            factor: _,
        }
    );
    define_pattern_predicate!(is_join, Node::Join { control: _ });
    define_pattern_predicate!(
        is_phi,
        Node::Phi {
            control: _,
            data: _,
        }
    );
    define_pattern_predicate!(is_thread_id, Node::ThreadID { control: _ });
    define_pattern_predicate!(
        is_reduce,
        Node::Reduce {
            control: _,
            init: _,
            reduct: _,
        }
    );
    define_pattern_predicate!(
        is_return,
        Node::Return {
            control: _,
            data: _,
        }
    );
    define_pattern_predicate!(
        is_read,
        Node::Read {
            collect: _,
            indices: _
        }
    );
    define_pattern_predicate!(
        is_write,
        Node::Write {
            collect: _,
            indices: _,
            data: _
        }
    );
    define_pattern_predicate!(is_match, Node::Match { control: _, sum: _ });

    /*
     * Read nodes can be considered control when following an if or match
     * node. However, it is sometimes useful to exclude such nodes when
     * considering control nodes.
     */
    pub fn is_strictly_control(&self) -> bool {
        self.is_start()
            || self.is_region()
            || self.is_if()
            || self.is_match()
            || self.is_fork()
            || self.is_join()
            || self.is_return()
    }

    pub fn upper_case_name(&self) -> &'static str {
        match self {
            Node::Start => "Start",
            Node::Region { preds: _ } => "Region",
            Node::If {
                control: _,
                cond: _,
            } => "If",
            Node::Match { control: _, sum: _ } => "Match",
            Node::Fork {
                control: _,
                factor: _,
            } => "Fork",
            Node::Join { control: _ } => "Join",
            Node::Phi {
                control: _,
                data: _,
            } => "Phi",
            Node::ThreadID { control: _ } => "ThreadID",
            Node::Reduce {
                control: _,
                init: _,
                reduct: _,
            } => "Reduce",
            Node::Return {
                control: _,
                data: _,
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
            Node::Ternary {
                first: _,
                second: _,
                third: _,
                op,
            } => op.upper_case_name(),
            Node::Call {
                function: _,
                dynamic_constants: _,
                args: _,
            } => "Unary",
            Node::Read {
                collect: _,
                indices: _,
            } => "Read",
            Node::Write {
                collect: _,
                data: _,
                indices: _,
            } => "Write",
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
            Node::Match { control: _, sum: _ } => "match",
            Node::Fork {
                control: _,
                factor: _,
            } => "fork",
            Node::Join { control: _ } => "join",
            Node::Phi {
                control: _,
                data: _,
            } => "phi",
            Node::ThreadID { control: _ } => "thread_id",
            Node::Reduce {
                control: _,
                init: _,
                reduct: _,
            } => "reduce",
            Node::Return {
                control: _,
                data: _,
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
            Node::Ternary {
                first: _,
                second: _,
                third: _,
                op,
            } => op.lower_case_name(),
            Node::Call {
                function: _,
                dynamic_constants: _,
                args: _,
            } => "call",
            Node::Read {
                collect: _,
                indices: _,
            } => "read",
            Node::Write {
                collect: _,
                data: _,
                indices: _,
            } => "write",
        }
    }

    pub fn is_control(&self) -> bool {
        if self.is_strictly_control() {
            return true;
        }

        if let Node::Read {
            collect: _,
            indices,
        } = self
        {
            return indices.len() == 1 && indices[0].is_control();
        }

        false
    }
}

impl UnaryOperator {
    pub fn upper_case_name(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "Not",
            UnaryOperator::Neg => "Neg",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "not",
            UnaryOperator::Neg => "neg",
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

impl TernaryOperator {
    pub fn upper_case_name(&self) -> &'static str {
        match self {
            TernaryOperator::Select => "Select",
        }
    }

    pub fn lower_case_name(&self) -> &'static str {
        match self {
            TernaryOperator::Select => "select",
        }
    }
}

/*
 * Rust things to make newtyped IDs usable.
 */

#[macro_export]
macro_rules! define_id_type {
    ($x: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $x(u32);

        impl $x {
            pub fn new(x: usize) -> Self {
                $x(x as u32)
            }

            pub fn idx(&self) -> usize {
                self.0 as usize
            }
        }
    };
}

define_id_type!(FunctionID);
define_id_type!(NodeID);
define_id_type!(TypeID);
define_id_type!(ConstantID);
define_id_type!(DynamicConstantID);
