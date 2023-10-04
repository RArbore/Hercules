# Hercules' Design

Hercules' is a compiler targeting heterogenous devices. The key goals of Hercules are listed below:

- Generate optimized, memory efficient, and parallel code for devices containing CPUs, GPUs, and other processing elements.
- Explore language design for programming heterogenous systems in a performant, expressive, and safe manner.
- Expose detailed configuration of code generation and scheduling through a novel scheduling language.
- Design an intermediate representation that allows for fine-grained control of what code is executed on what device in a system.
- Develop a runtime system capable of dynamically scheduling generated code fragments on a heterogenous machine.

The following sections contain information on how Hercules is designed to meet these goals.

## Front-end Language Design

TODO: @aaronjc4

## Scheduling Language Design

TODO: @aaronjc4

## Compiler Design

The Hercules' compiler is split into the following components:

### Hercules IR

The IR of the Hercules compiler is similar to the sea of nodes IR presented in "A Simple Graph-Based Intermediate Representation", with a few differences.

- There are dynamic constants, which are constants provided dynamically to the conductor (this is the runtime system, [see the section describing the conductor](#the-conductor)) - these can be used to specify array type sizes, unlike normal runtime values.
- There is no single global store. The closest analog are individual values with an array type, which support dynamically indexed read and write operations.
- There is no I/O, or other side effects.
- There is no recursion.
- The implementation of Hercules IR does not follow the original object oriented design of sea-of-nodes.

A key design consideration of Hercules IR is the absence of a concept of memory. A downside of this approach is that any language targetting Hecules IR must also be very restrictive regarding memory - in practice, this means tightly controlling or eliminating first-class references. The upside is that the compiler has complete freedom to layout data however it likes in memory when performing code generation. This includes deciding which data resides in which address spaces, which is a necessary ability for a compiler striving to have fine-grained control over what operations are computed on what devices.

In addition to not having a generalized memory, Hercules IR has no functionality for calling functions with side-effects, or doing IO. In other words, Hercules is a pure IR (it's not functional, as functions aren't first class values). This may be changed in the future - we could support effectful programs by giving call operators a control input and output edge. However, at least for now, we'd like to work with the simplest IR possible, so the IR is pure.

The key idea behind the sea of nodes IR is that control flow and data flow are represented in the same graph. The entire program thus can be represented by one large flow graph. This has several nice properties, the primary of which being that instructions are unordered except by true dependencies. This alleviates most code motion concerns, and also makes peephole optimizations more practical. Additionally, loop invariant code is neither "inside" nor "outside" a loop in the sea of nodes. Thus, any optimizations benefitting from a particular assumption about the position of loop invariant code works without needing to do code motion. Deciding whether code lives inside a loop or not becomes a scheduling concern.

We chose to use a sea of nodes based IR because we believe it will be easier to partition than a CFG + basic block style IR. A CFG + basic block IR is inherently two-level - there is the control flow level in the CFG, and the data flow in the basic blocks. Partitioning a function across these two levels is a challenging task. As shown by previous work (HPVM), introducing more graph levels into the IR makes partitioning harder, not easier. We want Hercules to have fine-grained control over which code executes where. This requires Hercules' compiler IR to have as few graph levels as reasonable.

Hercules IR is structured as following:
- One entire program lives in one "Module".
- Each module contains a set of functions, as well as interned types, constants, and dynamic constants. The most important element of a module is its resident functions.
- Each function consists of a name, a set of types for its parameters, a return type, a list of nodes, and the number of dynamic constants it takes as argument. Types are not needed for dynamic constants, since all dynamic constants have type u64. The most important element of a function is its node list.
- There are control and data types. The control type is parameterized by a list of thread replication factors. The primitive data types are boolean, signed integers, unsigned integers, and floating point numbers. The integer types can hold 8, 16, 32, or 64 bits. The floating point types can hold 32 or 64 bits. The compound types are product, summation, and arrays. A product type is a tuple, containing some number of children data types. A summation type is a union, containing exactly one of some number of children data types at runtime. An array is a dynamically indexable collection of elements, where each element is the same type. The size of the array is part of the type, and is represented with a dynamic constant.
- Dynamic constants are constants provided to the conductor when a Hercules IR program is started. Through this mechanism, Hercules IR can represent programs operating on a variable number of array elements, while forbidding runtime dynamic memory allocation (all dynamic memory allocation happens in the conductor).
- The nodes in a function are structured as a flow graph, which an explicit start node. Although control and data flow from definitions to uses, def-use edges are stored implicitly in the IR. Each node stores its predecessor nodes, so use-def edges are stored explicitly. To query the def-use edges in an IR graph, use the `def_use` function.

Below, all of the nodes in Hercules IR are described.

#### Start

The start node of the IR flow graph. This node is implicitly defined in the text format. It takes no inputs. Its output type is the empty control type (control with no thread replication factors).

#### Region

Region nodes are the mechanism for merging multiple branches inside Hercules IR. A region node takes at least one input - each input must have a control type, and all of the inputs must have the same control type. The output type of the region node is the same control type as all of its inputs. The main purpose of a region node is to drive some number of [phi](#phi) nodes.

#### If

The branch mechanism in Hercules IR. An if node takes two inputs - a control predecessor, and a condition. The control predecessor must have control type, and the condition must have boolean type. The output type is a product of two control types, which are the same as the control input's type. Every if node must be followed directly by two [read\_prod](#readprod) nodes, each of which reads differing elements of the if node's output product. This is the mechanism by which the output edges from the if node (and also the [match](#match) node) are labelled, even though nodes only explicitly store their input edges.

#### Fork

Fork (and [join](#join)) nodes are the mechanism for representing data-parallelism inside Hercules IR. A fork node takes one input - a control predecessor. A fork node also stores a thread replication factor (TRF), represented as a dynamic constant. The output type of a fork node is a control type, which is the same as the type of the control predecessor, with the TRF pushed to the end of the control type's factor list. Conceptually, for every thread that comes in to a fork node, TRF threads come out. A fork node can drive any number of children [thread\_id](#threadid) nodes. Each fork must have a single corresponding [join](#join) node - the fork must dominate the join node, and the join node must post-dominate the fork node (in the control flow subgraph).

#### Join

Join (and [fork](#fork)) nodes are the mechanism for synchronizing data-parallel threads inside Hercules IR. A join nodes takes one input - a control predecessor. The output type of a join node is a control type, which is the same as the type of the control predecessor, with the last factor in the control type's list removed. Conceptually, after all threads created by the corresponding fork reach the join, then and only then does the join output a single thread. A join node can drive any number of children [collect](#collect) nodes. Each join must have a single corresponding [fork](#fork) node - the join must post-dominate the fork node, and the fork node must dominate the join node (in the control flow subgraph).

#### Phi

Phi nodes merge potentially many data sources into one data output, driven by a corresponding region node. Phi nodes in Hercules IR perform the same function as phi nodes in other SSA-based IRs. Phi nodes take at least one input - a control predecessor, and some number of data inputs. The control predecessor of a phi node must be a region node. The data inputs must all have the same type. The output of the phi node has that data type. In the sea of nodes execution model, a phi node can be thought of as "latching" when its corresponding region node is reached. The phi node will latch to output the value of the input corresponding to the input that control traversed to reach the region node. After latching, the phi node's output won't change until the region node is reached again.

#### ThreadID

The thread\_id node provides the thread ID as a datum to children nodes after a [fork](#fork) has been performed. A thread\_id node takes one input - a control predecessor. The control predecessor must be a [fork](#fork) node. The output type is a 64-bit unsigned integer. The output thread IDs generated by a thread\_id node range from 0 to TRF - 1, inclusive, where TRF is the thread replication factor of the input [fork](#fork) node.

#### Collect

The collect node collects data from multiple executing threads, and puts them all into an array. A collect node takes two inputs - a control predecessor, and a data input. The control predecessor must be a [join](#join) node. The data input must have a non-control type. The output type will be an array, where the element type will be the type of the data input. The extent of the array will be equal to the thread replication factor of the [fork](#fork) node corresponding to the input [join](#join) node. For each datum input, the thread ID corresponding to that datum will be the index the datum is inserted into the array.

#### Return

The return node returns some data from the current function. A return node has two inputs - a control predecessor, and a data input. The control predecessor must have a control type with an empty factor list - just as only one thread starts the execution of a function, only one thread can return from a function. The data input must have the same type as the function's return type. No node should use a return node as input (technically, the output type of a return node is an empty product type).

#### Parameter

The parameter node represents a parameter of the function. A parameter node takes no inputs. A parameter node stores the parameter index of the function it corresponds to. Its value at runtime is the index-th argument to the function. Its output type is the type of the index-th parameter of the function.

#### Constant

The constant node represents a constant value. A constant node takes no inputs. A constant node stores the constant ID of the constant it corresponds to. Its value at runtime is the constant it references. Its output type is the type of the constant it references.

#### DynamicConstant

The dynamic\_constant node represents a dynamic constant, used as a runtime value. A dynamic\_constant node takes no inputs. A dynamic\_constant node stores the dynamic constant ID of the dynamic constant it corresponds to. Its value at runtime is the value of the dynamic constant it references, which is calculated at conductor time. Its output type is a 64-bit unsigned integer.

### Optimizations

Hercules relies on other compiler infrastructures, such as LLVM, to do code generation for specific devices. Thus, Hercules itself doesn't perform particularly sophisticated optimizations. In general, the optimizations Hercules do are done to make partitioning easier. This includes things like GVN and peephole optimizations, which in general, make the IR "simpler".

TODO: @rarbore2

### Partitioning

Partitioning is responsible for deciding which operations in the IR graph are executed on which devices. Additionally, operations are broken up into shards - every node in a shard executes on the same device, and the runtime system schedules execution at the shard level. Partitioning is conceptually very similar to instruction selection. Each shard can be thought of as a single instruction, and the device the shard is executed on can be thought of as the particular instruction being selected. In instruction selection, there is not only the choice of which instructions to use, but also how to partition the potentially many operations in the IR into a smaller number of target instructions. Similarly, the Hercules IR partitioning process must decide which operations are grouped together into the same shard, and for each shard, which device it should execute on. The set of operations each potential target device is capable of executing is crucial information when forming the shard boundaries, so this cannot be performed optimally as a sequential two step process.

TODO: @rarbore2

### Code Generation

Hercules uses LLVM for generating CPU and GPU code. Memory is "introduced" into the program representation at this stage. Operations in a function are separated into basic blocks. The data layout of values is decided on, and memory is allocated on the stack or is designated as separately allocated and passed into functions as necessary. Code is generated corresponding to possibly several estimates of dynamic constants.

TODO: @rarbore2

## The Conductor

The conductor is responsible for dynamically executing code generated by Hercules. It exposes a Rust API for executing Hercules code. It takes care of memory allocation, synchronization, and scheduling. It is what is called the "runtime" in other systems - we chose a different name as there are events that happen distinctly as "conductor time" (such as providing dynamic constants), rather than at "runtime" (where the generated code is actually executed).

TODO: @rarbore2
