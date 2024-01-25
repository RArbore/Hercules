# Hercules IR

Hercules IR is structured as following:
- One entire program lives in one "Module".
- Each module contains a set of functions, as well as interned types, constants, and dynamic constants. The most important element of a module is its resident functions.
- Each function consists of a name, a set of types for its parameters, a return type, a list of nodes, and the number of dynamic constants it takes as argument. Types are not needed for dynamic constants, since all dynamic constants have type u64. The most important element of a function is its node list.
- There are control and data types. The control type is parameterized by a list of thread replication factors. The primitive data types are boolean, signed integers, unsigned integers, and floating point numbers. The integer types can hold 8, 16, 32, or 64 bits. The floating point types can hold 32 or 64 bits. The compound types are product, summation, and arrays. A product type is a tuple, containing some number of children data types. A summation type is a union, containing exactly one of some number of children data types at runtime. An array is a dynamically indexable collection of elements, where each element is the same type. The size of the array is part of the type, and is represented with a dynamic constant.
- Dynamic constants are constants provided to the conductor when a Hercules IR program is started. Through this mechanism, Hercules IR can represent programs operating on a variable number of array elements, while forbidding runtime dynamic memory allocation (all dynamic memory allocation happens in the conductor).
- The nodes in a function are structured as a flow graph, which an explicit start node. Although control and data flow from definitions to uses, def-use edges are stored implicitly in the IR. Each node stores its predecessor nodes, so use-def edges are stored explicitly. To query the def-use edges in an IR graph, use the `def_use` function.

Below, all of the nodes in Hercules IR are described.

## Start

The start node of the IR flow graph. This node is implicitly defined in the text format. It takes no inputs. Its output type is the empty control type (control with no thread replication factors).

## Region

Region nodes are the mechanism for merging multiple branches inside Hercules IR. A region node takes at least one input - each input must have a control type, and all of the inputs must have the same control type. The output type of the region node is the same control type as all of its inputs. The main purpose of a region node is to drive some number of [phi](#phi) nodes.

## If

The branch mechanism in Hercules IR. An if node takes two inputs - a control predecessor, and a condition. The control predecessor must have control type, and the condition must have boolean type. The output type is the same control type as the control input. Every if node must be followed directly by two [read](#read) nodes, each of which represents the opposite destinations of the branch. This is the mechanism by which the output edges from the if node (and also the [match](#match) node) are labelled, even though nodes only explicitly store their input edges.

## Fork

Fork (and [join](#join)) nodes are the mechanism for representing data-parallelism inside Hercules IR. A fork node takes one input - a control predecessor. A fork node also stores a thread replication factor (TRF), represented as a dynamic constant. The output type of a fork node is a control type, which is the same as the type of the control predecessor, with the TRF pushed to the end of the control type's factor list. Conceptually, for every thread that comes in to a fork node, TRF threads come out. A fork node can drive any number of children [thread\_id](#threadid) nodes. Each fork must have a single corresponding [join](#join) node - the fork must dominate the join node, and the join node must post-dominate the fork node (in the control flow subgraph).

## Join

Join (and [fork](#fork)) nodes are the mechanism for synchronizing data-parallel threads inside Hercules IR. A join nodes takes one input - a control predecessor. The output type of a join node is a control type, which is the same as the type of the control predecessor, with the last factor in the control type's list removed. Conceptually, after all threads created by the corresponding fork reach the join, then and only then does the join output a single thread. A join node can drive any number of children [collect](#collect) nodes. Each join must have a single corresponding [fork](#fork) node - the join must post-dominate the fork node, and the fork node must dominate the join node (in the control flow subgraph).

## Phi

Phi nodes merge potentially many data sources into one data output, driven by a corresponding region node. Phi nodes in Hercules IR perform the same function as phi nodes in other SSA-based IRs. Phi nodes take at least one input - a control predecessor, and some number of data inputs. The control predecessor of a phi node must be a region node. The data inputs must all have the same type. The output of the phi node has that data type. In the sea of nodes execution model, a phi node can be thought of as "latching" when its corresponding region node is reached. The phi node will latch to output the value of the input corresponding to the input that control traversed to reach the region node. After latching, the phi node's output won't change until the region node is reached again.

## ThreadID

The thread\_id node provides the thread ID as a datum to children nodes after a [fork](#fork) has been performed. A thread\_id node takes one input - a control predecessor. The control predecessor must be a [fork](#fork) node. The output type is a 64-bit unsigned integer. The output thread IDs generated by a thread\_id node range from 0 to TRF - 1, inclusive, where TRF is the thread replication factor of the input [fork](#fork) node.

## Collect

The collect node collects data from multiple executing threads, and puts them all into an array. A collect node takes two inputs - a control predecessor, and a data input. The control predecessor must be a [join](#join) node. The data input must have a non-control type. The output type will be an array, where the element type will be the type of the data input. The extent of the array will be equal to the thread replication factor of the [fork](#fork) node corresponding to the input [join](#join) node. For each datum input, the thread ID corresponding to that datum will be the index the datum is inserted into the array.

## Return

The return node returns some data from the current function. A return node has two inputs - a control predecessor, and a data input. The control predecessor must have a control type with an empty factor list - just as only one thread starts the execution of a function, only one thread can return from a function. The data input must have the same type as the function's return type. No node should use a return node as input (technically, the output type of a return node is an empty product type).

## Parameter

The parameter node represents a parameter of the function. A parameter node takes one input - the start node. A parameter node stores the parameter index of the function it corresponds to. Its value at runtime is the index-th argument to the function. Its output type is the type of the index-th parameter of the function.

## Constant

The constant node represents a constant value. A constant node takes one input - the start node. A constant node stores the constant ID of the constant it corresponds to. Its value at runtime is the constant it references. Its output type is the type of the constant it references.

## DynamicConstant

The dynamic\_constant node represents a dynamic constant, used as a runtime value. A dynamic\_constant node takes one input - the start node. A dynamic\_constant node stores the dynamic constant ID of the dynamic constant it corresponds to. Its value at runtime is the value of the dynamic constant it references, which is calculated at conductor time. Its output type is a 64-bit unsigned integer.

## Unary

The unary node represents a basic unary operation. A unary node takes one input - a data input. The data input must have a non-control type. A unary node additionally stores which unary operation it performs. The output type of the unary node is the same as its input type. The acceptable input data type depends on the unary operation.

## Binary

The binary node represents a basic binary operation. A binary node takes two inputs - a left data input, and a right data input. The left and right data inputs must be the same non-control type. A binary node additionally stores the binary operation it performs. The output type of the binary node is the same as its input type. The acceptable input data type depends on the binary operation.

## Call

The call node passes its inputs to a function, and outputs the result of the function call. A call node takes some number of data inputs. A call node also stores a reference to the function it calls. The number and types of the data inputs must match the referenced function. A call node also stores references to dynamic constants it uses as inputs to the function. The number of dynamic constants references must match the number of dynamic constant inputs of the referenced function. The output type of a call node is the return type of the referenced function. A call node notably does not take as input or output a control type. This is because all operations in Hercules IR are pure, including arbitrary function calls. Thus, the only things affecting a function call are the data inputs, and (conceptually) the function may be called an arbitrary amount of times.

## Read

The read node reads an element from a collection consisting of product, summation, and array types. It uses a series of indices, that index successive levels in a collection's type tree. There are 4 kinds of indices - fields (for products), variants (for summations), positions (for arrays), and controls (for succeeding [if](#if) and [match](#match) nodes). A read node takes one collect input, and potentially position inputs (there are node inputs only for positions). The output type of a read node is the indexed type in the collection's type tree.

## Write

The write node writes an element into a collection consisting of product, summation, and array types. It uses a series of indices, that index successive levels in a collection's type tree. There are 4 kinds of indices - fields (for products), variants (for summations), and positions (for arrays). A write node takes one collect input, a data input, and potentially position inputs (there are node inputs only for positions). The output type of a write node is the same as the collection input. The indexed type in the collection must be the same type as the data input.

## Match

The match node branches based on the variant of a sum typed value. A match node takes two inputs - a control predecessor, and a sum input. The control predecessor must have control type, and the sum input must have a sum type. The output type is a product of N control types, where N is the number of possible variants in the sum input's sum type. The control types in the product are the same as the control input's type. Every match node must be followed directly by N [read](#read) nodes, each of which reads differing elements of the match node's output product. This is the mechanism by which the output edges from the match node (and also the [if](#if) node) are labelled, even though nodes only explicitly store their input edges.
