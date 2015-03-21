# Summary

* [Introduction](README.md)
* [Title Page](fm/title-page.md)
* [Copyright Page](fm/copyright-page.md)
* [Dedication](fm/dedication.md)
* [Foreword](fm/foreword.md)
* [Foreword to the LFE Edition](fm/lfe-foreword.md)
* [Preface to the LFE Edition](fm/preface-3/README.md)
   * [The Hidden Origins of Lisp](fm/preface-3/origins.md)
       * [Giuseppe Peano](fm/preface-3/peano.md)
       * [Bertrand Russell](fm/preface-3/russell.md)
       * [Alonzo Church](fm/preface-3/church.md)
       * [John McCarthy](fm/preface-3/mccarthy.md)
   * [A Recap of Erlang's Genesis](fm/preface-3/erlang.md)
   * [The Inspiration for LFE](fm/preface-3/lfe.md)
   * [The Place of Lisp in the 21st Century](fm/preface-3/21stcent.md)
   * [Notes on Changes from the Original](fm/preface-3/changes.md)
   * [Obtaining the Book and Related Code](fm/preface-3/sources.md)
* [Preface to the Second Edition](fm/preface-2.md)
* [Preface to the First Edition](fm/preface-1.md)
* [Acknowledgments](fm/acknowledgments.md)
* [Building Abstractions with Functions](ch1/building-abstractions-with-functions.md)
   * [Programming in Lisp](ch1/programming-in-lisp.md)
   * [The Elements of Programming](ch1/the-elements-of-programming.md)
       * [Expressions](ch1/expressions.md)
       * [Naming and the Environment](ch1/naming-and-the-environment.md)
       * [Evaluating Combinations](ch1/evaluating-combinations.md)
       * [Compound Functions](ch1/compound-functions.md)
       * [The Substitution Model for Function Application](ch1/the-substitution-model-for-function-application.md)
       * [Conditional Expressions and Predicates](ch1/conditional-expressions-and-predicates.md)
       * [Exercises](ch1/exercises-1.md)
       * [Example: Square Roots by Newton's Method](ch1/example-square-roots-by-newtons-method.md)
       * [Exercises](ch1/exercises-2.md)
       * [Functions as Black-Box Abstractions](ch1/functions-as-black-box-abstractions.md)
   * [Functions and the Processes They Generate](ch1/functions-and-the-processes-they-generate.md)
       * [Linear Recursion and Iteration](ch1/linear-recursion-and-iteration.md)
       * [Exercises](ch1/exercises-3.md)
       * [Tree Recursion](ch1/tree-recursion.md)
       * [Exercises](ch1/exercises-4.md)
       * [Orders of Growth](ch1/orders-of-growth.md)
       * [Exercises](ch1/exercises-5.md)
       * [Exponentiation](ch1/exponentiation.md)
       * [Exercises](ch1/exercises-6.md)
       * [Greatest Common Divisors](ch1/greatest-common-divisors.md)
       * [Exercises](ch1/exercises-7.md)
       * [Example: Testing for Primality](ch1/example-testing-for-primality.md)
       * [Exercises](ch1/exercises-8.md)
   * [Formulating Abstractions with Higher-Order Functions](ch1/formulating-abstractions-with-higher-order-functions.md)
       * [Functions as Arguments](ch1/functions-as-arguments.md)
       * [Exercises](ch1/exercises-9.md)
       * [Constructing Functions Using Lambda](ch1/constructing-functions-using-lambda.md)
       * [Exercises](ch1/exercises-10.md)
       * [Functions as General Methods](ch1/functions-as-general-methods.md)
       * [Exercises](ch1/exercises-11.md)
       * [Functions as Returned Values](ch1/functions-as-returned-values.md)
       * [Exercises](ch1/exercises-12.md)
* [Building Abstractions with Data](ch2/building-abstractions-with-data.md)
   * [Introduction to Data Abstraction](ch2/introduction-to-data-abstraction.md)
       * [Example: Arithmetic Operations for Rational Numbers](ch2/example-arithmetic-operationsfor-rational-numbers.md)
       * [Exercises](ch2/exercises-1.md)
       * [Abstraction Barriers](ch2/abstraction-barriers.md)
       * [Exercises](ch2/exercises-2.md)
       * [What Is Meant by Data?](ch2/what-is-meant-by-data.md)
       * [Exercises](ch2/exercises-3.md)
       * [Extended Exercise: Interval Arithmetic](ch2/extended-exercise-interval-arithmetic.md)
       * [Exercises](ch2/exercises-4.md)
   * [Hierarchical Data and the Closure Property](ch2/hierarchical-data-and-the-closure-property.md)
       * [Representing Sequences](ch2/representing-sequences.md)
           * [List operations](ch2/representing-sequences-list-operations.md)
           * [Exercises](ch2/exercises-5.md)
           * [Mapping over lists](ch2/representing-sequences-mapping-over-lists.md)
           * [Exercises](ch2/exercises-6.md)
       * [Hierarchical Structures](ch2/hierarchical-structures.md)
           * [Exercises](ch2/exercises-7.md)
           * [Mapping over trees](ch2/hierarchical-structures-mapping-over-trees.md)
           * [Exercises](ch2/exercises-8.md)
       * [Sequences as Conventional Interfaces](ch2/sequences-as-conventional-interfaces.md)
           * [Sequence operations](ch2/sequence-operations.md)
           * [Exercises](ch2/exercises-9.md)
           * [Nested mappings](ch2/nested-mappings.md)
           * [Exercises](ch2/exercises-10.md)
       * [Example: A Picture Language[(ch2/example-picture-language.md)
           * [The picture language[(ch2/the-picture-language.md)
           * [Exercises](ch2/exercises-11.md)
           * [Higher order operations](ch2/higher-order-operations.md)
           * [Frames](ch2/frames.md)
           * [Exercises](ch2/exercises-12.md)
           * [Painters](ch2/painters.md)
           * [Exercises](ch2/exercises-13.md)
           * [Transforming and combining painters](ch2/transforming-and-combining-painters.md)
           * [Exercises](ch2/exercises-14.md)
           * [Levels of language for robust design](ch2/levels-of-language-for-robust-design.md)
           * [Exercises](ch2/exercises-15.md)
   * Symbolic Data
       * Quotation
       * Example: Symbolic Differentiation
       * Example: Representing Sets
       * Example: Huffman Encoding Trees
   * Multiple Representations for Abstract Data
       * Representations for Complex Numbers
       * Tagged data
       * Data-Directed Programming and Additivity
   * Systems with Generic Operations
       * Generic Arithmetic Operations
       * Combining Data of Different Types
       * Example: Symbolic Algebra
* Modularity, Objects, and State
   * Assignment and Local State
       * Local State Variables
       * The Benefits of Introducing Assignment
       * The Costs of Introducing Assignment
   * The Environment Model of Evaluation
       * The Rules for Evaluation
       * Applying Simple Functions
       * Frames as the Repository of Local State
       * Internal Definitions
   * Modeling with Mutable Data
       * Mutable List Structure
       * Representing Queues
       * Representing Tables
       * A Simulator for Digital Circuits
       * Propagation of Constraints
   * Concurrency: Time Is of the Essence
       * The Nature of Time in Concurrent Systems
       * Mechanisms for Controlling Concurrency
   * Streams
       * Streams Are Delayed Lists
       * Infinite Streams
       * Exploiting the Stream Paradigm
       * Streams and Delayed Evaluation
       * Modularity of Functional Programs and Modularity of Objects
* Metalinguistic Abstraction
   * The Metacircular Evaluator
       * The Core of the Evaluator
       * Representing Expressions
       * Evaluator Data Structures
       * Running the Evaluator as a Program
       * Data as Programs
       * Internal Definitions
       * Separating Syntactic Analysis from Execution
   * Variations on a Scheme -- Lazy Evaluation
       * Normal Order and Applicative Order
       * An Interpreter with Lazy Evaluation
       * Streams as Lazy Lists
   * Variations on a Scheme -- Nondeterministic Computing
       * Amb and Search
       * Examples of Nondeterministic Programs
       * Implementing the Amb Evaluator
   * Logic Programming
       * Deductive Information Retrieval
       * How the Query System Works
       * Is Logic Programming Mathematical Logic?
       * Implementing the Query System
* Computing with Register Machines
   * Designing Register Machines
       * A Language for Describing Register Machines
       * Abstraction in Machine Design
       * Subroutines
       * Using a Stack to Implement Recursion
       * Instruction Summary
   * A Register-Machine Simulator
       * The Machine Model
       * The Assembler
       * Generating Execution Functions for Instructions
       * Monitoring Machine Performance
   * Storage Allocation and Garbage Collection
       * Memory as Vectors
       * Maintaining the Illusion of Infinite Memory
   * The Explicit-Control Evaluator
       * The Core of the Explicit-Control Evaluator
       * Sequence Evaluation and Tail Recursion
       * Conditionals, Assignments, and Definitions
       * Running the Evaluator
   * Compilation
       * Structure of the Compiler
       * Compiling Expressions
       * Compiling Combinations
       * Combining Instruction Sequences
       * An Example of Compiled Code
       * Lexical Addressing
       * Interfacing Compiled Code to the Evaluator
* [References](bm/references.md)
* [List of Exercises](bm/list-of-exercises.md)

