### Sequences as Conventional Interfaces

In working with compound data, we've stressed how data abstraction permits us to design programs without becoming enmeshed in the details of data representations, and how abstraction preserves for us the flexibility to experiment with alternative representations. In this section, we introduce another powerful design principle for working with data structures -- the use of *conventional interfaces*.

In the section [Formulating Abstractions with Higher-Order Procedures]() we saw how program abstractions, implemented as higher-order procedures, can capture common patterns in programs that deal with numerical data. Our ability to formulate analogous operations for working with compound data depends crucially on the style in which we manipulate our data structures. Consider, for example, the following procedure, analogous to the ``count-leaves/2`` procedure of the section [Hierarchical Structures](), which takes a tree as argument and computes the sum of the squares of the leaves that are odd:

```lisp

```