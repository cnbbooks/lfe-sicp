### Exercises

#### Exercise 2.24

Suppose we evaluate the expression ``(list 1 (list 2 (list 3 4)))``. Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree diagram.

#### Exercise 2.25

Give combinations of ``car``s and ``cdr``s that will pick 7 from each of the following lists:

```lisp
(1 3 (5 7) 9)

((7))

(1 (2 (3 (4 (5 (6 7))))))
```

#### Exercise 2.26

Suppose we define x and y to be two lists:

```lisp
(defun x () (list 1 2 3))
(defun y () (list 4 5 6))
```

What result is printed by the interpreter in response to evaluating each of the following expressions:

```lisp
(append (x) (y))

(cons (x) (y))

(list (x) (y))
```

#### Exercise 2.27

Modify your reverse function of exercise 2.18 to produce a deep-reverse function that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

```lisp
> (defun x () (list (list 1 2) (list 3 4)))
x
> (x)
((1 2) (3 4))
> (reverse (x))
((3 4) (1 2))
> (deep-reverse (x))
((4 3) (2 1))
```

#### Exercise 2.28

Write a function ``fringe`` that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,

```lisp
> (defun x () (list (list 1 2) (list 3 4)))
> (fringe (x))
(1 2 3 4)
> (fringe (list (x) (x)))
(1 2 3 4 1 2 3 4)
```

#### Exercise 2.29

A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using ``list``):

```lisp
(defun make-mobile (left right)
  (list left right))
```

A branch is constructed from a ``length`` (which must be a number) together with a ``structure``, which may be either a number (representing a simple weight) or another mobile:

```lisp
(defun make-branch (length structure)
  (list length structure))
```

a.  Write the corresponding selectors ``left-branch`` and ``right-branch``, which return the branches of a mobile, and ``branch-length`` and ``branch-structure``, which return the components of a branch.

b.  Using your selectors, define a function ``total-weight`` that returns the total weight of a mobile.

c.  A mobile is said to be *balanced* if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.

d.  Suppose we change the representation of mobiles so that the constructors are

```lisp
(defun make-mobile (left right)
  (cons left right))
(defun make-branch (length structure)
  (cons length structure))
```

How much do you need to change your programs to convert to the new representation?

