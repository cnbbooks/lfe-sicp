#### Sequence Operations

The key to organizing programs so as to more clearly reflect the signal-flow structure is to concentrate on the "signals" that flow from one stage in the process to the next. If we represent these signals as lists, then we can use list operations to implement the processing at each of the stages. For instance, we can implement the mapping stages of the signal-flow diagrams using the ``mapper/2`` function from the section [Representing Sequences]():

```lisp
> (mapper #'square/1 (list 1 2 3 4 5))
(1 4 9 16 25)
```

Filtering a sequence to select only those elements that satisfy a given predicate is accomplished by

```lisp
(defun filter
  ((_ '())
   '())
  ((predicate (cons head tail))
   (if (funcall predicate head)
       (cons head (filter predicate tail))
       (filter predicate tail))))
```

For example,

```lisp
> (filter #'odd?/1 (list 1 2 3 4 5))
(1 3 5)
```

Accumulations can be implemented by

```lisp
(defun accumulate
  ((_ initial '())
   initial)
  ((op initial (cons head tail))
   (funcall op head (accumulate op initial tail))))
```

With usage such as

```lisp
> (accumulate #'+/2 0 (list 1 2 3 4 5))
15
> (accumulate #'*/2 1 (list 1 2 3 4 5))
120
> (accumulate #'cons/2 '() (list 1 2 3 4 5))
(1 2 3 4 5)
```

All that remains to implement signal-flow diagrams is to enumerate the sequence of elements to be processed. For ``even-fibs/1``, we need to generate the sequence of integers in a given range, which we can do as follows[^1]:

```lisp
(defun enumerate-interval
  ((low high) (when (> low high))
   '())
  ((low high)
   (cons low (enumerate-interval (+ low 1) high))))
```

```lisp
> (enumerate-interval 2 7)
(2 3 4 5 6 7)
```

To enumerate the leaves of a tree, we can use [^2]:

```lisp
(defun enumerate-tree
  (('())
   '())
  (((cons head tail))
   (append (enumerate-tree head)
           (enumerate-tree tail)))
  ((tree)
   (list tree)))
```

```lisp
> (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
(1 2 3 4 5)
```

Now we can reformulate ``sum-odd-squares/1`` and ``even-fibs/1`` as in the signal-flow diagrams. For ``sum-odd-squares/1``, we enumerate the sequence of leaves of the tree, filter this to keep only the odd numbers in the sequence, square each element, and sum the results:

```lisp
(defun sum-odd-squares (tree)
  (accumulate #'+/2
              0
              (mapper #'square/1
                      (filter #'odd?/1
                              (enumerate-tree tree)))))
```

Compare that to our first implementation, noting how much more clear the code with composition is:

```lisp
(defun sum-odd-squares
  (('())
    0)
  (((cons head tail))
    (+ (sum-odd-squares head)
       (sum-odd-squares tail)))
  ((elem)
    (if (odd?)
        (square elem)
        0)))
```


For ``even-fibs/1``, we enumerate the integers from 0 to $$n$$, generate the Fibonacci number for each of these integers, filter the resulting sequence to keep only the even elements, and accumulate the results into a list:

```lisp
(defun even-fibs (n)
  (accumulate #'cons/2
              '()
              (filter #'even?/1
                      (mapper #'fib/1
                              (enumerate-interval 0 n)))))
```

Again, compare with the original, where the contrast in clarity is even more stark than in the previous example: [^3]

```lisp
(defun even-fibs (n)
  (fletrec ((next (k)
              (if (> k n)
                  '()
                  (let ((f (fib k)))
                    (if (even? f)
                        (cons f (next (+ k 1)))
                        (next (+ k 1)))))))
    (next 0)))
```

The value of expressing programs as sequence operations is that this helps us make program designs that are modular, that is, designs that are constructed by combining relatively independent pieces. We can encourage modular design by providing a library of standard components together with a conventional interface for connecting the components in flexible ways.

Modular construction is a powerful strategy for controlling complexity in
engineering design. In real signal-processing applications, for example,
designers regularly build systems by cascading elements selected from
standardized families of filters and transducers. Similarly, sequence
operations provide a library of standard program elements that we can mix and
match. For instance, we can reuse pieces from the ``sum-odd-squares/1`` and
``even-fibs/1`` functions in a program that constructs a list of the squares of the first $$n + 1$$ Fibonacci numbers:

```lisp
(defun list-fib-squares (n)
  (accumulate #'cons/2
              '()
              (mapper #'square/1
                      (mapper #'fib/1
                              (enumerate-interval 0 n)))))
```
```lisp
> (list-fib-squares 10)
(0 1 1 4 9 25 64 169 441 1156 3025)
```

We can rearrange the pieces and use them in computing the product of the odd integers in a sequence:

```lisp
(defun product-of-squares-of-odd-elements (sequence)
  (accumulate #'*/2
              1
              (mapper #'square/1
                      (filter #'odd?/1 sequence))))
```
```lisp
> (product-of-squares-of-odd-elements (list 1 2 3 4 5))
225
```

We can also formulate conventional data-processing applications in terms of sequence operations. Suppose we have a sequence of personnel records and we want to find the salary of the highest-paid programmer. Assume that we have a selector ``salary`` that returns the salary of a record, and a predicate ``programmer?/1`` that tests if a record is for a programmer. Then we can write

```lisp
(defun salary-of-highest-paid-programmer (records)
  (accumulate #'max/2
              0
              (mapper #'salary/1
                      (filter #'programmer?/1 records))))
```

These examples give just a hint of the vast range of operations that can be expressed as sequence operations. [^4]

Sequences, implemented here as lists, serve as a conventional interface that permits us to combine processing modules. Additionally, when we uniformly represent structures as sequences, we have localized the data-structure dependencies in our programs to a small number of sequence operations. By changing these, we can experiment with alternative representations of sequences, while leaving the overall design of our programs intact. We will exploit this capability in the section [Streams](), when we generalize the sequence-processing paradigm to admit infinite sequences.

----

[^1]: In an actual program you would want to use the BIF (built-in function) ``lists:seq/2`` instead of creating your own less efficient version as is done here.

[^2]: This is, in fact, precisely the ``fringe`` function from exercise 2.28. Here we've renamed it to emphasize that it is part of a family of general sequence-manipulation functions.

[^3]: This importance of this type of refactoring is hard to over-emphasize in real-world applications. Long-term maintainability of large software projects is one of the greatest hidden costs in the industry, often providing the impetus for complete rewrites, and even greater wastes of time and resources. In the heat of solving a problem, it can be difficult to view the work clearly enough to think in terms of the patterns discussed here, but we must try; anything less is just endless patchwork which ultimately prevents us from truly practicing our professions. Finding bugs, adding features, enhancing reusability -- all of these things being done in a timely and efficient manner depend upon the engineer 1) clearly observing the underlying patterns in a given portion of code, and 2) making that code as composable as appropriate for the recognzied patterns and as is warranted by the given problem. As always, there is a balance to strike here: one must learn to distinguish *necessary elegance* from over-engineering and premature optimization. These are, however, skills that one gains over time.

[^4]: Richard Waters (1979) developed a program that automatically analyzes traditional Fortran programs, viewing them in terms of maps, filters, and accumulations. He found that fully 90 percent of the code in the Fortran Scientific Subroutine Package fits neatly into this paradigm. One of the reasons for the success of Lisp as a programming language is that lists provide a standard medium for expressing ordered collections so that they can be manipulated using higher-order operations. The programming language APL owes much of its power and appeal to a similar choice. In APL all data are represented as arrays, and there is a universal and convenient set of generic operators for all sorts of array operations.
