### Sequences as Conventional Interfaces

In working with compound data, we've stressed how data abstraction permits us to design programs without becoming enmeshed in the details of data representations, and how abstraction preserves for us the flexibility to experiment with alternative representations. In this section, we introduce another powerful design principle for working with data structures -- the use of *conventional interfaces*.

In the section [Formulating Abstractions with Higher-Order Functions]() we saw how program abstractions, implemented as higher-order functions, can capture common patterns in programs that deal with numerical data. Our ability to formulate analogous operations for working with compound data depends crucially on the style in which we manipulate our data structures. Consider, for example, the following function, analogous to the ``count-leaves/2`` function of the section [Hierarchical Structures](), which takes a tree as argument and computes the sum of the squares of the leaves that are odd:

```lisp
(defun sum-odd-squares
  (('()) 0)
  (((cons head tail))
    (+ (sum-odd-squares head)
       (sum-odd-squares tail)))
  ((elem)
    (if (odd?)
        (square elem)
        0)))
```

On the surface, this procedure is very different from the following one, which constructs a list of all the even Fibonacci numbers $$Fib(k)$$, where $$k$$ is less than or equal to a given integer $$n$$:

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

[to be continued ...]