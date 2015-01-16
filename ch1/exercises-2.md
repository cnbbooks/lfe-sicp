### Exercises

#### Exercise 1.6

Bob Bitwright doesn't see why ``if`` needs to be provided as a special form. "Why can't I just define it as an ordinary function in terms of ``cond``?" he asks. Bob's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of ``if``:

```lisp
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        ('true else-clause)))
```

Eva demonstrates the program for Bob:

```lisp
> (new-if (== 2 3) 0 5)
5
> (new-if (== 1 1) 0 5)
0
```

Delighted, Bob uses ``new-if/3`` to rewrite the square-root program:

```lisp
(defun sqrt (guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt (improve guess x)
                     x)))
```

What happens when Alyssa attempts to use this to compute square roots? Explain. 

#### Exercise 1.7.

The ``good-enough?/2`` test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing ``good-enough?/2`` is to watch how ``guess`` changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root function that uses this kind of end test. Does this work better for small and large numbers? 

#### Exercise 1.8.

Newton's method for cube roots is based on the fact that if $$y$$ is an approximation to the cube root of $$x$$, then a better approximation is given by the value

$$
\begin{align}
\frac{\frac{x}{y^2}+2y}
{3}
\end{align}
$$

Use this formula to implement a cube-root function analogous to the square-root function. (In section 9.4.4 we will see how to implement Newton's method in general as an abstraction of these square-root and cube-root functions.) 


