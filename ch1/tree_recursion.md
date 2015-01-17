### Tree Recursion

Another common pattern of computation is called *tree recursion*. As an example, consider computing the sequence of Fibonacci numbers, in which each number is the sum of the preceding two:

$$
0,\;1,\;1,\;2,\;3,\;5,\;8,\;13,\;21,\;34,\;55,\;89,\;144,\; \ldots\;
$$

In general, the Fibonacci numbers can be defined by the rule 
 
$$
Fib(n) =
\begin{cases}
0 & \mbox{if } n = 0 \\
1 & \mbox{if } n = 1  \\
Fib(n-1) + Fib(n-2) & \mbox{otherwise }
\end{cases}
$$

We can immediately translate this definition into a recursive procedure for computing Fibonacci numbers:
 
```lisp
(defun fib
  ((0) 0)
  ((1) 1)
  ((n)
    (+ (fib (- n 1))
       (fib (- n 2)))))
```

<a name="figure-5"></a>
![The tree-recursive process generated in computing (fib 5)](images/ch1-Z-G-13.png)

**Figure 1.5**:  The tree-recursive process generated in computing ``(fib 5)``.

Consider the pattern of this computation. To compute ``(fib 5)``, we compute ``(fib 4)`` and ``(fib 3)``. To compute ``(fib 4)``, we compute ``(fib 3)`` and ``(fib 2)``. In general, the evolved process looks like a tree, as shown in [figure 1.5](#figure-5). Notice that the branches split into two at each level (except at the bottom); this reflects the fact that the ``fib/1`` function calls itself twice each time it is invoked.

This procedure is instructive as a prototypical tree recursion, but it is a terrible way to compute Fibonacci numbers because it does so much redundant computation. Notice in [figure 1.5](#figure-5) that the entire computation of ``(fib 3)`` -- almost half the work -- is duplicated. In fact, it is not hard to show that the number of times the procedure will compute ``(fib 1)`` or ``(fib 0)`` (the number of leaves in the above tree, in general) is precisely $$Fib(n + 1)$$. To get an idea of how bad this is, one can show that the value of $$Fib(n)$$ grows exponentially with $$n$$. More precisely (see exercise 1.13), $$Fib(n)$$ is the closest integer to $$\frac{\phi^n}{\sqrt 5}$$, where

$$
\phi = \frac{1 + \sqrt 5}{2} \approx 1.61803
$$

is the golden ratio, which satisfies the equation

$$
\phi^2 = \phi + 1
$$

Thus, the process uses a number of steps that grows exponentially with the input. On the other hand, the space required grows only linearly with the input, because we need keep track only of which nodes are above us in the tree at any point in the computation. In general, the number of steps required by a tree-recursive process will be proportional to the number of nodes in the tree, while the space required will be proportional to the maximum depth of the tree.

We can also formulate an iterative process for computing the Fibonacci numbers. The idea is to use a pair of integers a and b, initialized to $$Fib(1) = 1$$ and $$Fib(0) = 0$$, and to repeatedly apply the simultaneous transformations 

$$a \gets a + b$$

$$b \gets a $$

It is not hard to show that, after applying this transformation $$n$$ times, $$a$$ and $$b$$ will be equal, respectively, to $$Fib(n + 1)$$ and $$Fib(n)$$. Thus, we can compute Fibonacci numbers iteratively using the procedure

```lisp
(defun fib
  ((n) (when (>= n 0))
    (fib n 0 1)))

(defun fib
  ((0 result _)
    result)
  ((n result next)
    (fib (- n 1) next (+ result next))))
```

This second method for computing $$Fib(n)$$ is a linear iteration. The difference in number of steps required by the two methods -- one linear in $$n$$, one growing as fast as $$Fib(n)$$ itself -- is enormous, even for small inputs.

One should not conclude from this that tree-recursive processes are useless. When we consider processes that operate on hierarchically structured data rather than numbers, we will find that tree recursion is a natural and powerful tool.[^1] But even in numerical operations, tree-recursive processes can be useful in helping us to understand and design programs. For instance, although the first ``fib`` function is much less efficient than the second one, it is more straightforward, being little more than a translation into Lisp of the definition of the Fibonacci sequence. To formulate the iterative algorithm required noticing that the computation could be recast as an iteration with three state variables.

#### Example: Counting change


----

[^1]: An example of this was hinted at in the section [Evaluating Combinations](): The interpreter itself evaluates expressions using a tree-recursive process. 





