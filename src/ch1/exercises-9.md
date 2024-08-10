### Exercises

#### Exercise 1.29

Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as

\\[
\begin{align}
\frac{h}{3}
\left[
y_0 + 4y_1 + 2y_2 + 4y_3 + \dots + 2y_{n-2} + 4y_{n-1} + y_n
\right]
\end{align}
\\]

where \\(h = \frac{b - a}{n}\\) for some integer \\(n\\), and \\(y_k = f(a + kh)\\).
Increasing \\(n\\) increases the accuracy of the approximation.) Define a function that takes as arguments \\(f\\), \\(a\\), \\(b\\), and \\(n\\) and returns the value of the integral, computed using Simpson's Rule. Use your function to integrate ``cube/4`` between 0 and 1 (with \\(n = 100\\) and \\(n = 1000\\)), and compare the results to those of the ``integral/4`` function shown in the previous section.

#### Exercise 1.30

The ``sum/4`` function from the previous section generates a linear recursion. The function can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:

```lisp
(defun sum (term a next b)
  (flet ((iter (a result)
    (if <??>
        <??>
        (iter <??> <??>))))
    (iter <??> <??>)))
```

#### Exercise 1.31

a.  The ``sum/4`` function is only the simplest of a vast number of similar abstractions that can be captured as higher-order functions.[^1] Write an analogous function called ``product`` that returns the product of the values of a function at points over a given range. Show how to define ``factorial`` in terms of ``product``. Also use ``product`` to compute approximations to \\(\pi\\) using the formula [^2]

\\[
\begin{align}
\frac{\pi}{4} =
\frac{ 2 \cdot 4 \cdot 4 \cdot 6 \cdot 6 \cdot 8 \dots}
     { 3 \cdot 3 \cdot 5 \cdot 5 \cdot 7 \cdot 7 \dots}
\end{align}
\\]

b. If your ``product`` function generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

#### Exercise 1.32

a. Show that ``sum/4`` and ``product`` (exercise 1.31) are both special cases of a still more general notion called ``accumulate/6`` that combines a collection of terms, using some general accumulation function:

```lisp
(accumulate combiner null-value term a next b)
```

``accumulate/6`` takes as arguments the same term and range specifications as sum and ``product``, together with a ``combiner`` function (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a ``null-value`` that specifies what base value to use when the terms run out. Write ``accumulate/6`` and show how ``sum`` and ``product`` can both be defined as simple calls to ``accumulate/6``.

b. If your ``accumulate/6`` function generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

#### Exercise 1.33

You can obtain an even more general version of ``accumulate/6`` (exercise 1.32) by introducing the notion of a *filter* on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting ``filtered-accumulate/7`` abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write ``filtered-accumulate/7`` as a function. Show how to express the following using ``filtered-accumulate/7``:

a. the sum of the squares of the prime numbers in the interval \\(a\\) to \\(b\\) (assuming that you have a ``prime?/1`` predicate already written)

b. the product of all the positive integers less than \\(n\\) that are relatively prime to \\(n\\) (i.e., all positive integers \\(i < n\\) such that \\(\text{GCD}(i,n) = 1\\)).


----

[^1]: The intent of exercises 1.31, 1.32, and 1.33 is to demonstrate the expressive power that is attained by using an appropriate abstraction to consolidate many seemingly disparate operations. However, though accumulation and filtering are elegant ideas, our hands are somewhat tied in using them at this point since we do not yet have data structures to provide suitable means of combination for these abstractions. We will return to these ideas in the section [equences as Conventional Interfaces]() when we show how to use *sequences* as interfaces for combining filters and accumulators to build even more powerful abstractions. We will see there how these methods really come into their own as a powerful and elegant approach to designing programs.

[^2]: This formula was discovered by the seventeenth-century English mathematician John Wallis.
