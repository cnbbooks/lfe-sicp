### Exercises

#### Exercise 1.29

Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as

$$
\begin{align}
\frac{h}{3}
\left[
y_0 + 4y_1 + 2y_2 + 4y_3 + \dots + 2y_{n-2} + 4y_{n-1} + y_n
\right]
\end{align}
$$

where $$h = \frac{b - a}{n}$$ for some integer $$n$$, and $$y_k = f(a + kh)$$.
Increasing $$n$$ increases the accuracy of the approximation.) Define a procedure that takes as arguments $$f$$, $$a$$, $$b$$, and $$n$$ and returns the value of the integral, computed using Simpson's Rule. Use your function to integrate ``cube/4`` between 0 and 1 (with $$n $$= 100 and $$n$$ = 1000), and compare the results to those of the ``integral/4`` function shown in the previous section.

#### Exercise 1.30

The ``sum/4`` procedure from the previous section generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:

```lisp
(defun (sum term a next b)
  (flet ((iter (a result)
    (if <??>
        <??>
        (iter <??> <??>))))
    (iter <??> <??>)))
```

#### Exercise 1.31

#### Exercise 1.32

#### Exercise 1.33
