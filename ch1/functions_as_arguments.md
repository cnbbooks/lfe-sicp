### Functions as Arguments

Consider the following three functions. The first computes the sum of the integers from a through b:

```lisp
(defun sum-integers (a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
```

The second computes the sum of the cubes of the integers in the given range:

```lisp
(defun sum-cubes (a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
```

The third computes the sum of a sequence of terms in the series

$$
\begin{align}
\frac{1}{1 \cdot 3} + \frac{1}{5 \cdot 7} + \frac{1}{9 \cdot 11} + \dots
\end{align}
$$

which converges to $$\frac{\pi}{8}$$ (very slowly):[^1]

```lisp
(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
```

These three functions clearly share a common underlying pattern. They are for the most part identical, differing only in the name of the function, the function of ``a`` used to compute the term to be added, and the function that provides the next value of ``a``. We could generate each of the functions by filling in slots in the same template:

```lisp
(defun <name> (a b)
  (if (> a b)
      0
      (+ (<term> a)
         (<name> (<next> a) b))))
```

The presence of such a common pattern is strong evidence that there is a useful abstraction waiting to be brought to the surface. Indeed, mathematicians long ago identified the abstraction of summation of a series and invented "sigma notation," for example

$$
\begin{align}
\sum_{n=a}^{b} f(n) = f(a) + \dots + f(b)
\end{align}
$$

to express this concept. The power of sigma notation is that it allows mathematicians to deal with the concept of summation itself rather than only with particular sums -- for example, to formulate general results about sums that are independent of the particular series being summed.

Similarly, as program designers, we would like our language to be powerful enough so that we can write a function that expresses the concept of summation itself rather than only functions that compute particular sums. We can do so readily in our procedural language by taking the common template shown above and transforming the "slots" into formal parameters:

```lisp
(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))
```

Notice that ``sum/4`` takes as its arguments the lower and upper bounds ``a`` and ``b`` together with the functions ``term`` and ``next``. We can use ``sum/4`` just as we would any function. For example, we can use it (along with a function ``inc/1`` that increments its argument by 1) to define ``sum-cubes/4``:

```lisp
(defun inc (n) (+ n 1))

(defun sum-cubes (a b)
  (sum #'cube/1 a #'inc/1 b))
```

Using this, we can compute the sum of the cubes of the integers from 1 to 10:

```lisp
> (sum-cubes 1 10)
3025
```

With the aid of an identity function to compute the term, we can define ``sum-integers`` in terms of ``sum``:

```lisp
(defun identity (x) x)

(defun sum-integers (a b)
  (sum #'identity/1 a #'inc/1 b))
```

 Then we can add up the integers from 1 to 10:

```lisp
> (sum-integers 1 10)
55
```

We can also define pi-sum in the same way:[^2]

```lisp
(defun pi-sum (a b)
  (flet ((pi-term (x)
           (/ 1.0 (* x (+ x 2))))
         (pi-next (x)
           (+ x 4)))
    (sum #'pi-term/1 a #'pi-next/1 b)))
```

Using these functions, we can compute an approximation to $$\pi$$:

```lisp
> (* 8 (pi-sum 1 100000))
3.141572653589795
```

Once we have ``sum/4``, we can use it as a building block in formulating further concepts. For instance, the definite integral of a function $$f$$ between the limits $$a$$ and $$b$$ can be approximated numerically using the formula

$$
\begin{align}
\int_a^b f = \left[
f\left(a + \frac{dx}{2} \right) +
f\left(a + dx + \frac{dx}{2} \right) +
f\left(a + 2dx + \frac{dx}{2} \right) + \dots
\right] dx
\end{align}
$$

for small values of $$dx$$. We can express this directly as a function:

```lisp
(defun integral (f a b dx)
  (flet ((add-dx (x)
           (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) #'add-dx/1 b)
       dx)))
```

```lisp
> (integral #'cube/1 0 1 0.01)
0.24998750000000042
> (integral #'cube/1 0 1 0.001)
0.249999875000001
> (integral #'cube/1 0 1 0.0001)
0.24999999874993412
```

(The exact value of the integral of cube between 0 and 1 is 1/4.)

----

[^1]: This series, usually written in the equivalent form $$\frac{pi}{4} = 1 - \frac{1}{3} + \frac{1}{5} - \frac{1}{7} + \dots $$, is due to Leibniz. We'll see how to use this as the basis for some fancy numerical tricks in the section [Exploiting the Stream Paradigm]().

[^2]: Notice that we have used ``flet``s (from the section [Functions as Black-Box Abstractions]()) to embed the definitions of ``pi-next/1`` and ``pi-term/1`` within ``pi-sum/2``, since these functions are unlikely to be useful for any other purpose. We will learn more about how these work when we get to the section [Constructing Functions Using Lambda]().




