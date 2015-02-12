### Example: Arithmetic Operations for Rational Numbers

Suppose we want to do arithmetic with rational numbers. We want to be able to add, subtract, multiply, and divide them and to test whether two rational numbers are equal.

Let us begin by assuming that we already have a way of constructing a rational number from a numerator and a denominator. We also assume that, given a rational number, we have a way of extracting (or selecting) its numerator and its denominator. Let us further assume that the constructor and selectors are available as functions:

* ``(make-rat <n> <d>)`` returns the rational number whose numerator is the integer ``<n>`` and whose denominator is the integer ``<d>``.
* ``(numer <x>)`` returns the numerator of the rational number ``<x>``.
* ``(denom <x>)`` returns the denominator of the rational number ``<x>``.

We are using here a powerful strategy of synthesis: *wishful thinking*. We haven't yet said how a rational number is represented, or how the functions ``numer/1``, ``denom/1``, and ``make-rat/2`` should be implemented. Even so, if we did have these three functions, we could then add, subtract, multiply, divide, and test equality by using the following relations:

$$
\begin{align}
\frac{n_1}{d_1} + \frac{n_2}{d_2} =
\frac{n_1 d_2 + n_2 d_1}{d_1 d_2}
\end{align}
$$

$$
\begin{align}
\frac{n_1}{d_1} - \frac{n_2}{d_2} =
\frac{n_1 d_2 - n_2 d_1}{d_1 d_2}
\end{align}
$$

$$
\begin{align}
\frac{n_1}{d_1} \cdot \frac{n_2}{d_2} =
\frac{n_1 n_2}{d_1 d_2}
\end{align}
$$

$$
\begin{align}
\frac{\frac{n_1}{d_1}}{\frac{n_2}{d_2}} =
\frac{n_1 d_2}{d_1 n_2}
\end{align}
$$

$$
\begin{align}
\frac{n_1}{d_1} = \frac{n_2}{d_2} \text{if and only if }
n_1 d_2 = n_2 d_1
\end{align}
$$

We can express these rules as functions:

```lisp
(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(defun equal-rat? (x y)
  (== (* (numer x) (denom y))
      (* (numer y) (denom x))))
```

Now we have the operations on rational numbers defined in terms of the selector and constructor functions ``numer/1``, ``denom/1``, and ``make-rat/2``. But we haven't yet defined these. What we need is some way to glue together a numerator and a denominator to form a rational number.

Now we have the operations on rational numbers defined in terms of the selector and constructor functions ``numer/1``, ``denom/1``, and ``make-rat/2``. But we haven't yet defined these. What we need is some way to glue together a numerator and a denominator to form a rational number.
