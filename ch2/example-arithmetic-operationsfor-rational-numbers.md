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
\frac{n_1}{d_1} = \frac{n_2}{d_2} \text{if and only if}
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

#### Pairs

To enable us to implement the concrete level of our data abstraction, our language provides a compound structure called a *pair*, which can be constructed with the primitive procedure ``cons/2``. This procedure takes two arguments and returns a compound data object that contains the two arguments as parts. Given a pair, we can extract the parts using the primitive procedures ``car/1`` and ``cdr/1``.[^1] Thus, we can use ``cons/2``, ``car/1``, and ``cdr/1`` as follows:

```lisp
> (set x (cons 1 2))
(1 . 2)
> (car x)
1
> (cdr x)
2
```

Notice that a pair is a data object that can be given a name and manipulated, just like a primitive data object. Moreover, ``cons/2`` can be used to form pairs whose elements are pairs, and so on:

```lisp
> (set x (cons 1 2))
(1 . 2)
> (set y (cons 3 4))
(3 . 4)
> (set z (cons x y))
((1 . 2) 3 . 4)
> (car (car z))
1
> (car (cdr z))
3
```

In the section [Hierarchical Data and the Closure Property]() we will see how this ability to combine pairs means that pairs can be used as general-purpose building blocks to create all sorts of complex data structures. The single compound-data primitive *pair*, implemented by the procedures ``cons/2``, ``car/1``, and ``cdr/1``, is the only glue we need. Data objects constructed from pairs are called *list-structured* data.

----

[^1]: The name ``cons`` stands for "construct." The names ``car`` and ``cdr`` derive from the original implementation of Lisp on the IBM 704. That machine had an addressing scheme that allowed one to reference the "address" and "decrement" parts of a memory location. ``car`` stands for "Contents of Address part of Register" and ``cdr`` (pronounced "could-er" or "cudder") stands for "Contents of Decrement part of Register."
