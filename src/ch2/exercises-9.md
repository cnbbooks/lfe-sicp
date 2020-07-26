### Exercises

#### Exercise 2.33

Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

```lisp
(defun map (p seq)
  (accumulate (lambda (x y) <??>) '() seq))

(defun append (seq1 seq2)
  (accumulate #'cons/2 <??> <??>))

(defun length (seq)
  (accumulate <??> 0 seq))
```

#### Exercise 2.34

Evaluating a polynomial in $$x$$ at a given value of $$x$$ can be formulated as an accumulation. We evaluate the polynomial

$$
\begin{align}
a_n x^n + a_{n - 1} x^{n -1} + \cdots + a_1 x + a_0
\end{align}
$$

using a well-known algorithm called *Horner's rule*, which structures the computation as

$$
\begin{align}
( \cdots (a_n x + a_{n -1})x + \cdots + a_1)x + a_0
\end{align}
$$

In other words, we start with $$a_n$$, multiply by $$x$$, add $$a_{n-1}$$, multiply by $$x$$, and so on, until we reach $$a_0$$.[^1] Fill in the following template to produce a procedure that evaluates a polynomial using Horner's rule. Assume that the coefficients of the polynomial are arranged in a sequence, from $$a_0$$ through $$a_n$$.

```lisp
(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) <??>)
              0
              coefficient-sequence))
```

For example, to compute $$1 + 3x + 5x^3 + x^5$$ at $$x = 2$$ you would evaluate

```lisp
(horner-eval 2 (list 1 3 0 5 0 1))
```

#### Exercise 2.35

Redefine count-leaves from the section [Hierarchical Structures]() as an accumulation:

```lisp
(defun count-leaves (t)
  (accumulate <??> <??> (mapper <??> <??>)))
```

#### Exercise 2.36

The procedure ``accumulate-n/3`` is similar to ``accumulate/3`` except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results. For instance, if s is a sequence containing four sequences, ``((1 2 3) (4 5 6) (7 8 9) (10 11 12))``, then the value of ``(accumulate-n + 0 s)`` should be the sequence ``(22 26 30)``. Fill in the missing expressions in the following definition of ``accumulate-n/3``:

```lisp
(defun accumulate-n
  ((_ _ (cons '() _))
   '())
  ((op init seqs)
   (cons (accumulate op init <??>)
         (accumulate-n op init <??>))))
```

#### Exercise 2.37

Suppose we represent vectors $$v = (v_i)$$ as sequences of numbers, and matrices $$m = (m_{ij})$$ as sequences of vectors (the rows of the matrix). For example, the matrix

$$
\begin{bmatrix}
1 & 2 & 3 & 4 \\
4 & 5 & 6 & 6 \\
6 & 7 & 8 & 9
\end{bmatrix}
$$

is represented as the sequence ``((1 2 3 4) (4 5 6 6) (6 7 8 9))``. With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:

* ``(dot-product v w)`` - returns the sum $$\Sigma_i v_i w_i$$
* ``(matrix-*-vector m v)`` - returns the vector of $$t$$ where $$t_i = \Sigma_j m_{ij} v_j$$
* ``(matrix-*-matrix m n)`` - returns the matrix $$p$$ where $$p_{ij} = \Sigma_k m_{ik} n_{kj}$$
* ``(transpose m)`` - returns the matrix $$n$$, where $$n_{ij} = m_{ji}$$ 

We can define the dot product as [^2]

```lisp
(defun dot-product (v w)
  (accumulate #'+/2 0 (mapper-n #'* v w)))
```

where ``mapper2/2`` takes a function and a list of lists where the arity of the function is the same number as the number of inner lists. This is because the first element of each list is passed to the function, then the second element of each list is passed, and so on, returning a list of the results. For example:

```lisp
> (mapper-n #'+/3
            (list (list 1 2 3)
                  (list 40 50 60)
                  (list 700 800 900))
(741 852 963)
```
or
```lisp
> (mapper-n (lambda (x y) (+ x (* 2 y)))
            (list (list 1 2 3)
                  (list 4 5 6)))
(9 12 15)
```

Write ``mapper-n/2`` and then fill in the missing expressions in the following functions for computing the other matrix operations. (The function ``accumulate-n/3`` is defined in exercise 2.36.)

```lisp
(defun matrix-*-vector (m v)
  (mapper2 <??> m))

(defun transpose (mat)
  (accumulate-n <??> <??> mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapper2 <??> m)))
```


#### Exercise 2.38

The ``accumulate/3`` function is also known as ``fold-right/3``[^2], because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a ``fold-left/3``[^3], which is similar to ``fold-right/3``, except that it combines elements working in the opposite direction:

```lisp
(defun fold-left
  ((_ result '())
   result)
  ((op result (cons head tail))
   (fold-left op
              (funcall op result head)
              tail)))
```
What are the values of

```lisp
(fold-right #'//2 1 (list 1 2 3))
```
```lisp
(fold-left #'//2 1 (list 1 2 3))
```
```lisp
(fold-right #'list/2 '() (list 1 2 3))
```
```lisp
(fold-left #'list/2 '() (list 1 2 3))
```

Give a property that ``op`` should satisfy to guarantee that ``fold-right/3`` and ``fold-left/3`` will produce the same values for any sequence.

#### Exercise 2.39

Complete the following definitions of reverse (exercise 2.18) in terms of ``fold-right/3`` and ``fold-left/3`` from exercise 2.38:

```lisp
(defun reverse (sequence)
  (fold-right (lambda (x y) <??>) '() sequence))
  
(defun reverse (sequence)
  (fold-left (lambda (x y) <??>) '() sequence))
```
----

[^1]: According to Knuth (1981), this rule was formulated by W. G. Horner early in the nineteenth century, but the method was actually used by Newton over a hundred years earlier. Horner's rule evaluates the polynomial using fewer additions and multiplications than does the straightforward method of first computing an $$x^n$$, then adding $$a_{n-1} x^{n-1}$$, and so on. In fact, it is possible to prove that any algorithm for evaluating arbitrary polynomials must use at least as many additions and multiplications as does Horner's rule, and thus Horner's rule is an optimal algorithm for polynomial evaluation. This was proved (for the number of additions) by A. M. Ostrowski in his 1954 paper "On Two Problems in Abstract Algebra Connected with Horner's rule" that essentially founded the modern study of optimal algorithms. (For an overview of Ostrowski's life and work, see [Walter Gautschi's paper](https://www.cs.purdue.edu/homes/wxg/AMOengl.pdf).) The analogous statement for multiplications was proved by V. Y. Pan in 1966. The book by Borodin and Munro (1975) provides an overview of these and other results about optimal algorithms.

[^2]: This is available in the Erlang standard library as ``lists:foldr/3``.

[^3]: This is available in the Erlang standard library as ``lists:foldl/3``. Note that ``foldl`` is more commonly used, in general, than ``foldr`` and it has the added benefit of being tail recursive in Erlang/LFE.