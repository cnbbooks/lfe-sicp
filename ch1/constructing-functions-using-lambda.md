### Constructing Functions Using Lambda

In using ``pi-sum/2`` as in the section [Functions as Arguments](), it seems terribly awkward to have to define trivial ``flet`` functions such as ``pi-term/1`` and ``pi-next/1`` just so we can use them as arguments to our higher-order function. Rather than define ``pi-next/1`` and ``pi-term/1``, it would be more convenient to have a way to directly specify "the function that returns its input incremented by 4" and "the function that returns the reciprocal of its input times its input plus 2." We can do this by introducing the special form ``lambda``, which creates functions. Using lambda we can describe what we want as

```lisp
(lambda (x) (+ x 4))
```

and

```lisp
(lambda (x) (/ 1.0 (* x (+ x 2))))
```

Then our ``pi-sum/2`` function can be expressed (without defining any auxiliary functions in an ``flet``) as

```lisp
(defun pi-sum (a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))
```

Again using ``lambda``, we can write the ``integral/4`` function without having to define the auxiliary function ``add-dx/1``:

```lisp
(defun integral (f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))
```

In general, lambda is used to create functions in the same way as ``defun``, except that no name is specified for the function:

```lisp
(lambda (<formal-parameters>) <body>)
```

The resulting function is just as much a function as one that is created using define. The only difference is that it has not been associated with any name in the environment.

We can read a lambda expression as follows:

```lisp
   (lambda             (x)            (+     x       4))
      ^                 ^              ^     ^       ^
      |                 |              |     |       |
 the function   of an argument x  that adds  x  and  4
```

Like any expression that has a function as its value, a ``lambda`` expression can be used as the operator in a combination such as

```lisp
> (apply (lambda (x y z) (+ x y (square z))) '(1 2 3))
12
```

or, more generally, in any context where we would normally use a function name.[^1] For example, if we defined the ``lambda`` expression above as a function

```lisp
(defun add-sq (x y z)
  (+ x y (square z)))
```

we would apply it the same way as we did the ``lambda`` expression:

```lisp
> (apply #'add-sq/3 '(1 2 3))
12
```

#### Using ``let`` to create local variables

Another use of ``lambda`` is in creating local variables. We often need local variables in our functions other than those that have been bound as formal parameters. For example, suppose we wish to compute the function

$$
\begin{align}
f(x, y) = x(1 + xy)^2 + y(1 -y) + (1 + xy)(1 - y)
\end{align}
$$

which we could also express as

$$
\begin{align}
a = & \ 1 + xy \\
b = & \ 1 - y \\
f(x, y) = & \ ra^2 + yb + ab.
\end{align}
$$

In writing a function to compute $$f$$, we would like to include as local variables not only $$x$$ and $$y$$ but also the names of intermediate quantities like $$a$$ and $$b$$. One way to accomplish this is to use an auxiliary function to bind the local variables:

```lisp
(defun f (x y)
  (flet ((f-helper (a b)
          (+ (* x (square a))
             (* y b)
             (* a b))))
    (f-helper (+ 1 (* x y))
              (- 1 y))))
```

Of course, we could use a ``lambda`` expression to specify an anonymous function for binding our local variables. The body of ``f`` then becomes a single call to that function:

```lisp
(defun f (x y)
  (funcall
    (lambda (a b)
      (+ (* x (square a))
         (* y b)
         (* a b)))
    (+ 1 (* x y))
    (- 1 y)))
```

This construct is so useful that there is a special form called let to make its use more convenient. Using let, the f function could be written as

```lisp
(defun f (x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
```

 The general form of a ``let`` expression is

```lisp
(let ((<var-1> <exp-1>)
      (<var-2> <exp-2>)
      ...
      (<var-n> <exp-n>))
   <body>)
```

which can be thought of as saying

* Let ``<var-1>`` have the value ``<exp-1>`` and
* Let ``<var-2>`` have the value ``<exp-2>`` and
* ... and
* Let ``<var-n>`` have the value ``<exp-n>``
* All in the context of ``<body>``

The first part of the ``let`` expression is a list of name-expression pairs. When the ``let`` is evaluated, each name is associated with the value of the corresponding expression. The body of the ``let`` is evaluated with these names bound as local variables. The way this happens is that the ``let`` expression is interpreted as an alternate syntax for

```lisp
(funcall
  (lambda (<var-1> ... <var-n>)
    <body>)
   <exp-1>
   ...
   <exp-n>)
```

No new mechanism is required in the interpreter in order to provide local variables. A ``let`` expression is simply syntactic sugar for the underlying ``lambda`` application.

We can see from this equivalence that the scope of a variable specified by a ``let`` expression is the body of the ``let``. This implies that:

* ``let`` allows one to bind variables as locally as possible to where they are to be used. For example, if the value of ``x`` is 5, the value of the expression
  ```lisp
  (+ (let ((x 3))
     (+ x (* x 10)))
   x)
  ```
  is 38. Here, the ``x`` in the body of the ``let`` is 3, so the value of the let expression is 33. On the other hand, the ``x`` that is the second argument to the outermost ``+`` is still 5.

* The variables' values are computed outside the ``let``. This matters when the expressions that provide the values for the local variables depend upon variables having the same names as the local variables themselves. For example, if the value of ``x`` is 2, the expression
  ```lisp
  (let ((x 3)
      (y (+ x 2)))
  (* x y))
  ```
  will have the value 12 because, inside the body of the ``let``, ``x`` will be 3 and ``y`` will be 4 (which is the outer ``x`` plus 2).

----

[^1]: It would be clearer and less intimidating to people learning Lisp if a name more obvious than ``lambda``, such as ``make-function``, were used. But the convention is firmly entrenched. The notation is adopted from the $$\lambda$$ calculus, a mathematical formalism introduced by the mathematical logician Alonzo Church (1941). Church developed the $$\lambda$$ calculus to provide a rigorous foundation for studying the notions of function and function application. The $$\lambda$$ calculus has become a basic tool for mathematical investigations of the semantics of programming languages.
