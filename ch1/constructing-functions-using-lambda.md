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
   (lambda             (x)              (+    x     4))
      ^                 ^                ^    ^     ^
      |                 |                |    |     |
 the function   of an argument x  that adds  x and 4
```

Like any expression that has a function as its value, a ``lambda`` expression can be used as the operator in a combination such as

```lisp
> (apply (lambda (x y z) (+ x y (square z))) '(1 2 3))
12
```

or, more generally, in any context where we would normally use a procedure name.[^1] For example, if we defined the ``lambda`` expression above as a function

```lisp
(defun add-sq (x y z)
  (+ x y (square z)))
```

We would apply it the same way as we did the ``lambda`` expression:

```lisp
> (apply #'add-sq/3 '(1 2 3))
12
```


----

[^1]: It would be clearer and less intimidating to people learning Lisp if a name more obvious than ``lambda``, such as ``make-procedure``, were used. But the convention is firmly entrenched. The notation is adopted from the $$\lambda$$ calculus, a mathematical formalism introduced by the mathematical logician Alonzo Church (1941). Church developed the $$\lambda$$ calculus to provide a rigorous foundation for studying the notions of function and function application. The $$\lambda$$ calculus has become a basic tool for mathematical investigations of the semantics of programming languages.
