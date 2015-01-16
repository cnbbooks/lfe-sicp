### Procedures as Black-Box Abstractions

``sqrt/1`` is our first example of a process defined by a set of mutually defined procedures. Notice that the definition of ``sqrt/2`` is recursive; that is, the procedure is defined in terms of itself. The idea of being able to define a procedure in terms of itself may be disturbing; it may seem unclear how such a "circular" definition could make sense at all, much less specify a well-defined process to be carried out by a computer. This will be addressed more carefully in section 9.3. But first let's consider some other important points illustrated by the ``sqrt`` example.

Observe that the problem of computing square roots breaks up naturally into a number of subproblems: how to tell whether a guess is good enough, how to improve a guess, and so on. Each of these tasks is accomplished by a separate procedure. The entire ``sqrt`` program can be viewed as a cluster of procedures (shown in [figure 1.2](#figure-2)) that mirrors the decomposition of the problem into subproblems.

<a name="figure-2"></a>
![Tree representation](images/ch1-Z-G-6.png)

**Figure 1.2**:  Procedural decomposition of the ``sqrt`` program.

The importance of this decomposition strategy is not simply that one is dividing the program into parts. After all, we could take any large program and divide it into parts -- the first ten lines, the next ten lines, the next ten lines, and so on. Rather, it is crucial that each procedure accomplishes an identifiable task that can be used as a module in defining other procedures. For example, when we define the ``good-enough?/2`` procedure in terms of ``square/1``, we are able to regard the ``square/1`` procedure as a "black box." We are not at that moment concerned with how the procedure computes its result, only with the fact that it computes the square. The details of how the square is computed can be suppressed, to be considered at a later time. Indeed, as far as the ``good-enough?/2`` procedure is concerned, ``square/1`` is not quite a procedure but rather an abstraction of a procedure, a so-called procedural abstraction. At this level of abstraction, any procedure that computes the square is equally good.

Thus, considering only the values they return, the following two procedures for squaring a number should be indistinguishable. Each takes a numerical argument and produces the square of that number as the value.[^1]

```lisp
(defun square (x) (* x x))

(defun square (x) 
  (exp (double (log x))))

(defun double (x) (+ x x))
```

So a procedure definition should be able to suppress detail. The users of the procedure may not have written the procedure themselves, but may have obtained it from another programmer as a black box. A user should not need to know how the procedure is implemented in order to use it.

#### Local names

One detail of a procedure's implementation that should not matter to the user of the procedure is the implementer's choice of names for the procedure's formal parameters. Thus, the following procedures should not be distinguishable:

```lisp
(defun square (x) (* x x))

(defun square (y) (* y y))
```

This principle -- that the meaning of a procedure should be independent of the parameter names used by its author -- seems on the surface to be self-evident, but its consequences are profound. The simplest consequence is that the parameter names of a procedure must be local to the body of the procedure. For example, we used ``square/1`` in the definition of ``good-enough?/2`` in our square-root procedure:

```lisp
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
```

The intention of the author of ``good-enough?/2`` is to determine if the square of the first argument is within a given tolerance of the second argument. We see that the author of ``good-enough?/2`` used the name ``guess`` to refer to the first argument and ``x`` to refer to the second argument. The argument of ``square/1`` is ``guess``. If the author of ``square/1`` used ``x`` (as above) to refer to that argument, we see that the x in ``good-enough?/2`` must be a different ``x`` than the one in square. Running the procedure square must not affect the value of ``x`` that is used by ``good-enough?/2``, because that value of ``x`` may be needed by ``good-enough?/2`` after ``square/1`` is done computing.

If the parameters were not local to the bodies of their respective procedures, then the parameter ``x`` in ``square/1`` could be confused with the parameter ``x`` in ``good-enough?/2``, and the behavior of ``good-enough?/2`` would depend upon which version of ``square/1`` we used. Thus, ``square/1`` would not be the black box we desired.

A formal parameter of a procedure has a very special role in the procedure definition, in that it doesn't matter what name the formal parameter has. Such a name is called a *bound variable*, and we say that the procedure definition *binds* its formal parameters. The meaning of a procedure definition is unchanged if a bound variable is consistently renamed throughout the definition.[^2] If a variable is not bound, we say that it is *free*. The set of expressions for which a binding defines a name is called the *scope* of that name. In a procedure definition, the bound variables declared as the formal parameters of the procedure have the body of the procedure as their scope.

In the definition of ``good-enough?/2`` above, ``guess`` and ``x`` are bound variables but ``<``, ``-``, ``abs``, and ``square/1`` are free. The meaning of ``good-enough?/2`` should be independent of the names we choose for ``guess`` and ``x`` so long as they are distinct and different from ``<``, ``-``, ``abs``, and ``square``. (If we were running this code in the LFE REPL and renamed ``guess`` to a variable that had been ``set`` earlier in the session, we would have introduced a bug by capturing that previously set variable. It would have changed from free to bound.) The meaning of ``good-enough?/2`` is not independent of the names of its free variables, however. It surely depends upon the fact (external to this definition) that the symbol ``abs`` names a procedure for computing the absolute value of a number. ``good-enough?/2`` will compute a different function if we substitute ``cos`` for ``abs`` in its definition.

#### Internal definitions and block structure

```lisp

(defun sqrt (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x)
                 x)))
(defun abs
  ((x) (when (< x 0)) (- x))
  ((x) x))
(defun square (x) (* x x))
```

#### Modules, exports, and private functions

----

[^1]: It is not even clear which of these procedures is a more efficient implementation. This depends upon the hardware available. There are machines for which the "obvious" implementation is the less efficient one. Consider a machine that has extensive tables of logarithms and antilogarithms stored in a very efficient manner. 

[^2]: The concept of consistent renaming is actually subtle and difficult to define formally. Famous logicians have made embarrassing errors here. 






