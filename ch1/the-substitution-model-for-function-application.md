### The Substitution Model for Function Application

To evaluate a combination whose operator names a compound function, the interpreter follows much the same process as for combinations whose operators name primitive functions, which we described in section [9.2.3](evaluating-combinations.html). That is, the interpreter evaluates the elements of the combination and applies the function (which is the value of the operator of the combination) to the arguments (which are the values of the operands of the combination).

We can assume that the mechanism for applying primitive functions to arguments is built into the interpreter. For compound functions, the application process is as follows:

* To apply a compound function to arguments, evaluate the body of the function with each formal parameter replaced by the corresponding argument. 

To illustrate this process, let's evaluate the combination

```lisp
(f 5)
```

where ``f`` is the function defined in section [9.2.4](compound-functions.html). We begin by retrieving the body of ``f``:

```lisp
(sum-of-squares (+ a 1) (* a 2))
```

Then we replace the formal parameter ``a`` by the argument ``5``:

```lisp
(sum-of-squares (+ 5 1) (* 5 2))
```

Thus the problem reduces to the evaluation of a combination with two operands and an operator ``sum-of-squares``. Evaluating this combination involves three subproblems. We must evaluate the operator to get the function to be applied, and we must evaluate the operands to get the arguments. Now ``(+ 5 1)`` produces 6 and ``(* 5 2)`` produces ``10``, so we must apply the ``sum-of-squares`` function to ``6`` and ``10``. These values are substituted for the formal parameters ``x`` and ``y`` in the body of ``sum-of-squares``, reducing the expression to

```lisp
(+ (square 6) (square 10))
```

If we use the definition of ``square``, this reduces to

```lisp
(+ (* 6 6) (* 10 10))
```

which reduces by multiplication to

```lisp
(+ 36 100)
```

and finally to

```lisp
136
```

The process we have just described is called the *substitution* model for function application. It can be taken as a model that determines the "meaning" of function application, insofar as the functions in this chapter are concerned. However, there are two points that should be stressed:

* The purpose of the substitution is to help us think about function application, not to provide a description of how the interpreter really works. Typical interpreters do not evaluate function applications by manipulating the text of a function to substitute values for the formal parameters. In practice, the "substitution" is accomplished by using a local environment for the formal parameters. We will discuss this more fully in chapters 11 and 12 when we examine the implementation of an interpreter in detail.
* Over the course of this book, we will present a sequence of increasingly elaborate models of how interpreters work, culminating with a complete implementation of an interpreter and compiler in chapter 13. The substitution model is only the first of these models -- a way to get started thinking formally about the evaluation process. In general, when modeling phenomena in science and engineering, we begin with simplified, incomplete models. As we examine things in greater detail, these simple models become inadequate and must be replaced by more refined models. The substitution model is no exception. In particular, when we address in chapter 3 the use of functions with "mutable data," we will see that the substitution model breaks down and must be replaced by a more complicated model of function application.[^1]

#### Applicative order versus normal order

According to the description of evaluation given in section [9.2.3](evaluating-combinations.html), the interpreter first evaluates the operator and operands and then applies the resulting function to the resulting arguments. This is not the only way to perform evaluation. An alternative evaluation model would not evaluate the operands until their values were needed. Instead it would first substitute operand expressions for parameters until it obtained an expression involving only primitive operators, and would then perform the evaluation. If we used this method, the evaluation of

```lisp
(f 5)
```

would proceed according to the sequence of expansions

```lisp
(sum-of-squares (+ 5 1) (* 5 2))

(+    (square (+ 5 1))      (square (* 5 2))  )

(+    (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))
```
followed by the reductions

```lisp
(+         (* 6 6)             (* 10 10))

(+           36                   100)

                    136
```

This gives the same answer as our previous evaluation model, but the process is different. In particular, the evaluations of ``(+ 5 1)`` and ``(* 5 2)`` are each performed twice here, corresponding to the reduction of the expression

```lisp
(* x x)
```

with ``x`` replaced respectively by ``(+ 5 1)`` and ``(* 5 2)``.

This alternative "fully expand and then reduce" evaluation method is known as *normal-order evaluation*, in contrast to the "evaluate the arguments and then apply" method that the interpreter actually uses, which is called *applicative-order evaluation*. It can be shown that, for function applications that can be modeled using substitution (including all the functions in the first two chapters of this book) and that yield legitimate values, normal-order and applicative-order evaluation produce the same value. (See exercise 1.5 for an instance of an "illegitimate" value where normal-order and applicative-order evaluation do not give the same result.)

LFE uses applicative-order evaluation, partly because of the additional efficiency obtained from avoiding multiple evaluations of expressions such as those illustrated with ``(+ 5 1)`` and ``(* 5 2)`` above and, more significantly, because normal-order evaluation becomes much more complicated to deal with when we leave the realm of functions that can be modeled by substitution. On the other hand, normal-order evaluation can be an extremely valuable tool, and we will investigate some of its implications in chapters 11 and 12.[^2]


----

[^1]: Despite the simplicity of the substitution idea, it turns out to be surprisingly complicated to give a rigorous mathematical definition of the substitution process. The problem arises from the possibility of confusion between the names used for the formal parameters of a function and the (possibly identical) names used in the expressions to which the function may be applied. Indeed, there is a long history of erroneous definitions of substitution in the literature of logic and programming semantics. See Stoy 1977 for a careful discussion of substitution. 
    
[^2]: In chapter 11 we will introduce stream processing, which is a way of handling apparently "infinite" data structures by incorporating a limited form of normal-order evaluation. In section 12.2 we will modify the LFE interpreter to produce a normal-order variant of LFE. 



