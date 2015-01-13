### The Substitution Model for Procedure Application

To evaluate a combination whose operator names a compound procedure, the interpreter follows much the same process as for combinations whose operators name primitive procedures, which we described in section [9.2.3](evaluating-combinations.html). That is, the interpreter evaluates the elements of the combination and applies the procedure (which is the value of the operator of the combination) to the arguments (which are the values of the operands of the combination).

We can assume that the mechanism for applying primitive procedures to arguments is built into the interpreter. For compound procedures, the application process is as follows:

* To apply a compound procedure to arguments, evaluate the body of the procedure with each formal parameter replaced by the corresponding argument. 

To illustrate this process, let's evaluate the combination

```lisp
(f 5)
```

where ``f`` is the procedure defined in section [9.2.4](compound-procedures.html). We begin by retrieving the body of ``f``:

```lisp
(sum-of-squares (+ a 1) (* a 2))
```

Then we replace the formal parameter ``a`` by the argument ``5``:

```lisp
(sum-of-squares (+ 5 1) (* 5 2))
```

Thus the problem reduces to the evaluation of a combination with two operands and an operator ``sum-of-squares``. Evaluating this combination involves three subproblems. We must evaluate the operator to get the procedure to be applied, and we must evaluate the operands to get the arguments. Now ``(+ 5 1)`` produces 6 and ``(* 5 2)`` produces ``10``, so we must apply the ``sum-of-squares`` procedure to ``6`` and ``10``. These values are substituted for the formal parameters ``x`` and ``y`` in the body of ``sum-of-squares``, reducing the expression to

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

 The process we have just described is called the substitution model for procedure application. It can be taken as a model that determines the ``meaning'' of procedure application, insofar as the procedures in this chapter are concerned. However, there are two points that should be stressed:

    The purpose of the substitution is to help us think about procedure application, not to provide a description of how the interpreter really works. Typical interpreters do not evaluate procedure applications by manipulating the text of a procedure to substitute values for the formal parameters. In practice, the ``substitution'' is accomplished by using a local environment for the formal parameters. We will discuss this more fully in chapters 3 and 4 when we examine the implementation of an interpreter in detail.

    Over the course of this book, we will present a sequence of increasingly elaborate models of how interpreters work, culminating with a complete implementation of an interpreter and compiler in chapter 5. The substitution model is only the first of these models -- a way to get started thinking formally about the evaluation process. In general, when modeling phenomena in science and engineering, we begin with simplified, incomplete models. As we examine things in greater detail, these simple models become inadequate and must be replaced by more refined models. The substitution model is no exception. In particular, when we address in chapter 3 the use of procedures with ``mutable data,'' we will see that the substitution model breaks down and must be replaced by a more complicated model of procedure application.15 
    
    
    
    



