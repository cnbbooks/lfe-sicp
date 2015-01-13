### Compound Procedures

We have identified in Lisp some of the elements that must appear in any powerful programming language:

* Numbers and arithmetic operations are primitive data and procedures.
* Nesting of combinations provides a means of combining operations.
* Definitions that associate names with values provide a limited means of abstraction. 

Now we will learn about *procedure definitions*, a much more powerful abstraction technique by which a compound operation can be given a name and then referred to as a unit.

We begin by examining how to express the idea of "squaring." We might say, "To square something, multiply it by itself." This is expressed in our language as 

```lisp
(defun square (x) (* x x))
```

We can understand this in the following way:

```lisp
(defun   square (x)        (*       x      x))
  ^        ^     ^          ^       ^      ^
  |        |     |          |       |      |
 To      square something, multiply it by itself.
```

We have here a *compound procedure*, which has been given the name ``square``. The procedure represents the operation of multiplying something by itself. The thing to be multiplied is given a local name, ``x``, which plays the same role that a pronoun plays in natural language. Evaluating the definition creates this compound procedure and associates it with the name ``square``.[^1]

The general form of a procedure definition is

```lisp
(defun <name> (<formal parameters>) <body>)
```

The ``<name>`` is a symbol to be associated with the procedure definition in the environment.[^2] The ``<formal parameters>`` are the names used within the body of the procedure to refer to the corresponding arguments of the procedure. The ``<body>`` is an expression that will yield the value of the procedure application when the formal parameters are replaced by the actual arguments to which the procedure is applied.[^3] The ``<name>`` and the`` <formal parameters>`` are grouped within parentheses, just as they would be in an actual call to the procedure being defined.

Having defined square, we can now use it:

```lisp
> (square 21)
441
> (square (+ 2 5))
49
> (square (square 3))
81
```

We can also use square as a building block in defining other procedures. For example, $$x^2$$ + $$y^2$$ can be expressed as

```lisp
(+ (square x) (square y))
```

 We can easily define a procedure ``sum-of-squares`` that, given any two numbers as arguments, produces the sum of their squares:
 
 ```lisp
 > (defun sum-of-squares (x y)
     (+ (square x) (square y)))
sum-of-squares
> (sum-of-squares 3 4)
25
```

Now we can use ``sum-of-squares`` as a building block in constructing further procedures:

```lisp
> (defun f (a)
    (sum-of-squares (+ a 1) (* a 2)))
f
> (f 5)
136
```

Compound procedures are used in exactly the same way as primitive procedures. Indeed, one could not tell by looking at the definition of ``sum-of-squares`` given above whether ``square`` was built into the interpreter, like ``+`` and ``*``, or defined as a compound procedure.

----

[^1]: Observe that there are two different operations being combined here: we are creating the procedure, and we are giving it the name ``square``. It is possible, indeed important, to be able to separate these two notions -- to create procedures without naming them, and to give names to procedures that have already been created. We will see how to do this in section [9.4.2](). 

[^2]: Throughout this book, we will describe the general syntax of expressions by using italic symbols delimited by angle brackets -- e.g., ``<name>`` -- to denote the "slots" in the expression to be filled in when such an expression is actually used. 

[^3]: More generally, the body of the procedure can be a sequence of expressions. In this case, the interpreter evaluates each expression in the sequence in turn and returns the value of the final expression as the value of the procedure application. 





