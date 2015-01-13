### Expressions

One easy way to get started at programming is to examine some typical interactions with an interpreter for the LFE dialect of Lisp. Imagine that you are sitting at a computer terminal. You type an expression, and the interpreter responds by displaying the result of its evaluating that expression.

One kind of primitive expression you might type is a number. (More precisely, the expression that you type consists of the numerals that represent the number in base 10.) If you present LFE with a number

```
> 42
```

the interpreter will respond by printing[^1]

```
42
```

Expressions representing numbers may be combined with an expression representing a primitive procedure (such as ``+`` or ``*``) to form a compound expression that represents the application of the procedure to those numbers. For example:

```
> (- 309 267)
42
> (+ 1000 337)
1337
> (* 5 99)
495
> (/ 10 5)
2.0
> (+ 2.7 10)
12.7
```


----

[^1]: Throughout this book, when we wish to emphasize the distinction between the input typed by the user and the response printed by the interpreter, we will present the former with the standard LFE REPL prompt, ``>``. 