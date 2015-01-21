### Expressions

One easy way to get started at programming is to examine some typical
interactions with an interpreter for the LFE dialect of Lisp. Imagine that you
are sitting at a computer terminal. You type an expression, and the interpreter
responds by displaying the result of its evaluating that expression.

One kind of primitive expression you might type is a number. (More precisely,
the expression that you type consists of the numerals that represent the number
in base 10.) If you present LFE with a number

```lisp
> 42
```

the interpreter will respond by printing[^1]

```lisp
42
```

Expressions representing numbers may be combined with an expression
representing a primitive function (such as ``+`` or ``*``) to form a compound
expression that represents the application of the function to those numbers.
For example:

```lisp
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

Expressions such as these, formed by delimiting a list of expressions within
parentheses in order to denote function application, are called *combinations*.
The leftmost element in the list is called the *operator*, and the other
elements are called *operands*. The value of a combination is obtained by
applying the function specified by the operator to the arguments that are the
values of the operands.

The convention of placing the operator to the left of the operands is known as
*prefix notation*, and it may be somewhat confusing at first[^2] because it
departs significantly from the customary mathematical convention. Prefix
notation has several advantages, however. One of them is that it can
accommodate functions that may take an arbitrary number of arguments, as in the
following examples:

```lisp
> (+ 21 35 12 7)
75

> (* 25 4 12)
1200
```

No ambiguity can arise, because the operator is always the leftmost element and
the entire combination is delimited by the parentheses.

A second advantage of prefix notation is that it extends in a straightforward
way to allow combinations to be nested, that is, to have combinations whose
elements are themselves combinations:

```lisp
> (+ (* 3 5) (- 10 6))
19
```

There is no limit (in principle) to the depth of such nesting and to the
overall complexity of the expressions that the Lisp interpreter can evaluate.
It is we humans who get confused by still relatively simple expressions such as

```lisp
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
```

which the interpreter would readily evaluate to be 57. We can help ourselves by
writing such an expression in the form

```lisp
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
```

following a formatting convention known as *pretty-printing*, in which each
long combination is written so that the operands are aligned vertically. The
resulting indentations display clearly the structure of the expression.[^3]

Even with complex expressions, the interpreter always operates in the same
basic cycle: It reads an expression from the terminal, evaluates the
expression, and prints the result. This mode of operation is often expressed by
saying that the interpreter runs in a *read-eval-print loop*, or *REPL*.
Observe in particular that it is not necessary to explicitly instruct the
interpreter to print the value of the expression.[^4]

----

[^1]: Throughout this book, when we wish to emphasize the distinction between the input typed by the user and the response printed by the interpreter, we will present the former with the standard LFE REPL prompt, ``>``.

[^2]: Such confusion was fabulously and lovingly presented in a playful blog post by Cristina Videira Lopes, entitled [Jedi Masters](http://tagide.com/blog/2014/10/jedi-masters/): "Master Yoda spoke in an unusual way. If he needed to add 2 to 5 and then multiply the result by 3, he would say “multiply by 3 the result of adding 2 to 5.” This made conversations with him always intriguing, as when asked for directions to the bathroom he would say things like “come back to the council room after counting two doors on the left after turning left after coming out of the bathroom after washing your hands after peeing after entering the bathroom on the 3rd door on the right after counting two doors on your right after turning right after exiting the council room.” As a consequence, all Jedi spoke like that… at least until many years later, when a rebellious Jedi called Jolee Bindo found a clever way of transforming the language of the Force into the language spoken by non-Force-sensitives, which helped a lot of people not to get lost on their way to the bathroom… while still being puzzled about whether they were actually allowed to pee, as that would irreversibly change the state of the world. Anyway, I digress."

[^3]: Lisp-enabled text-editors and IDEs (including vi and emacs) typically provide features to aid the user in formatting expressions. Two especially useful features are one that automatically indents to the proper pretty-print position whenever a new line is started and one that highlights the matching left parenthesis whenever a right parenthesis is typed.

[^4]: Lisp obeys the convention that every expression has a value. This convention, together with the old reputation of Lisp as an inefficient language, is the source of the quip by Alan Perlis (paraphrasing Oscar Wilde) that "Lisp programmers know the value of everything but the cost of nothing."









