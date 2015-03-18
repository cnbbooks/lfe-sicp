### Example: Square Roots by Newton's Method

Functions, as introduced above, are much like ordinary mathematical functions. They specify a value that is determined by one or more parameters. But there is an important difference between mathematical functions and computer functions. Functions must be effective.

As a case in point, consider the problem of computing square roots. We can define the square-root function as

$$
\begin{align}
\sqrt{r} = \text{the } y \text{ such that } y \geq 0 \text{ and } y^2 = x
\end{align}
$$

This describes a perfectly legitimate mathematical function. We could use it to recognize whether one number is the square root of another, or to derive facts about square roots in general. On the other hand, the definition does not describe a function. Indeed, it tells us almost nothing about how to actually find the square root of a given number. It will not help matters to rephrase this definition in pseudo-LFE:

```lisp
(defun sqrt (x)
  (the y (and (>= y 0)
              (= (square y) x))))
```

This only begs the question.

The contrast between function and function is a reflection of the general distinction between describing properties of things and describing how to do things, or, as it is sometimes referred to, the distinction between declarative knowledge and imperative knowledge. In mathematics we are usually concerned with declarative (what is) descriptions, whereas in computer science we are usually concerned with imperative (how to) descriptions.[^1]

How does one compute square roots? The most common way is to use Newton's method of successive approximations, which says that whenever we have a guess $$y$$ for the value of the square root of a number $$x$$, we can perform a simple manipulation to get a better guess (one closer to the actual square root) by averaging $$y$$ with $$x/y$$.[^2] For example, we can compute the square root of 2 as follows. Suppose our initial guess is 1:

| Guess  | Quotient                | Average                            |
|:------:|:-----------------------:|:----------------------------------:|
| 1      | ``(2/1) = 2``           | ``((2 + 1)/2) = 1.5 ``             |
| 1.5    | ``(2/1.5) = 1.3333``    | ``((1.3333 + 1.5)/2) = 1.4167``    |
| 1.4167 | ``(2/1.4167) = 1.4118`` | ``((1.4167 + 1.4118)/2) = 1.4142`` |
| 1.4142 | ...                     | ...                                |

Continuing this process, we obtain better and better approximations to the square root.

Now let's formalize the process in terms of functions. We start with a value for the radicand (the number whose square root we are trying to compute) and a value for the guess. If the guess is good enough for our purposes, we are done; if not, we must repeat the process with an improved guess. We write this basic strategy as a function:

```lisp
(defun sqrt (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x)
            x)))
```

A guess is improved by averaging it with the quotient of the radicand and the
old guess:

```lisp
(defun improve (guess x)
  (average guess (/ x guess)))
```

where

```lisp
(defun average (x y)
  (/ (+ x y) 2))
```

We also have to say what we mean by "good enough." The following will do for
illustration, but it is not really a very good test. (See exercise 1.7.) The
idea is to improve the answer until it is close enough so that its square
differs from the radicand by less than a predetermined tolerance (here
0.001):[^3]

```lisp
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
```

Finally, we need a way to get started. For instance, we can always guess that
the square root of some $$x$$ is $$\frac{x}{2}$$:[^4]

```lisp
(defun sqrt (x)
  (sqrt (* 0.5 x) x))
```

Note that we used the same name as a previous function, the iterative function
that took two arguments. In LFE, not only do functions and variables have a
difference namespace (unlike Scheme), but functions are also dfferentiated by
their arity. As such, functions with different arity may share the same
name.[^5]

If we type these definitions to the interpreter, we can use ``sqrt/1``[^6] just
as we can use any function:

```lisp
> (sqrt 1)
1.0003048780487804
> (sqrt 2)
1.4142342859400734
> (sqrt 9)
3.0001264920597093
> (sqrt (+ 100 37))
11.704699918046352
> (sqrt (+ (sqrt 2) (sqrt 3)))
1.773849624588538
> (square (sqrt 1000))
1000.0003714258778
```

The sqrt program also illustrates that the simple procedural language we have
introduced so far is sufficient for writing any purely numerical program that
one could write in, say, C or Pascal. This might seem surprising, since we have
not included in our language any iterative (looping) constructs that direct the
computer to do something over and over again. ``sqrt/2``, on the other hand,
demonstrates how iteration can be accomplished using no special construct other
than the ordinary ability to call a function.[^7]

----

[^1]: Declarative and imperative descriptions are intimately related, as indeed are mathematics and computer science. For instance, to say that the answer produced by a program is "correct" is to make a declarative statement about the program. There is a large amount of research aimed at establishing techniques for proving that programs are correct, and much of the technical difficulty of this subject has to do with negotiating the transition between imperative statements (from which programs are constructed) and declarative statements (which can be used to deduce things). In a related vein, an important current area in programming-language design is the exploration of so-called very high-level languages, in which one actually programs in terms of declarative statements. The idea is to make interpreters sophisticated enough so that, given "what is" knowledge specified by the programmer, they can generate "how to" knowledge automatically. This cannot be done in general, but there are important areas where progress has been made. We shall revisit this idea in chapter 12.

[^2]: This square-root algorithm is actually a special case of Newton's method, which is a general technique for finding roots of equations. The square-root algorithm itself was developed by [Heron of Alexandria](https://en.wikipedia.org/wiki/Heron_of_Alexandria) in the first century A.D. We will see how to express the general [Newton's method](https://en.wikipedia.org/wiki/Newton%27s_method) as a Lisp function in the section [Functions as Returned Values]().

[^3]: We will usually give predicates names ending with question marks, to help us remember that they are predicates. This is just a stylistic convention. As far as the interpreter is concerned, the question mark is just an ordinary character.

[^4]: We could also have expressed our initial guess as an integer. As with many Lisp implementations, LFE will convert an integer to a float if an operation (such as division) would result in a non-integer. The first two editions of this text, however, used MIT Scheme and not LFE. MIT Scheme *did* distinguish between exact integers and decimal values, and dividing two integers produces a rational number rather than a decimal. For example, dividing 10 by 6 yielded $$\frac{5}{3}$$, while dividing 10.0 by 6.0 yielded 1.6666666666666667. (We will learn how to implement arithmetic on rational numbers in the section [Example: Arithmetic Operations for Rational Numbers]().) In MIT Scheme, if the initial guess had been an integer and the passed argument $$x$$ was an integer, all subsequent values produced in the square-root computation would have been rational numbers rather than decimals. Mixed operations on rational numbers and decimals always yielded decimals, so starting with an initial guess that was a decimal forced all subsequent values to be decimals.

[^5]: Not only is this common practice in Erlang and LFE, but it is considered good style.If two functions of differing arity share the same name it is understood that they should be designed to solve the same problem.

[^6]: Due to the fact that functions of different arity may share the same name, to correctly identify a function, we need to refer to its arity. This is done with a "slash" (``/``) after the function name, followed by the number representing the function's arity, for example, ``average/2`` or ``sqrt/1``.

[^7]: Readers who are worried about the efficiency issues involved in using function calls to implement iteration should note the remarks on "tail recursion" in the section [Linear Recursion and Iteration]().







