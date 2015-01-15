### Example: Square Roots by Newton's Method

 Procedures, as introduced above, are much like ordinary mathematical functions. They specify a value that is determined by one or more parameters. But there is an important difference between mathematical functions and computer procedures. Procedures must be effective.

As a case in point, consider the problem of computing square roots. We can define the square-root function as 

$$
\begin{align}
\sqrt{r} = \text{the } y \text{ such that } y \geq 0 \text{ and } y^2 = x
\end{align}
$$

This describes a perfectly legitimate mathematical function. We could use it to recognize whether one number is the square root of another, or to derive facts about square roots in general. On the other hand, the definition does not describe a procedure. Indeed, it tells us almost nothing about how to actually find the square root of a given number. It will not help matters to rephrase this definition in pseudo-LFE:

```lisp
(defun sqrt (x)
  (the y (and (>= y 0)
              (= (square y) x))))
```

This only begs the question.

The contrast between function and procedure is a reflection of the general distinction between describing properties of things and describing how to do things, or, as it is sometimes referred to, the distinction between declarative knowledge and imperative knowledge. In mathematics we are usually concerned with declarative (what is) descriptions, whereas in computer science we are usually concerned with imperative (how to) descriptions.[^1]

How does one compute square roots? The most common way is to use Newton's method of successive approximations, which says that whenever we have a guess $$y$$ for the value of the square root of a number $$x$$, we can perform a simple manipulation to get a better guess (one closer to the actual square root) by averaging $$y$$ with $$x/y$$.[^2] For example, we can compute the square root of 2 as follows. Suppose our initial guess is 1:

| Guess  | Quotient                | Average                            |
|:------:|:-----------------------:|:----------------------------------:|
| 1      | ``(2/1) = 2``           | ``((2 + 1)/2) = 1.5 ``             |
| 1.5    | ``(2/1.5) = 1.3333``    | ``((1.3333 + 1.5)/2) = 1.4167``    |
| 1.4167 | ``(2/1.4167) = 1.4118`` | ``((1.4167 + 1.4118)/2) = 1.4142`` |
| 1.4142 | ...                     | ...                                |

Continuing this process, we obtain better and better approximations to the square root.
 
 

----

[^1]: Declarative and imperative descriptions are intimately related, as indeed are mathematics and computer science. For instance, to say that the answer produced by a program is "correct" is to make a declarative statement about the program. There is a large amount of research aimed at establishing techniques for proving that programs are correct, and much of the technical difficulty of this subject has to do with negotiating the transition between imperative statements (from which programs are constructed) and declarative statements (which can be used to deduce things). In a related vein, an important current area in programming-language design is the exploration of so-called very high-level languages, in which one actually programs in terms of declarative statements. The idea is to make interpreters sophisticated enough so that, given "what is" knowledge specified by the programmer, they can generate "how to" knowledge automatically. This cannot be done in general, but there are important areas where progress has been made. We shall revisit this idea in chapter 12. 

[^2]: This square-root algorithm is actually a special case of Newton's method, which is a general technique for finding roots of equations. The square-root algorithm itself was developed by Heron of Alexandria in the first century A.D. We will see how to express the general Newton's method as a Lisp procedure in section 9.4.4. 







