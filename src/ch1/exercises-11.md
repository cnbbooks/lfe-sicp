### Exercises

#### Exercise 1.35

Show that the golden ratio \\(\phi\\) (the section [Tree Recursion]()) is a fixed point of the transformation \\(x \mapsto 1 + \frac{1}{x}\\), and use this fact to compute \\(\phi\\) by means of the ``fixed-point/2`` function.

#### Exercise 1.36

Modify ``fixed-point/2`` so that it prints the sequence of approximations it generates, using ``io:format`` as shown in exercise 1.22. Then find a solution to \\(x^x = 1000\\) by finding a fixed point of \\(x \mapsto \frac{log(1000)}{log(x)}\\). (Use Erlang's ``math:log`` function, which computes natural logarithms.) Compare the number of steps this takes with and without average damping. (Note that you cannot start ``fixed-point/2`` with a guess of 1, as this would cause division by \\(log(1) = 0 \\).)

#### Exercise 1.37

a. An infinite *continued fraction* is an expression of the form

\\[
\begin{align}
f= \frac{N_1}{D_1 + \frac{N_2}{D_2 + \frac{N_3}{D_3 + \cdots}}}
\end{align}
\\]

As an example, one can show that the infinite continued fraction expansion with the \\(N_i\\) and the \\(D_i\\) all equal to 1 produces \\(frac{1}{\phi}\\), where \\(\phi\\) is the golden ratio (described in the section [Tree Recursion]()]). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called *k-term finite continued fraction* -- has the form

\\[
\begin{align}
\frac{N_1}{D_1 + \frac{N_2}{\ddots + \frac{N_K}{D_K}}}
\end{align}
\\]

Suppose that ``n`` and ``d`` are functions of one argument (the term index \\(i\\)) that return the \\(N_i\\) and \\(D_i\\) of the terms of the continued fraction. Define a function ``cont-frac/3`` such that evaluating ``(cont-frac n d k)`` computes the value of the \\(k\\)-term finite continued fraction. Check your function by approximating \\(\frac{1}{\phi}\\) using

```lisp
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)
```

for successive values of ``k``. How large must you make ``k`` in order to get an approximation that is accurate to 4 decimal places?

b. If your ``cont-frac/3`` function generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

#### Exercise 1.38

In 1737, the Swiss mathematician Leonhard Euler published a memoir *De Fractionibus Continuis*, which included a continued fraction expansion for \\(e - 2\\), where \\(e\\) is the base of the natural logarithms. In this fraction, the \\(N_i\\) are all 1, and the \\(D_i\\) are successively \\(1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, \cdot \\). Write a program that uses your ``cont-frac/3`` function from exercise 1.37 to approximate \\(e\\), based on Euler's expansion.

#### Exercise 1.39

A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert:

\\[
\begin{align}
\tan r = \frac{r}{1 - \frac{r^2}{3 - \frac{r^2}{5 - \ddots}}}
\end{align}
\\]

where \\(x\\) is in radians. Define a function ``(tan-cf x k)`` that computes an approximation to the tangent function based on Lambert's formula. ``k`` specifies the number of terms to compute, as in exercise 1.37.
