### Exercises

#### Exercise 1.40

Define a function ``cubic/3`` that can be used together with the ``newtons-method/2`` function in expressions of the form

```lisp
(newtons-method (cubic a b c) 1)
```

to approximate zeros of the cubic $$x^3 + ax^2 + bx + c$$.

#### Exercise 1.41

Define a function ``double`` that takes a function of one argument as argument and returns a function that applies the original function twice. For example, if inc is a function that adds 1 to its argument, then (double inc) should be a function that adds 2. What value is returned by

```lisp
(funcall (funcall (double (double #'double/1)) #'inc/1) 5)
```

#### Exercise 1.42

Let $$f$$ and $$g$$ be two one-argument functions. The *composition* $$f$$ after $$g$$ is defined to be the function $$x \mapsto f(g(x))$$. Define a function ``compose`` that implements composition. For example, if ``inc/1`` is a function that adds 1 to its argument,

```lisp
> (funcall (compose square inc) 6)
49
```

#### Exercise 1.43

If $$f$$ is a numerical function and $$n$$ is a positive integer, then we can form the $$n$$th repeated application of $$f$$, which is defined to be the function whose value at x is $$f(f( \cdots (f(x)) \cdots ))$$. For example, if $$f$$ is the function $$x \mapsto x + 1$$, then the $$n$$th repeated application of $$f$$ is the function $$x \mapsto x + n$$. If $$f$$ is the operation of squaring a number, then the $$n$$th repeated application of $$f$$ is the function that raises its argument to the $$2^n$$th power. Write a function that takes as inputs a function that computes $$f$$ and a positive integer $$n$$ and returns the function that computes the $$n$$th repeated application of $$f$$. Your function should be able to be used as follows:

```lisp
(funcall (repeated square 2) 5)
625
```

Hint: You may find it convenient to use compose from exercise 1.42.

#### Exercise 1.44

The idea of $$smoothing$$ a function is an important concept in signal processing. If $$f$$ is a function and $$dx$$ is some small number, then the smoothed version of $$f$$ is the function whose value at a point $$x$$ is the average of $$f(x - dx)$$, $$f(x)$$, and $$f(x + dx)$$. Write a function ``smooth`` that takes as input a function that computes $$f$$ and returns a function that computes the smoothed $$f$$. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtained the *n-fold smoothed function*. Show how to generate the *n*-fold smoothed function of any given function using ``smooth`` as well as ``repeated`` from exercise 1.43.

#### Exercise 1.45

We saw in the section [Functions as General Methods]() that attempting to compute square roots by naively finding a fixed point of $$y \mapsto \frac{x}{y}$$ does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped $$y \mapsto \frac{x}{y^2}. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for $$y \mapts \frac{x}{y^3}$$ converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of $$y \mapsto \frac{x}{y^3}) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute $$n$$th roots as a fixed-point search based upon repeated average damping of $$y \mapsto \frac{x}{y^{n-1}}$$. Use this to implement a simple function for computing $$n$$th roots using ``fixed-point/2``, ``average-damp/1``, and the ``repeated`` function of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

#### Exercise 1.46

Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as *iterative improvement*. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess. Write a function ``iterative-improve/2`` that takes two functions as arguments: a method for telling whether a guess is good enough and a method for improving a guess. ``iterative-improve/2`` should return as its value a function that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the ``sqrt/1`` function of the section [Example: Square Roots by Newton's Method]() and the ``fixed-point/2`` function of the section [Functions as General Methods]() in terms of ``iterative-improve/2``.
