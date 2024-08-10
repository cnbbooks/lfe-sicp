### Functions as Returned Values

The previous examples demonstrate how the ability to pass functions as arguments significantly enhances the expressive power of our programming language. We can achieve even more expressive power by creating functions whose returned values are themselves functions.

We can illustrate this idea by looking again at the fixed-point example described at the end of the section [Functions as General Methods](). We formulated a new version of the square-root function as a fixed-point search, starting with the observation that \\(\sqrt x\\) is a fixed-point of the function \\(y \mapsto \frac{x}{y}\\). Then we used average damping to make the approximations converge. Average damping is a useful general technique in itself. Namely, given a function \\(f\\), we consider the function whose value at \\(x\\) is equal to the average of \\(x\\) and \\(f(x)\\).

We can express the idea of average damping by means of the following function:

```lisp
(defun average-damp (f)
  (lambda (x)
    (average x (funcall f x))))
```

``average-damp/1`` is a function that takes as its argument a function ``f`` and returns as its value a function (produced by the ``lambda``) that, when applied to a number ``x``, produces the average of ``x`` and ``(f x)``. For example, applying ``average-damp/1`` to the ``square/1`` function produces a function whose value at some number \\(x\\) is the average of \\(x\\) and \\(x^2\\). Applying this resulting function to 10 returns the average of 10 and 100, or 55:[^1]

```lisp
> (funcall (average-damp #'square/1) 10)
55.0
```

Using ``average-damp/1``, we can reformulate the ``sqrt/1`` function as follows:

```lisp
(defun sqrt (x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
```

Notice how this formulation makes explicit the three ideas in the method: fixed-point search, average damping, and the function \\(y \mapsto \frac{x}{y}\\). It is instructive to compare this formulation of the square-root method with the original version given in the section [Example: Square Roots by Newton's Method](). Bear in mind that these functions express the same process, and notice how much clearer the idea becomes when we express the process in terms of these abstractions. In general, there are many ways to formulate a process as a function. Experienced programmers know how to choose procedural formulations that are particularly perspicuous, and where useful elements of the process are exposed as separate entities that can be reused in other applications. As a simple example of reuse, notice that the cube root of \\(x\\) is a fixed point of the function \\(y \mapsto \frac{x}{y^2}\\), so we can immediately generalize our square-root function to one that extracts cube roots:[^2]

```lisp
(defun cube-root (x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
```

#### Newton's method

When we first introduced the square-root function in the section [Example: Square Roots by Newton's Method](), we mentioned that this was a special case of *Newton's method*. If \\(x \mapsto g(x)\\) is a differentiable function, then a solution of the equation \\(g(x) = 0\\) is a fixed point of the function \\(x \mapsto f(x)\\) where

\\[
\begin{align}
f(x) = r - \frac{g(x)}{Dg(x)}
\end{align}
\\]

and \\(Dg(x)\\) is the derivative of \\(g\\) evaluated at \\(x\\). Newton's method is the use of the fixed-point method we saw above to approximate a solution of the equation by finding a fixed point of the function \\(f\\).[^3] For many functions \\(g\\) and for sufficiently good initial guesses for \\(x\\), Newton's method converges very rapidly to a solution of \\(g(x) = 0\\).[^4]

In order to implement Newton's method as a function, we must first express the idea of derivative. Note that "derivative," like average damping, is something that transforms a function into another function. For instance, the derivative of the function \\((x \mapsto x^3\\) is the function \\(x \mapsto 3x^2\\). In general, if \\(g\\) is a function and \\(dx\\) is a small number, then the derivative \\(Dg\\) of \\(g\\) is the function whose value at any number \\(x\\) is given (in the limit of small \\(dx\\)) by

\\[
\begin{align}
Dg(x) = \frac{g(x + dx) - g(x)}{dx}
\end{align}
\\]

Thus, we can express the idea of derivative (taking \\(dx\\) to be, say, 0.00001) as the function

```lisp
(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x dx)) (funcall g x))
       dx)))
```

along with the definition

```lisp
> (set dx 0.00001)
1.0e-5
```

Like ``average-damp/1``, ``deriv/1`` is a function that takes a function as argument and returns a function as value. For example, to approximate the derivative of \\(x \mapsto x^3\\) at 5 (whose exact value is 75) we can evaluate

```lisp
> (defun cube (x) (* x x x))
cube
> (funcall (deriv #'cube/1) 5)
75.00014999664018
```

With the aid of ``deriv/1``, we can express Newton's method as a fixed-point process:

```lisp
(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))
```

The ``newton-transform/1`` function expresses the formula at the beginning of this section, and ``newtons-method/2`` is readily defined in terms of this. It takes as arguments a function that computes the function for which we want to find a zero, together with an initial guess. For instance, to find the square root of \\(x\\), we can use Newton's method to find a zero of the function \\(y \mapsto y^2 - x\\) starting with an initial guess of 1.[^5] This provides yet another form of the square-root function:

```lisp
(defun sqrt (x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
```

#### Abstractions and first-class functions

We've seen two ways to express the square-root computation as an instance of a more general method, once as a fixed-point search and once using Newton's method. Since Newton's method was itself expressed as a fixed-point process, we actually saw two ways to compute square roots as fixed points. Each method begins with a function and finds a fixed point of some transformation of the function. We can express this general idea itself as a function:

```lisp
(defun fixed-point-of-transform (g transform guess)
  (fixed-point (transform g) guess))
```

This very general function takes as its arguments a function ``g`` that computes some function, a function that transforms ``g``, and an initial guess. The returned result is a fixed point of the transformed function.

Using this abstraction, we can recast the first square-root computation from this section (where we look for a fixed point of the average-damped version of \\(y \mapsto \frac{x}{y}
\\)) as an instance of this general method:

```lisp
(defun sqrt (x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
```

Similarly, we can express the second square-root computation from this section (an instance of Newton's method that finds a fixed point of the Newton transform of \\(y \mapsto y^2 - x\\)) as

```lisp
(defun sqrt (x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
```

We began the section [Formulating Abstractions with Higher-Order Functions]() with the observation that compound functions are a crucial abstraction mechanism, because they permit us to express general methods of computing as explicit elements in our programming language. Now we've seen how higher-order functions permit us to manipulate these general methods to create further abstractions.

As programmers, we should be alert to opportunities to identify the underlying abstractions in our programs and to build upon them and generalize them to create more powerful abstractions. This is not to say that one should always write programs in the most abstract way possible; expert programmers know how to choose the level of abstraction appropriate to their task. But it is important to be able to think in terms of these abstractions, so that we can be ready to apply them in new contexts. The significance of higher-order functions is that they enable us to represent these abstractions explicitly as elements in our programming language, so that they can be handled just like other computational elements.

In general, programming languages impose restrictions on the ways in which computational elements can be manipulated. Elements with the fewest restrictions are said to have *first-class* status. Some of the "rights and privileges" of first-class elements are:[^6]

* They may be named by variables.
* They may be passed as arguments to functions.
* They may be returned as the results of functions.
* They may be included in data structures.[^7]

Lisp, unlike other common programming languages, awards functions full first-class status. This poses challenges for efficient implementation, but the resulting gain in expressive power is enormous.[^8]

----

[^1]: Observe that this is a combination whose operator is itself a combination. Exercise 1.4 already demonstrated the ability to form such combinations, but that was only a toy example. Here we begin to see the real need for such combinations -- when applying a function that is obtained as the value returned by a higher-order function.

[^2]: See exercise 1.45 for a further generalization.

[^3]: Elementary calculus books usually describe Newton's method in terms of the sequence of approximations \\(x_{n+1} = x_n - g(x_n)/Dg(x_n)\\). Having language for talking about processes and using the idea of fixed points simplifies the description of the method.

[^4]: Newton's method does not always converge to an answer, but it can be shown that in favorable cases each iteration doubles the number-of-digits accuracy of the approximation to the solution. In such cases, Newton's method will converge much more rapidly than the half-interval method.

[^5]: For finding square roots, Newton's method converges rapidly to the correct solution from any starting point.

[^6]: The notion of first-class status of programming-language elements is due to the British computer scientist Christopher Strachey (1916-1975).

[^7]: We'll see examples of this after we introduce data structures in the next chapter 2.

[^8]: The major implementation cost of first-class functions is that allowing functions to be returned as values requires reserving storage for a function's free variables even while the function is not executing. In the Scheme implementation we will study in the section [The Metacircular Evaluator](), these variables are stored in the function's environment.
