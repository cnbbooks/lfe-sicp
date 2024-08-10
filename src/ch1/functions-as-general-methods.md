### Functions as General Methods

We introduced compound functions in the section [Compound Functions]() as a mechanism for abstracting patterns of numerical operations so as to make them independent of the particular numbers involved. With higher-order functions, such as the ``integral/4`` function of the section [Functions as Arguments](), we began to see a more powerful kind of abstraction: functions used to express general methods of computation, independent of the particular functions involved. In this section we discuss two more elaborate examples -- general methods for finding zeros and fixed points of functions -- and show how these methods can be expressed directly as functions.


#### Finding roots of equations by the half-interval method

The *half-interval method* is a simple but powerful technique for finding roots of an equation \\(f(x) = 0\\), where \\(f\\) is a continuous function. The idea is that, if we are given points \\(a\\) and \\(b\\) such that \\(f(a) < 0 < f(b)\\), then \\(f\\) must have at least one zero between \\(a\\) and \\(b\\). To locate a zero, let \\(x\\) be the average of \\(a\\) and \\(b\\) and compute \\(f(x)\\). If \\(f(x) > 0\\), then \\(f\\) must have a zero between \\(a\\) and \\(x\\). If \\(f(x) < 0\\), then \\(f\\) must have a zero between \\(x\\) and \\(b\\). Continuing in this way, we can identify smaller and smaller intervals on which \\(f\\) must have a zero. When we reach a point where the interval is small enough, the process stops. Since the interval of uncertainty is reduced by half at each step of the process, the number of steps required grows as \(\Theta(log( L/T))\\), where \\(L\\) is the length of the original interval and \\(T\\) is the error tolerance (that is, the size of the interval we will consider "small enough"). Here is a function that implements this strategy:

```lisp
(defun negative? (x)
  (< x 0))

(defun positive? (x)
  (> x 0))

(defun search (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (funcall f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
```

We assume that we are initially given the function \\(f\\) together with points at which its values are negative and positive. We first compute the midpoint of the two given points. Next we check to see if the given interval is small enough, and if so we simply return the midpoint as our answer. Otherwise, we compute as a test value the value of \\(f\\) at the midpoint. If the test value is positive, then we continue the process with a new interval running from the original negative point to the midpoint. If the test value is negative, we continue with the interval from the midpoint to the positive point. Finally, there is the possibility that the test value is 0, in which case the midpoint is itself the root we are searching for.

To test whether the endpoints are "close enough" we can use a function similar to the one used in the section [Example: Square Roots by Newton's Method]() for computing square roots:[^1]

```lisp
(defun close-enough? (x y)
  (< (abs (- x y)) 0.001))
```

``search/3`` is awkward to use directly, because we can accidentally give it points at which \\(f\\)'s values do not have the required sign, in which case we get a wrong answer. Instead we will use ``search/3`` via the following function, which checks to see which of the endpoints has a negative function value and which has a positive value, and calls the ``search/3`` function accordingly. If the function has the same sign on the two given points, the half-interval method cannot be used, in which case the function signals an error.[^2]

```lisp

(defun half-interval-method (f a b)
  (let ((a-value (funcall f a))
        (b-value (funcall f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign")))))
```

The following example uses the half-interval method to approximate \\(\pi\\) as the root between 2 and 4 of \\(sin x = 0\\):

```lisp
> (half-interval-method #'math:sin/1 2.0 4.0)
3.14111328125
```

Here is another example, using the half-interval method to search for a root of the equation \\(x^3 - 2x - 3 = 0\\) between 1 and 2:

```lisp
> (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                        1.0
                        2.0)
1.89306640625
```

#### Finding fixed points of functions

A number \\(x\\) is called a *fixed point* of a function \\(f\\) if \\(x\\) satisfies the equation \\(f(x) = x\\). For some functions \\(f\\) we can locate a fixed point by beginning with an initial guess and applying \\(f\\) repeatedly,

\\[
\begin{align}
f(x),f(f(x)), f(f(f(x))), \dots
\end{align}
\\]

until the value does not change very much. Using this idea, we can devise a function ``fixed-point/2`` that takes as inputs a function and an initial guess and produces an approximation to a fixed point of the function. We apply the function repeatedly until we find two successive values whose difference is less than some prescribed tolerance:

```lisp
(defun fixed-point (f first-guess)
  (fletrec ((close-enough? (v1 v2)
              (< (abs (- v1 v2)) tolerance))
            (try-it (guess)
              (let ((next (funcall f guess)))
                (if (close-enough? guess next)
                    next
                    (try-it next)))))
    (try-it first-guess)))
```

For example, we can use this method to approximate the fixed point of the cosine function, starting with 1 as an initial approximation:[^3]

```lisp
> (set tolerance 0.00001)
1.0e-5
> (fixed-point #'math:cos/1 1.0)
0.7390822985224023
```

Similarly, we can find a solution to the equation \\(y = \sin y + \cos y\\):

```lisp
> (fixed-point (lambda (y) (+ (math:sin y) (math:cos y)))
               1.0)
1.2587315962971173
```

The fixed-point process is reminiscent of the process we used for finding square roots in the section [Example: Square Roots by Newton's Method](). Both are based on the idea of repeatedly improving a guess until the result satisfies some criterion. In fact, we can readily formulate the square-root computation as a fixed-point search. Computing the square root of some number \\(x\\) requires finding a \\(y\\) such that \\(y^2 = x\\). Putting this equation into the equivalent form \\(y = \frac{x}{y}\\), we recognize that we are looking for a fixed point of the function[^4] \\(y \mapsto \frac{x}{y}\\), and we can therefore try to compute square roots as

```lisp
(defun sqrt (x)
  (fixed-point (lambda (y) (/ x y))
               1.0))
```

Unfortunately, this fixed-point search does not converge. Consider an initial guess \\(y_1\\). The next guess is \\(y_2 = x/y1\\) and the next guess is \\((y_3 = x/y2 = x/(x/y1) = y_1\\). This results in an infinite loop in which the two guesses \\((y_1\\) and \\(y_2\\) repeat over and over, oscillating about the answer.

One way to control such oscillations is to prevent the guesses from changing so much. Since the answer is always between our guess \\(y\\) and \\(\frac{x}{y}\\), we can make a new guess that is not as far from \\(y\\) as \\(\frac{x}{y}\\) by averaging \\(y\\) with \\(\frac{x}{y}\\), so that the next guess after \\(y\\) is \\(\frac{y + \frac{x}{y}}{2}\\) instead of \\(\frac{x}{y}\\). The process of making such a sequence of guesses is simply the process of looking for a fixed point of \\(y \mapsto \frac{y + \frac{x}{y}}{2}\\):

```lisp
(defun sqrt (x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
```

(Note that \\(y = \frac{y + \frac{x}{y}}{2}\\) is a simple transformation of the equation \\(y = \frac{x}{y}\\); to derive it, add \\(y\\) to both sides of the equation and divide by 2.)

With this modification, the square-root function works. In fact, if we unravel the definitions, we can see that the sequence of approximations to the square root generated here is precisely the same as the one generated by our original square-root function of the section [Example: Square Roots by Newton's Method](). This approach of averaging successive approximations to a solution, a technique we that we call *average damping*, often aids the convergence of fixed-point searches.

----

[^1]: We have used 0.001 as a representative "small" number to indicate a tolerance for the acceptable error in a calculation. The appropriate tolerance for a real calculation depends upon the problem to be solved and the limitations of the computer and the algorithm. This is often a very subtle consideration, requiring help from a numerical analyst or some other kind of magician.

[^2]: This can be accomplished using ``error/1``, which takes a "reason" as an argument and stops the execution of the calling process with with an exception error and the provided reason.

[^3]: Try this during a boring lecture: Set your calculator to radians mode and then repeatedly press the ``cos`` button until you obtain the fixed point.

[^4]: \\(\mapsto\\) (pronounced "maps to") is the mathematician's way of writing ``lambda``. \\(y \mapsto \frac{x}{y}\\) means ``(lambda(y) (/ x y))``, that is, the function whose value at \\(y\\) is \\(\frac{x}{y}\\).
