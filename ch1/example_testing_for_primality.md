### Example: Testing for Primality

This section describes two methods for checking the primality of an integer $$n$$, one with order of growth $$\Theta(\sqrt n$$), and a "probabilistic" algorithm with order of growth $$\Theta(\log n)$$. The exercises at the end of this section suggest programming projects based on these algorithms.


#### Searching for divisors

Since ancient times, mathematicians have been fascinated by problems concerning prime numbers, and many people have worked on the problem of determining ways to test if numbers are prime. One way to test if a number is prime is to find the number's divisors. The following program finds the smallest integral divisor (greater than 1) of a given number $$n$$. It does this in a straightforward way, by testing $$n$$ for divisibility by successive integers starting with 2.

```lisp
(defun smallest-divisor (n)
  (find-divisor n 2))
  
(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ('true (find-divisor n (+ test-divisor 1)))))
        
(defun divides? (a b)
  (== (rem b a) 0))
```

We can test whether a number is prime as follows: n is prime if and only if n is its own smallest divisor.

```lisp
(defun prime? (n)
  (== n (smallest-divisor n)))
```

The end test for ``find-divisor/2`` is based on the fact that if $$n$$ is not prime it must have a divisor less than or equal to $$\sqrt n$$.[^1] This means that the algorithm need only test divisors between 1 and $$\sqrt n$$. Consequently, the number of steps required to identify $$n$$ as prime will have order of growth $$\Theta(\sqrt n)$$.

#### The Fermat test

The $$\Theta(\log n)$$ primality test is based on a result from number theory known as Fermat's Little Theorem.[^2]

**Fermat's Little Theorem**: If $$n$$ is a prime number and $$a$$ is any positive integer less than $$n$$, then $$a$$ raised to the $$n$$th power is congruent to a modulo $$n$$.

(Two numbers are said to be *congruent modulo* $$n$$ if they both have the same remainder when divided by $$n$$. The remainder of a number $$a$$ when divided by $$n$$ is also referred to as the *remainder of* $$a$$ *modulo* $$n$$, or simply as $$a$$ *modulo* $$n$$.)

If $$n$$ is not prime, then, in general, most of the numbers $$a \lt n$$ will not satisfy the above relation. This leads to the following algorithm for testing primality: Given a number $$n$$, pick a random number $$a \lt n$$ and compute the remainder of $$a^n$$ modulo $$n$$. If the result is not equal to $$a$$, then $$n$$ is certainly not prime. If it is $$a$$, then chances are good that $$n$$ is prime. Now pick another random number $$a$$ and test it with the same method. If it also satisfies the equation, then we can be even more confident that $$n$$ is prime. By trying more and more values of $$a$$, we can increase our confidence in the result. This algorithm is known as the Fermat test.

To implement the Fermat test, we need a procedure that computes the exponential of a number modulo another number:
 
```lisp
(defun expmod (base exp m)
  (cond ((== exp 0) 1)
        ((even? exp)
         (rem (square (expmod base (/ exp 2) m))
                    m))
        ('true
         (rem (* base (expmod base (- exp 1) m))
                    m))))
```

This is very similar to the ``fast-expt/2`` procedure of the section [Exponentiation](). It uses successive squaring, so that the number of steps grows logarithmically with the exponent.[^3]

The Fermat test is performed by choosing at random a number $$a$$ between 1 and $$n - 1$$ inclusive and checking whether the remainder modulo $$n$$ of the $$n$$th power of $$a$$ is equal to $$a$$. The random number $$a$$ is chosen using the ``uniform/1`` function from the ``random`` module in the Erlang standard library. ``uniform/1`` returns a nonnegative integer between ``1`` and the provided input, inclusive; this is exactly what we need.

```lisp
(defun fermat-test (n)
  (flet ((try-it (a)
           (== (expmod a n n) a)))
    (try-it (random:uniform (- n 1)))))
```

The following procedure runs the test a given number of times, as specified by a parameter. Its value is true if the test succeeds every time, and false otherwise.
 
```lisp
(defun fast-prime? (n times)
  (cond ((== n 1) 'true)
        ((== times 0) 'true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        ('true 'false)))
```
 
#### Probabilistic methods

TBD


----

[^1]: If $$d$$ is a divisor of $$n$$, then so is $$\frac{n}{d}$$. But $$d$$ and $$\frac{n}{d}$$ cannot both be greater than $$\sqrt n$$. 

[^2]: Pierre de Fermat (1601-1665) is considered to be the founder of modern number theory. He obtained many important number-theoretic results, but he usually announced just the results, without providing his proofs. Fermat's Little Theorem was stated in a letter he wrote in 1640. The first published proof was given by Euler in 1736 (and an earlier, identical proof was discovered in the unpublished manuscripts of Leibniz). The most famous of Fermat's results -- known as Fermat's Last Theorem -- was jotted down in 1637 in his copy of the book *Arithmetic* (by the third-century Greek mathematician Diophantus) with the remark ``I have discovered a truly remarkable proof, but this margin is too small to contain it.'' Finding a proof of Fermat's Last Theorem became one of the most famous challenges in number theory. A complete solution was finally given in 1995 by Andrew Wiles of Princeton University.

[^3]: The reduction steps in the cases where the exponent $$e$$ is greater than 1 are based on the fact that, for any integers $$x$$, $$y$$, and $$m$$, we can find the remainder of $$x$$ times $$y$$ modulo $$m$$ by computing separately the remainders of $$x$$ modulo $$m$$ and $$y$$ modulo $$m$$, multiplying these, and then taking the remainder of the result modulo $$m$$. For instance, in the case where $$e$$ is even, we compute the remainder of $$b^{\frac{e}{2}}$$ modulo $$m$$, square this, and take the remainder modulo $$m$$. This technique is useful because it means we can perform our computation without ever having to deal with numbers much larger than $$m$$. (Compare exercise 1.25.) 








