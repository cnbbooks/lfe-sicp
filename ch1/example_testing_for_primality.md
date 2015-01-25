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

TBD

#### Probabilistic methods

TBD


----

[^1]: 








