### Example: Testing for Primality

This section describes two methods for checking the primality of an integer $$n$$, one with order of growth $$\Theta(\sqrt n$$), and a "probabilistic" algorithm with order of growth $$\Theta(\log n)$$. The exercises at the end of this section suggest programming projects based on these algorithms.


#### Searching for divisors

Since ancient times, mathematicians have been fascinated by problems concerning prime numbers, and many people have worked on the problem of determining ways to test if numbers are prime. One way to test if a number is prime is to find the number's divisors. The following program finds the smallest integral divisor (greater than 1) of a given number $$n$$. It does this in a straightforward way, by testing $$n$$ for divisibility by successive integers starting with 2.

```lisp

```

XXX

```lisp

```

XXX

#### The Fermat test

TBD

#### Probabilistic methods

TBD

