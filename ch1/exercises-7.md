### Exercises

#### Exercise 1.20.

The process that a function generates is of course dependent on the rules used by the interpreter. As an example, consider the iterative ``gcd/2`` function given previously. Suppose we were to interpret this procedure using normal-order evaluation, as discussed in the section [The Substitution Model for Procedure Application](). (The normal-order-evaluation rule for ``if`` is described in exercise 1.5.) Using the substitution method (for normal order), illustrate the process generated in evaluating ``(gcd 206 40)`` and indicate the ``rem`` operations that are actually performed. How many ``rem`` operations are actually performed in the normal-order evaluation of ``(gcd 206 40)``? In the applicative-order evaluation? 