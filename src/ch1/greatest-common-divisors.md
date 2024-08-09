### Greatest Common Divisors

The greatest common divisor (GCD) of two integers \\(a\\) and \\(b\\) is defined to be the largest integer that divides both \\(a\\) and \\(b\\) with no remainder. For example, the GCD of 16 and 28 is 4. In chapter 2, when we investigate how to implement rational-number arithmetic, we will need to be able to compute GCDs in order to reduce rational numbers to lowest terms. (To reduce a rational number to lowest terms, we must divide both the numerator and the denominator by their GCD. For example, 16/28 reduces to 4/7.) One way to find the GCD of two integers is to factor them and search for common factors, but there is a famous algorithm that is much more efficient.

The idea of the algorithm is based on the observation that, if \\(r\\) is the remainder when \\(a\\) is divided by \\(b\\), then the common divisors of \\(a\\) and \\(b\\) are precisely the same as the common divisors of \\(b\\) and \\(r\\). Thus, we can use the equation

\\[
\DeclareMathOperator*{\gcd}{GCD}
\gcd(a, b) = \gcd(b, r)
\\]

to successively reduce the problem of computing a GCD to the problem of computing the GCD of smaller and smaller pairs of integers. For example,

\\[
\begin{align}
\DeclareMathOperator*{\gcd}{GCD}
\gcd(206, 40) = \gcd(40, 6) \\\\
= \gcd(6, 4) \ \ \\\\
= \gcd(4, 2) \ \ \\\\
= \gcd(2, 0) \ \ \\\\
= 2\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \
\end{align}
\\]

reduces \\(\text{GCD}(206,40)\\) to \\(\text{GCD}(2,0)\\), which is 2. It is possible to show that starting with any two positive integers and performing repeated reductions will always eventually produce a pair where the second number is 0. Then the GCD is the other number in the pair. This method for computing the GCD is known as *Euclid's Algorithm*.[^1]

It is easy to express Euclid's Algorithm as a function:

```lisp
(defun gcd (a b)
  (if (== b 0)
      a
      (gcd b (rem a b))))
```

This generates an iterative process, whose number of steps grows as the logarithm of the numbers involved.

The fact that the number of steps required by Euclid's Algorithm has logarithmic growth bears an interesting relation to the Fibonacci numbers:

**Lamé's Theorem**: If Euclid's Algorithm requires \\(k\\) steps to compute the GCD of some pair, then the smaller number in the pair must be greater than or equal to the \\(k\\)th Fibonacci number.[^2]

We can use this theorem to get an order-of-growth estimate for Euclid's Algorithm. Let \\(n\\) be the smaller of the two inputs to the function. If the process takes \\(k\\) steps, then we must have \\(n \ge Fib (k) \approx \frac{\phi^k}{\sqrt 5}\\). Therefore the number of steps \\(k\\) grows as the logarithm (to the base \\(\phi\\) ) of \\(n\\).
Hence, the order of growth is \\( \Theta( \log n) \\).

----

[^1]: Euclid's Algorithm is so called because it appears in Euclid's Elements (Book 7, ca. 300 B.C.). According to Knuth (1973), it can be considered the oldest known nontrivial algorithm. The ancient Egyptian method of multiplication (exercise 1.18) is surely older, but, as Knuth explains, Euclid's algorithm is the oldest known to have been presented as a general algorithm, rather than as a set of illustrative examples.

[^2]: This theorem was proved in 1845 by Gabriel Lamé, a French mathematician and engineer known chiefly for his contributions to mathematical physics. To prove the theorem, we consider pairs \\((a_k, b_k) \\), where \\(a_k \ge b_k \\), for which Euclid's Algorithm terminates in \\(k\\) steps. The proof is based on the claim that, if \\((a_{k+1}, b_{k+1}) \to (a_k, b_k) \to (a_{k-1}, b_{k-1})\\) are three successive pairs in the reduction process, then we must have \\(b_{k+1} \ge b_k + b_{k-1}\\). To verify the claim, consider that a reduction step is defined by applying the transformation \\(a_{k-1} = b_k\\), \\(b_{k-1} = \\) remainder of \\(a_k\\) divided by \\(b_k\\). The second equation means that \\(a_k = qb_k + b_{k-1}\\) for some positive integer \\(q\\). And since \\(q\\) must be at least 1 we have \\(a_k = qb_k + b_{k-1} \ge b_k + b_{k-1}\\). But in the previous reduction step we have \\(b_{k+1} = a_k\\). Therefore, \\(b_{k+1} = a_k \ge b_k + b_{k-1}\\). This verifies the claim. Now we can prove the theorem by induction on k, the number of steps that the algorithm requires to terminate. The result is true for \\(k = 1\\), since this merely requires that \\(b\\) be at least as large as \\(Fib(1) = 1\\). Now, assume that the result is true for all integers less than or equal to \\(k\\) and establish the result for \\(k + 1\\). Let \\( (a_{k+1}, b_{k+1}) \to (a_k, b_k) \to (a_{k-1}, b_{k-1}) \\) be successive pairs in the reduction process. By our induction hypotheses, we have \\(b_{k-1} \ge Fib(k - 1) \\) and \\(b_k \ge Fib(k) \\). Thus, applying the claim we just proved together with the definition of the Fibonacci numbers gives \\( b_{k+1} \ge b_k + b_{k-1} \ge Fib(k) + Fib(k - 1) = Fib(k + 1) \\), which completes the proof of Lamé's Theorem.






