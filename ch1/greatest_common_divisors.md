### Greatest Common Divisors

The greatest common divisor (GCD) of two integers $$a$$ and $$b$$ is defined to be the largest integer that divides both $$a$$ and $$b$$ with no remainder. For example, the GCD of 16 and 28 is 4. In chapter 2, when we investigate how to implement rational-number arithmetic, we will need to be able to compute GCDs in order to reduce rational numbers to lowest terms. (To reduce a rational number to lowest terms, we must divide both the numerator and the denominator by their GCD. For example, 16/28 reduces to 4/7.) One way to find the GCD of two integers is to factor them and search for common factors, but there is a famous algorithm that is much more efficient.

The idea of the algorithm is based on the observation that, if $$r$$ is the remainder when $$a$$ is divided by $$b$$, then the common divisors of $$a$$ and $$b$$ are precisely the same as the common divisors of $$b$$ and $$r$$. Thus, we can use the equation

$$
\DeclareMathOperator*{\gcd}{GCD}
\gcd(a, b) = \gcd(b, r)
$$

to successively reduce the problem of computing a GCD to the problem of computing the GCD of smaller and smaller pairs of integers. For example,

$$
\begin{align}
\DeclareMathOperator*{\gcd}{GCD}
\gcd(206, 40) = \gcd(40, 6) \\
= \gcd(6, 4) \ \ \\
= \gcd(4, 2) \ \ \\
= \gcd(2, 0) \ \ \\
= 2\;\;\;\;\;\;\;\;\;\;\;\;\:\,
\end{align}
$$
