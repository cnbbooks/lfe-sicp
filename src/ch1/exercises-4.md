### Exercises

####  Exercise 1.11

A function \\(f\\) is defined by the rule that \\(f(n) = n\\) if \\(n < 3\\) and \\(f(n) = f(n - 1) + 2 \cdot f(n - 2) + 3 \cdot f(n - 3)\\) if \\(n > 3\\). Write a function that computes \\(f\\) by means of a recursive process. Write a function that computes \\(f\\) by means of an iterative process. 

####  Exercise 1.12

The following pattern of numbers is called *Pascal's triangle*.

\\[
\begin{array}{ccccccccc}
  &     &     &      & 1     &      &      &     &   \\\\
  &     &     & 1    &       &  1   &      &     &   \\\\
  &     & 1   &      & 2     &      & 1    &     &   \\\\
  & 1   &     & 3    &       &  3   &      & 1   &   \\\\
1 &     & 4   &      & 6     &      & 4    &     & 1 \\\\
  &     &     &      & \dots &      &      &     &   \\\\
\end{array}
\\]

The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.[^1] Write a function that computes elements of Pascal's triangle by means of a recursive process. 

#### Exercise 1.13

Prove that \\(Fib(n)\\) is the closest integer to \\( \frac{\phi^n}{\sqrt 5} \\), where \\( \phi = \frac{1 + \sqrt 5}{2} \\). Hint: Let \\( \psi = \frac{1 - \sqrt 5}{2} \\). Use induction and the definition of the Fibonacci numbers (see the section [Tree Recursion]()) to prove that \\(Fib(n) = \frac{\phi^n - \psi^n}{\sqrt5}\\). 


----

[^1]: The elements of Pascal's triangle are called the *binomial coefficients*, because the nth row consists of the coefficients of the terms in the expansion of \\((x + y)^n\\). This pattern for computing the coefficients appeared in Blaise Pascal's 1653 seminal work on probability theory, *Traité du triangle arithmétique*. According to Knuth (1973), the same pattern appears in the *Szu-yuen Yü-chien* ("The Precious Mirror of the Four Elements"), published by the Chinese mathematician Chu Shih-chieh in 1303, in the works of the twelfth-century Persian poet and mathematician Omar Khayyam, and in the works of the twelfth-century Hindu mathematician Bháscara Áchárya. 
