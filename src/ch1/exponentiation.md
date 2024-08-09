### Exponentiation

Consider the problem of computing the exponential of a given number. We would like a function that takes as arguments a base \\(b\\) and a positive integer exponent \\(n\\) and computes \\(b^n\\). One way to do this is via the recursive definition

\\[
\begin{align}
& b^n = b \cdot b^{n-1} \\\\
& b^0 = 1
\end{align}
\\]

which translates readily into the function

```lisp
(defun expt (b n)
  (if (== n 0)
      1
      (* b (expt b (- n 1)))))
```

This is a linear recursive process, which requires \\(\Theta(n)\\) steps and \\(\Theta(n)\\) space. Just as with factorial, we can readily formulate an equivalent linear iteration:

```lisp
(defun expt (b n)
  (expt b n 1))

(defun expt (b counter product)
  (if (== counter 0)
      product
      (expt b
            (- counter 1)
            (* b product))))
```

This version requires \\(\Theta(n)\\) steps and \\(\Theta(1)\\) space.

We can compute exponentials in fewer steps by using successive squaring. For instance, rather than computing \\(b^8\\) as

\\[
b \cdot (b \cdot (b \cdot (b \cdot (b \cdot (b \cdot (b \cdot b))))))
\\]

we can compute it using three multiplications:

\\[
\begin{align}
& b^2 = b \cdot b \\\\
& b^4 = b^2 \cdot b^2 \\\\
& b^8 = b^4 \cdot b^4 \\\\
\end{align}
\\]

This method works fine for exponents that are powers of 2. We can also take advantage of successive squaring in computing exponentials in general if we use the rule

\\[
\begin{align}
& b^n = (b^\frac{n}{2})^2 & \mbox{if } n \ \text{is even} \\\\
& b^n = b \cdot b^{n-1} & \mbox{if } n \ \text{is odd} \\\\
\end{align}
\\]

We can express this method as a function:

```lisp
(defun fast-expt (b n)
  (cond ((== n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        ('true (* b (fast-expt b (- n 1))))))
```

where the predicate to test whether an integer is even is defined in terms of the primitive function ``rem`` by

```lisp
(defun even? (n)
  (=:= 0 (rem (trunc n) 2)))
```

The process evolved by ``fast-expt`` grows logarithmically with \\(n\\) in both space and number of steps. To see this, observe that computing \\(b^{2n}\\) using ``fast-expt`` requires only one more multiplication than computing \\(b^n\\). The size of the exponent we can compute therefore doubles (approximately) with every new multiplication we are allowed. Thus, the number of multiplications required for an exponent of n grows about as fast as the logarithm of \\(n\\) to the base 2. The process has \\(\Theta(\log n)\\) growth.[^1]

The difference between \\(\Theta(\log n)\\) growth and \\(\Theta(n)\\) growth becomes striking as \\(n\\) becomes large. For example, ``fast-expt`` for \\(n = 1000\\) requires only 14 multiplications.[^2] It is also possible to use the idea of successive squaring to devise an iterative algorithm that computes exponentials with a logarithmic number of steps (see exercise 1.16), although, as is often the case with iterative algorithms, this is not written down so straightforwardly as the recursive algorithm.[^3]

----

[^1]: More precisely, the number of multiplications required is equal to 1 less than the log base 2 of \\(n\\) plus the number of ones in the binary representation of \\(n\\). This total is always less than twice the log base 2 of \\(n\\). The arbitrary constants \\(k_1\\) and \\(k_2\\) in the definition of order notation imply that, for a logarithmic process, the base to which logarithms are taken does not matter, so all such processes are described as \\(\Theta(\log n)\\).

[^2]: You may wonder why anyone would care about raising numbers to the 1000th power. See the section [Example: Testing for Primality]().

[^3]: This iterative algorithm is ancient. It appears in the Chandah-sutra by Áchárya Pingala, written before 200 B.C. See Knuth 1981, section 4.6.3, for a full discussion and analysis of this and other methods of exponentiation.




