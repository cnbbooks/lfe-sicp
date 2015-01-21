### Exponentiation

Consider the problem of computing the exponential of a given number. We would like a procedure that takes as arguments a base $$b$$ and a positive integer exponent $$n$$ and computes $$b^n$$. One way to do this is via the recursive definition 

$$
\begin{align}
& b^n= b \cdot b^{n-1} \\
& b^0 = 1
\end{align}
$$

which translates readily into the procedure 

```lisp
(defun expt (b n)
  (if (== n 0)
      1
      (* b (expt b (- n 1)))))
```

This is a linear recursive process, which requires $$\Theta(n)$$ steps and $$\Theta(n)$$ space. Just as with factorial, we can readily formulate an equivalent linear iteration:

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

This version requires $$\Theta(n)$$ steps and $$\Theta(1)$$ space.