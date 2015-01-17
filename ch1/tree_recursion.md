### Tree Recursion

Another common pattern of computation is called tree recursion. As an example, consider computing the sequence of Fibonacci numbers, in which each number is the sum of the preceding two:

$$
0,\;1,\;1,\;2,\;3,\;5,\;8,\;13,\;21,\;34,\;55,\;89,\;144,\; \ldots\;
$$

```lisp
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
```

<a name="figure-5"></a>
![The tree-recursive process generated in computing (fib 5)](images/ch1-Z-G-13.png)

**Figure 1.5**:  The tree-recursive process generated in computing ``(fib 5)``.