### Exercises

#### Exercise 1.16

Design a function that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does ``fast-expt/2``. (Hint: Using the observation that $$(b^\frac{n}{2})^2 = (b^{2})^\frac{n}{2}$$, keep, along with the exponent $$n$$ and the base $$b$$, an additional state variable $$a$$, and define the state transformation in such a way that the product $$a \cdot b^n$$ is unchanged from state to state. At the beginning of the process $$a$$ is taken to be 1, and the answer is given by the value of $$a$$ at the end of the process. In general, the technique of defining an *invariant quantity* that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.) 


#### Exercise 1.17

The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication. In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication function (in which it is assumed that our language can only add, not multiply) is analogous to the ``expt`` function:

```lisp
(defun mult (a b)
  (if (== b 0)
      0
      (+ a (mult a (- b 1)))))
```

This algorithm takes a number of steps that is linear in ``b``. Now suppose we include, together with addition, operations ``double``, which doubles an integer, and ``halve``, which divides an (even) integer by 2. Using these, design a multiplication function analogous to ``fast-expt`` that uses a logarithmic number of steps.


#### Exercise 1.18.

Using the results of exercises 1.16 and 1.17 above, devise a function that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.[^1]


#### Exercise 1.19

There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps. Recall the transformation of the state variables $$a$$ and $$b$$ in the iterative version of ``fib/1`` (with ``fib/2``) in the section [Tree Recursion](): $$a \gets a + b$$ and $$b \gets a $$. Call this transformation $$T$$, and observe that applying $$T$$ over and over again $$n$$ times, starting with 1 and 0, produces the pair $$Fib(n + 1)$$ and $$Fib(n)$$. In other words, the Fibonacci numbers are produced by applying $$T^n$$, the $$n$$th power of the transformation $$T$$, starting with the pair $$(1,0)$$. Now consider $$T$$ to be the special case of $$p = 0$$ and $$q = 1$$ in a family of transformations $$T_{pq}$$, where $$T_{pq}$$ transforms the pair $$(a,b)$$ according to a $$a \gets bq + aq + ap$$ and $$b \gets bp + aq$$. Show that if we apply such a transformation $$T_{pq}$$ twice, the effect is the same as using a single transformation $$T_{p'q'}$$ of the same form, and compute $$p'$$ and $$q'$$ in terms of $$p$$ and $$q$$. This gives us an explicit way to square these transformations, and thus we can compute $$T^n$$ using successive squaring, as in the ``fast-expt/2`` function. Put this all together to complete the following functions, which run in a logarithmic number of steps:[^2]

```lisp
(defun fib (n)
  (fib 1 0 0 1 n))

(defun fib (a b p q count)
  (cond ((== count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>      ; compute p'
                   <??>      ; compute q'
                   (/ count 2)))
        ('true (iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))
```

----

[^1]: This algorithm, which is sometimes known as the "Russian peasant method" of multiplication, is ancient. Examples of its use are found in the Rhind Papyrus, one of the two oldest mathematical documents in existence, written about 1700 B.C. (and copied from an even older document) by an Egyptian scribe named A'h-mose. 

[^2]: This exercise was suggested to us by Joe Stoy, based on an example in Kaldewaij 1990.






