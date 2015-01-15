### Exercises

#### Exercise 1.6

Bob Bitwright doesn't see why ``if`` needs to be provided as a special form. "Why can't I just define it as an ordinary procedure in terms of cond?" he asks. Bob's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of ``if``:

```lisp
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        ('true else-clause)))
```

Eva demonstrates the program for Bob:

```lisp
(new-if (== 2 3) 0 5)
5

(new-if (== 1 1) 0 5)
0
```

Delighted, Bob uses ``new-if`` to rewrite the square-root program:

```lisp
(defun sqrt (guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt (improve guess x)
                     x)))
```

What happens when Alyssa attempts to use this to compute square roots? Explain. 