### Exercises

#### Exercise 1.34

Suppose we define the function

```lisp
(defun f (g)
  (funcall g 2))
```

Then we have

```lisp
> (f #'square/1)
4
> (f (lambda (z) (* z (+ z 1))))
6
```

 What happens if we (perversely) ask the interpreter to evaluate the combination ``(f #'f/1)``? Explain.
