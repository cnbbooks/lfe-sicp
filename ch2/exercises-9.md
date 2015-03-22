### Exercises

#### Exercise 2.33

Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

```lisp
(defun map (p seq)
  (accumulate (lambda (x y) <??>) '() seq))

(defun append (seq1 seq2)
  (accumulate #'cons/2 <??> <??>))

(defun length (seq)
  (accumulate <??> 0 seq))
```

#### Exercise 2.34

#### Exercise 2.35

#### Exercise 2.36

#### Exercise 2.37

#### Exercise 2.38

#### Exercise 2.39