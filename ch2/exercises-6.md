### Exercises

#### Exercise 2.21

The procedure ``square-list/1`` takes a list of numbers as argument and returns a list of the squares of those numbers.

```lisp
> (square-list (list 1 2 3 4))
(1 4 9 16)
```

Here are two different definitions of ``square-list/1``. Complete both of them by filling in the missing expressions:

```lisp
(defun square-list
  (('()) '())
  ((items) (cons <??> <??>)))
```
```lisp
(defun square-list (items)
  (map <??> <??>))
```

#### Exercise 2.22

Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves an iterative process:

```lisp
(defun square-list (items)
  (square-list items '())

(defun square-list
  (('() answer) answer) 
  ((things answer)
    (iter (cdr things) 
          (cons (square (car things))
                answer))))
```

Unfortunately, defining ``square-list`` this way produces the answer list in the reverse order of the one desired. Why?

Louis then tries to fix his bug by interchanging the arguments to cons:

```lisp
(defun square-list (items)
  (square-list items '())

(defun square-list
  (('() answer) answer) 
  ((things answer)
    (iter (cdr things) 
          (cons answer
                (square (car things))))))
```

This doesn't work either. Explain.

#### Exercise 2.23

The built-in LFE procedure ``lists:foreach/2`` is similar to our ``mapper/2`` and the built-in ``lists:map/2``. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, ``foreach/2`` just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all -- ``foreach/2`` is used with procedures that perform an action, such as printing. For example,

```lisp
> (lists:foreach (lambda (x) (io:format "~n~p~n (list x)))
                 (list 57 321 88))
57
321
88
```

The value returned by the call to ``foreach/2`` (not illustrated above) can be something arbitrary, such as ``true``. Give an implementation of ``foreach/2`` that mimics the behaviour of the built-in function.
