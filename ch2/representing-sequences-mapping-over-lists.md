#### Mapping over lists

One extremely useful operation is to apply some transformation to each element in a list and generate the list of results. For instance, the following function scales each number in a list by a given factor:

```lisp
(defun scale-list
  (('() _)
   '())
  ((items factor)
   (cons (* (car items) factor)
         (scale-list (cdr items) factor))))
```
```lisp
> (scale-list (list 1 2 3 4 5) 10)
(10 20 30 40 50)
```

We can abstract this general idea and capture it as a common pattern expressed as a higher-order function, just as in the section [Formulating Abstractions with Higher-Order Functions](). The higher-order function here is called ``mapper/2``.[^1] ``mapper/2`` takes as arguments a function of one argument and a list, and returns a list of the results produced by applying the function to each element in the list:[^2]

```lisp
(defun mapper
  ((_ '())
   '())
  ((func items)
   (cons (funcall func (car items))
         (mapper func (cdr items)))))
```
```lisp
> (mapper #'abs/1 (list -10 2.5 -11.6 17))
(10 2.5 11.6 17)
> (mapper (lambda (x) (* x x))
     (list 1 2 3 4))
(1 4 9 16)
```

Now we can give a new definition of ``scale-list/2`` in terms of map:

```lisp
(defun scale-list (items factor)
  (mapper
    (lambda (x) (* x factor))
    items))
```
       
``mapper/2`` is an important construct, not only because it captures a common pattern, but because it establishes a higher level of abstraction in dealing with lists. In the original definition of ``scale-list/2``, the recursive structure of the program draws attention to the element-by-element processing of the list. Defining ``scale-list/2`` in terms of ``mapper/2`` suppresses that level of detail and emphasizes that scaling transforms a list of elements to a list of results. The difference between the two definitions is not that the computer is performing a different process (it isn't) but that we think about the process differently. In effect, ``mapper/2`` helps establish an abstraction barrier that isolates the implementation of functions that transform lists from the details of how the elements of the list are extracted and combined. Like the barriers shown in [abstraction-barriers.html#figure-1](figure 2.1), this abstraction gives us the flexibility to change the low-level details of how sequences are implemented, while preserving the conceptual framework of operations that transform sequences to sequences. The section [Sequences as Conventional Interfaces]() expands on this use of sequences as a framework for organizing programs.

----

[^1]: Functions with this behaviour are ordinarily called ``map``, however the ``map`` name in LFE is already taken for creating Erlang map data structures.

[^2]: The canonical ``map`` function used in LFE is ``lists:map/2``.

