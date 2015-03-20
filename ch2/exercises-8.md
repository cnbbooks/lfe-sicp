### Exercises

#### Exercise 2.30

Define a function ``square-tree`` analogous to the ``square-list`` function of exercise 2.21. That is, ``square-list`` should behave as follows:

```lisp
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(1 (4 (9 16) 25) (36 49))
```

Define ``square-tree`` both directly (i.e., without using any higher-order procedures) and also by using ``map`` and recursion.


#### Exercise 2.31

Abstract your answer to exercise 2.30 to produce a function ``tree-map`` with the property that ``square-tree`` could be defined as

```lisp
(defun square-tree (tree)
  (tree-map square tree))
```

#### Exercise 2.32

We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is ``(1 2 3)```, then the set of all subsets is ``(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))``. Complete the following definition of a function that generates the set of subsets of a set and give a clear explanation of why it works:

```lisp
(defun subsets
  (('())
   '(()))
  (((cons _ tail))
   (let ((rest (subsets tail)))
     (append rest (lists:map <??> rest)))))
```