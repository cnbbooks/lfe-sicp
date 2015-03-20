#### Mapping over trees

Just as ``map`` is a powerful abstraction for dealing with sequences, ``map`` together with recursion is a powerful abstraction for dealing with trees. For instance, the ``scale-tree/2`` function, analogous to ``scale-list/2`` of the section [Representing Sequences](), takes as arguments a numeric factor and a tree whose leaves are numbers. It returns a tree of the same shape, where each number is multiplied by the factor. The recursive plan for ``scale-tree/2`` is similar to the one for ``count-leaves/2``:

```lisp
(defun scale-tree
  (('() _)
   '())
  (((cons head tail) factor)
   (cons (scale-tree head factor)
         (scale-tree tail factor)))
  ((tree factor)
    (* tree factor)))
```

```lisp
> (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 100)
(100 (200 (300 400) 500) (600 700))
```

Another way to implement ``scale-tree/2`` is to regard the tree as a sequence of sub-trees and use ``map``. We map over the sequence, scaling each sub-tree in turn, and return the list of results. In the base case, where the tree is a leaf, we simply multiply by the factor:

```lisp
(defun scale-tree (tree factor)
  (lists:map #'scale-sub-tree/1 tree))
       
(defun scale-sub-tree
  ((sub-tree) (when (is_integer sub-tree))
   (* sub-tree factor))
  ((sub-tree)
   (scale-tree sub-tree factor)))
```

```lisp
> (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 100)
(100 (200 (300 400) 500) (600 700))
```

Many tree operations can be implemented by similar combinations of sequence operations and recursion.

