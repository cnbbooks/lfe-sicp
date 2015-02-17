#### List Operations

The use of pairs to represent sequences of elements as lists is accompanied by conventional programming techniques for manipulating lists by successively "``cdr``ing down" the lists. For example, the procedure ``list-ref/2`` takes as arguments a list and a number $$n$$ and returns the $$n$$th item of the list. In Erlang, and thus LFE, is customary to number the elements of the list beginning with 1. The method for computing ``list-ref/2`` is the following: [^1]

* For $$n = 1$$, ``list-ref/2`` should return the ``car`` of the list.
* Otherwise, ``list-ref/2`` should return the $$n$$th item of the ``cdr`` of the list.

```lisp
(defun list-ref (items n)
  (if (== n 1)
      (car items)
      (list-ref (cdr items) (- n 1))))
```
```lisp
> (set squares (list 1 4 9 16 25))
(1 4 9 16 25)
> (list-ref squares 4)
16
```

Often we "``cdr`` down" the whole list. The procedure ``len/1``,[^2] which returns the number of items in a list, illustrates this typical pattern of use:

```lisp
(defun len
  (('())
   0)
  ((items)
   (+ 1 (len (cdr items)))))
```
```lisp
> (set odds (list 1 3 5 7))
(1 3 5 7)
> (len odds)
4
```

The ``len/1`` procedure implements a simple recursive plan. The reduction step is:

* The length of any list is 1 plus the length of the ``cdr`` of the list.

This is applied successively until we reach the base case:

* The passed list matches the pattern of the empty list.

We could also compute length in an iterative style:

```lisp
(defun len (items)
  (len items 0))

(defun len
  (('() count)
   count)
  ((a count)
   (len (cdr a) (+ 1 count))))
```

Another conventional programming technique is to "``cons`` up" an answer list while ``cdr``ing down a list, as in the procedure ``append/2``,[^3] which takes two lists as arguments and combines their elements to make a new list:

```lisp
> (append squares odds)
(1 4 9 16 25 1 3 5 7)
> (append odds squares)
(1 3 5 7 1 4 9 16 25)
```

``append/2`` is also implemented using a recursive plan. To append lists ``list1`` and ``list2``, do the following:

* If ``list1`` is the empty list, then the result is just ``list2``.
* Otherwise, append the ``cdr`` of ``list1`` and ``list2``, and ``cons`` the ``car`` of ``list1`` onto the result:

```lisp
(defun append
  (('() list2)
   list2)
  ((list1 list2)
   (cons (car list1) (append (cdr list1) list2))))
```

----

[^1]: This function is provided for pedagogical purposes and that one should usually rely upon the function from the Erlang ``lists`` module: ``(lists:nth 4 squares)``.

[^2]: One should normally not define one's own ``len/1`` function, but instead use the built-in ``erlang:length/1`` function which is also available as simply ``length/1``.

[^3]: Similarly, the Erlang function ``lists:append/2`` (or ``lists:append/1``) generally should be used instead.
