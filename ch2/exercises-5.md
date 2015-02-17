### Exercises

#### Exercise 2.17

Define a procedure ``last-pair/1`` that returns the list that contains only the last element of a given (nonempty) list:

```lisp
> (last-pair (list 23 72 149 34))
(34)
```

#### Exercise 2.18

Define a procedure ``reverse/1`` that takes a list as argument and returns a list of the same elements in reverse order:

```lisp
> (reverse (list 1 4 9 16 25))
(25 16 9 4 1)
```

#### Exercise 2.19

Consider the change-counting program of the section [Tree Recursion](). It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure ``first-denomination/1`` and partly into the procedure ``count-change/1`` (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

We want to rewrite the procedure ``cc/2`` so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:

```lisp
> (set us-coins (list 50 25 10 5 1))
(50 25 10 5 1)
> (set uk-coins (list 100 50 20 10 5 2 1 0.5))
(100 50 20 10 5 2 1 0.5)
```
We could then call ``cc/2`` as follows:

```lisp
> (cc 100 us-coins)
292
```

To do this will require changing the program ``cc/2`` somewhat. It will still have the same form, but it will access its second argument differently, as follows:

```lisp
(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        ('true
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
```
                
Define the procedures ``first-denomination/1``, ``except-first-denomination/1``, and ``no-more?/1`` in terms of primitive operations on list structures. Does the order of the list coin-values affect the answer produced by ``cc/2``? Why or why not?

#### Exercise 2.20

[Removed from the LFE Edition.[^1] ]

----

[^1]: In the Scheme 2nd edition, [Exercise 2.20](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html) covers a feature in Scheme that is not present in LFE. The analog would require creating a macro with a single parameter that is the body of the code; as we haven't covered quoting or macros yet, this is not a suitable exercise at this point.
