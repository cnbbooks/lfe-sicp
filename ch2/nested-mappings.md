#### Nested mappings

We can extend the sequence paradigm to include many computations that are commonly expressed using nested loops.[^1] Consider this problem: Given a positive integer $$n$$, find all ordered pairs of distinct positive integers $$i$$ and $$j$$, where $$1< j< i< n$$, such that $$i + j$$ is prime. For example, if $$n$$ is 6, then the pairs are the following:

$$
\begin{array}{c|c}
  i     & 2 \ 3 \ 4 \ 4 \ 5 \ 6 \ \; 6 \\
  j     & 1 \ 2 \ 1 \ 3 \ 2 \ 1 \ \; 5 \\
  \hline
  i + j & \, \, 3 \ 5 \ 5 \ 7 \ 7 \ 7 \ 11 \\
\end{array}
$$

A natural way to organize this computation is to generate the sequence of all ordered pairs of positive integers less than or equal to $$n$$, filter to select those pairs whose sum is prime, and then, for each pair $$(i, j)$$ that passes through the filter, produce the triple $$(i,j,i + j)$$.

Here is a way to generate the sequence of pairs: For each integer $$i< n$$, enumerate the integers $$j<i$$, and for each such $$i$$ and $$j$$ generate the pair $$(i,j)$$. In terms of sequence operations, we map along the sequence ``(enumerate-interval 1 n)``. For each ``i`` in this sequence, we map along the sequence ``(enumerate-interval 1 (- i 1))``. For each ``j`` in this latter sequence, we generate the pair ``(list i j)``. This gives us a sequence of pairs for each ``i``. Combining all the sequences for all the ``i`` (by accumulating with append) produces the required sequence of pairs: [^2]

```lisp
(accumulate #'append/2
            '()
            (mapper (lambda (i)
                    (mapper (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n)))
```

The combination of mapping and accumulating with append is so common in this sort of program that we will isolate it as a separate procedure:

```lisp
(defun flatmap (func seq)
  (accumulate append '() (mapper func seq)))
```

Now filter this sequence of pairs to find those whose sum is prime. The filter predicate is called for each element of the sequence; its argument is a pair and it must extract the integers from the pair. Thus, the predicate to apply to each element in the sequence is

```lisp
(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))
```

Finally, generate the sequence of results by mapping over the filtered pairs using the following procedure, which constructs a triple consisting of the two elements of the pair along with their sum:

```lisp
(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
```

Combining all these steps yields the complete procedure:

```lisp
(defun prime-sum-pairs (n)
  (mapper #'make-pair-sum/1
          (filter #'prime-sum?/1
                  (flatmap
                  (lambda (i)
                    (map (lambda (j) (list i j))
                         (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n)))))
```

Nested mappings are also useful for sequences other than those that enumerate intervals. Suppose we wish to generate all the permutations of a set S; that is, all the ways of ordering the items in the set. For instance, the permutations of {1,2,3} are {1,2,3}, { 1,3,2}, {2,1,3}, { 2,3,1}, { 3,1,2}, and { 3,2,1}. Here is a plan for generating the permutations of $$S$$: For each item $$x$$ in $$S$$, recursively generate the sequence of permutations of $$S - x$$,[^3] and adjoin $$x$$ to the front of each one. This yields, for each $$x$$ in $$S$$, the sequence of permutations of $$S$$ that begin with $$x$$. Combining these sequences for all $$x$$ gives all the permutations of $$S$$: [^4]

```lisp
(defun permutations
  (('())                     ; empty set?
   (list '()))               ; sequence containing empty set
  ((s)
   (flatmap (lambda (x)
              (map (lambda (p) (cons x p))
                   (permutations (remove x s))))
            s)))
```

Notice how this strategy reduces the problem of generating permutations of $$S$$ to the problem of generating the permutations of sets with fewer elements than $$S$$. In the terminal case, we work our way down to the empty list, which represents a set of no elements. For this, we generate ``(list '())``, which is a sequence with one item, namely the set with no elements. The remove procedure used in permutations returns all the items in a given sequence except for a given item. This can be expressed as a simple filter:

```lisp
(defun remove (item sequence)
  (filter (lambda (x) (not (=:= x item)))
          sequence))
```
          

----

[^1]: This approach to nested mappings was shown to us by David Turner, whose languages KRC and Miranda provide elegant formalisms for dealing with these constructs. The examples in this section (see also exercise 2.42) are adapted from Turner 1981. In the section [Exploiting the Stream Paradigm](), we'll see how this approach generalizes to infinite sequences.

[^2]: We're representing a pair here as a list of two elements rather than as a Lisp pair. Thus, the "pair" ``(i,j)`` is represented as ``(list i j)``, not ``(cons i j)``.

[^3]: The set $$S - x$$ is the set of all elements of $$S$$, excluding $$x$$.

[^4]: Semicolons in LFE code are used to introduce comments. Everything from the semicolon to the end of the line is ignored by the interpreter. In this book we don't use many comments; we try to make our programs self-documenting by using descriptive names.



