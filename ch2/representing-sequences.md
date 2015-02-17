### Representing Sequences

<a name="figure-4"></a>

![The sequence 1, 2, 3, 4 represented as a chain of pairs](images/ch2-Z-G-13.png)

**Figure 2.4**: The sequence 1, 2, 3, 4 represented as a chain of pairs.

One of the useful structures we can build with pairs is a *sequence* -- an ordered collection of data objects. There are, of course, many ways to represent sequences in terms of pairs. One particularly straightforward representation is illustrated in the figure above, where the sequence 1, 2, 3, 4 is represented as a chain of pairs. The ``car`` of each pair is the corresponding item in the chain, and the ``cdr`` of the pair is the next pair in the chain. The ``cdr`` of the final pair signals the end of the sequence by pointing to a distinguished value that is not a pair, represented in box-and-pointer diagrams as a diagonal line and in programs as the value of the empty list.[^1] The entire sequence is constructed by nested ``cons`` operations:

```lisp
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
```

Such a sequence of pairs, formed by nested ``cons``es, is called a *list*, and Scheme provides a primitive called list to help in constructing lists.[^2] The above sequence could be produced by ``(list 1 2 3 4)``. In general,

```lisp
(list <a1> <a2> ... <an>)
```

is equivalent to

```lisp
(cons <a1> (cons <a2> (cons ... (cons <an> '()) ... ))
```

Lisp systems conventionally print lists by printing the sequence of elements, enclosed in parentheses. Thus, the data object in figure 2.4 is printed as ``(1 2 3 4)``:

```lisp
> (set one-through-four (list 1 2 3 4))
(1 2 3 4)
```

Be careful not to confuse the expression ``(list 1 2 3 4)`` with the list ``(1 2 3 4)``, which is the result obtained when the expression is evaluated. Attempting to evaluate the expression ``(1 2 3 4)`` will signal an error when the interpreter tries to apply the procedure ``1/3`` to arguments ``2``, ``3``, and ``4``.

We can think of ``car`` as selecting the first item in the list, and of ``cdr`` as selecting the sublist consisting of all but the first item. Nested applications of ``car`` and ``cdr`` can be used to extract the second, third, and subsequent items in the list.[^3] The constructor ``cons`` makes a list like the original one, but with an additional item at the beginning.

```lisp
> (car one-through-four)
1
> (cdr one-through-four)
(2 3 4)
> (car (cdr one-through-four))
2
> (cons 10 one-through-four)
(10 1 2 3 4)
> (cons 5 one-through-four)
(5 1 2 3 4)
```

----

[^1]: The empty list will be discussed more in the section [Symbolic Data]() when we cover the topic of quoting.

[^2]: In this book, we use *list* to mean a chain of pairs terminated by the end-of-list marker. In contrast, the term *list structure* refers to any data structure made out of pairs, not just to lists.

[^3]: Since nested applications of ``car`` and ``cdr`` are cumbersome to write, Lisp dialects provide abbreviations for them -- for instance, ``(cadr <arg>)`` is equivalent to ``(car (cdr <arg>))``. The names of all such procedures start with ``c`` and end with ``r``. Each ``a`` between them stands for a ``car`` operation and each ``d`` for a ``cdr`` operation, to be applied in the same order in which they appear in the name. The names ``car`` and ``cdr`` persist because simple combinations like cadr are pronounceable.
