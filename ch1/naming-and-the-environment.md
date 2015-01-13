### Naming and the Environment

A critical aspect of a programming language is the means it provides for using names to refer to computational objects. We say that the name identifies a *variable* whose *value* is the object.

In the LFE dialect of Lisp, we name things using functions (macros, really; we'll get to that later). In the REPL, we can assign values to variables using ``set``:

```lisp
> (set size 2)
2
```

causes the interpreter to associate the value ``2`` with the name ``size``. Once the name size has been associated with the number ``2``, we can refer to the value ``2`` by name:

```lisp
> size
2
> (* 5 size)
10
```

Here are further examples of the use of ``set``:

```lisp
> (set pi 3.14159)
3.14159
> (set radius 10)
10
> (* pi (* radius radius))
314.159
> (set circumference (* 2 pi radius))
62.8318
> circumference
62.8318
```
 
When not in the REPL, we can use such things as the ``(let ...)`` form to locally assign values to variables.

Though one could also use ``set`` do define a *function* in the REPL

```lisp
> (set identity (lambda (x) x))
#Fun<lfe_eval.12.86468545>
> (funcall identity 2)
2
```

it is more idiomatic to use the ``defun`` macro:

```lisp
> (defun identity (x) x)
identity
> (identity 2)
2
```

And, as you can see, it's a bit easier to use.

The ``defun`` macro is a convenience macro for ``define-function``. These two, and in the REPL, ``set``, are examples of the sorts of basic tools of abstraction available to the LFE programmer. These allow us to use simple names to refer to the results of compound operations, such as the ``circumference`` computed above. In general, computational objects may have very complex structures, and it would be extremely inconvenient to have to remember and repeat their details each time we want to use them. Indeed, complex programs are constructed by building, step by step, computational objects of increasing complexity. The interpreter makes this step-by-step program construction particularly convenient because name-object associations can be created incrementally in successive interactions. This feature encourages the incremental development and testing of programs and is largely responsible for the fact that an LFE program usually consists of a large number of relatively simple procedures.

It should be clear that the possibility of associating values with symbols and later retrieving them means that the interpreter must maintain some sort of memory that keeps track of the name-object pairs. This memory is called the *environment* (more precisely the *global environment*, since we will see later that a computation may involve a number of different environments).[^1]



----

[^1]: Chapter 11 will show that this notion of environment is crucial, both for understanding how the interpreter works and for implementing interpreters. 


