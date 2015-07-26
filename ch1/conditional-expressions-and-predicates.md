### Conditional Expressions and Predicates

The expressive power of the class of functions that we can define at this point is very limited, because we have no way to make tests and to perform different operations depending on the result of a test. For instance, we cannot define a function that computes the absolute value of a number by testing whether the number is positive, negative, or zero and taking different actions in the different cases according to the rule

$$
\begin{align}
\mid \ r \mid \ = \ \left\{
\begin{array}{1 1}
\ \ r \quad \text{if } r > 0 \\
\ \ 0 \quad \text{if } r = 0 \\
-r \quad \text{if } r < 0
\end{array}
\right.\
\end{align}
$$

This construct is called a *case analysis*, and there are special forms in LFE for notating such case analyses:

* ``cond``
* ``if``
* ``case``
* pattern matching and guards

We will now explore those.

#### The ``cond`` Form

It is called ``cond`` (which stands for "conditional"), and it is used as follows:

```lisp
(defun abs (x)
  (cond ((> x 0) x)
        ((== x 0) 0)
        ((< x 0) (- x))))
```

In non-LFE Lisps, the general form of a conditional expression is

```lisp
(cond (<p1> <e1>)
      (<p2> <e2>)
      (<p3> <e3>)
      ...
      (<pn> <en>))
```

consisting of the symbol cond followed by parenthesized pairs of expressions ``(<p> <e>)`` called clauses. The first expression in each pair is a predicate -- that is, an expression whose value is interpreted as either true or false.[^1]

Thanks to that fact that Erlang underlies LFE, the LFE ``cond`` supports both pattern-matching and what Erlang calls "[guards](http://learnyousomeerlang.com/syntax-in-functions#guards-guards)". As such, the predicates in LFE ``cond``s may take the following additional forms:

```lisp
(cond (<p1> <e1>)
      ((?= <pattern2> <p2>) <e2>)
      ((?= <pattern3> <guard3> <p3>) <e3>)
      ...
      ((?= <patternn> <guardn> <pn>) <en>))
```

Conditional expressions are evaluated as follows. The predicate ``<p1>`` is evaluated first. If its value is false, then``<p2>`` is evaluated. If ``<p2>``'s value is also false, then ``<p3>`` is evaluated. This process continues until a predicate is found whose value is true, in which case the interpreter returns the value of the corresponding *consequent expression* ``<e>`` of the clause as the value of the conditional expression. If none of the ``<p>``'s is found to be ``true``, the value of the ``cond`` is ``false``.

The word *predicate* is used for functions that return ``true`` or ``false``, as well as for expressions that evaluate to ``true`` or ``false``. The absolute-value function ``abs`` makes use of the primitive predicates ``>``, ``<``, and ``==``.[^2] These take two numbers as arguments and test whether the first number is, respectively, greater than, less than, or equal to the second number, returning true or false accordingly.

Another way to write the absolute-value function is

```lisp
(defun abs (x)
  (cond ((< x 0) (- x))
        ('true x)))
```

which could be expressed in English as "If ``x`` is less than zero return ``-x``; otherwise return ``x``." Since in LFE a ``cond`` with no ``true`` predicates returns ``false``, if we want a final, "default" value, we need to provide a predicate that always evaluates to ``true``. The simplest such predicate is ``true``.

#### The ``if`` Form

Another condition form available to most Lisps and to LFE is ``if``. Here is yet another way to write the absolute-value function:

```lisp
(defun abs (x)
  (if (< x 0)
      (- x)
      x))
```

This uses the special form ``if``, a restricted type of conditional that can be used when there are precisely two cases in the case analysis. The general form of an if expression is

```lisp
(if <predicate> <consequent> <alternative>)
```

To evaluate an ``if`` expression, the interpreter starts by evaluating the ``<predicate>`` part of the expression. If the ``<predicate>`` evaluates to a ``true`` value, the interpreter then evaluates the ``<consequent>`` and returns its value. Otherwise it evaluates the ``<alternative>`` and returns its value.[^3]


#### The ``case`` Form

Through Erlang, LFE supports a form not found by default in most Lisps: ``case``. ``case`` takes an expression and then provides conditions based on matches for that expression. Here is the general form:

```lisp
(case <expression>
  (<pattern1> <e1>)
  (<pattern2> <guard2> <e2>)
  ...
  (<patternn> <guardn> <en>))
```

We could rewrite our absolute-value function using ``case`` like this:

```lisp

(defun abs (x)
  (case (< x 0)
        ('true (- x))
        (_ x)))
```

When the final pattern matched against is the "I-don't-care variable",[^4] the effect is the same as the final ``'true`` in the ``cond`` form: if all else fails to match, the expression associated with the ``_`` pattern is returned.

#### Function Argument Patterns

In our discussion of conditionals, we would be remiss in our duties if we did not bring up the topic of pattern-matching in function arguments, or in this case, patterns and guards.

Ordinarily in LFE you define a function as we have discussed, using the form

```lisp
(defun <name> (<arguments>) <body>)
```

However, like Erlang, LFE supports pattern matching and guards in functions. The more general form of function definition is

```lisp
(defun <name>
  ((<argpattern1>) <body1>)
  ((<argpattern2>) <guard2> <body2>)
  ...
  ((<argpatternn>) <guardn> <bodyn>))
```

We can rewrite our absolute-value function using a simple pattern and guards

```lisp
(defun abs
  ((x) (when (> x 0)) x)
  ((x) (when (== x 0)) 0)
  ((x) (when (< x 0)) (- x)))
```

which of course could be consolidated to

```lisp
(defun abs
  ((x) (when (< x 0)) (- x))
  ((x) x))
```

Note that in both definitions above our argument pattern is simply ``x``. We are not using the mechanics of pattern matching, per se, to implement our conditional logic. Rather, in this case we are taking advantage of the argument pattern's optional guard.


#### Logical Operators as Predicates

In addition to primitive predicates such as ``<``, ``=``, and ``>``, there are logical composition operations, which enable us to construct compound predicates. The three most frequently used are these:

* ``(and <e1> ... <en>)`` -
  The interpreter evaluates the expressions ``<e>`` one at a time, in left-to-right order. If any ``<e>`` evaluates to ``false``, the value of the ``and`` expression is ``false``, and the rest of the ``<e>``'s are not evaluated. If all ``<e>``'s evaluate to ``true`` values, the value of the ``and`` expression is ``true``.
* ``(or <e1> ... <en>)`` -
   The interpreter evaluates the expressions ``<e>`` one at a time, in left-to-right order. If any ``<e>`` evaluates to a ``true`` value, the value of the ``or`` expression is ``true``, and the rest of the ``<e>``'s are not evaluated. If all ``<e>``'s evaluate to ``false``, the value of the ``or`` expression is ``false``.
* ``(not <e>)`` -
  The value of a ``not`` expression is ``true`` when the expression ``<e>`` evaluates to ``false``, and ``false`` otherwise.

Notice that ``and`` and ``or`` are special forms, not functions, because the subexpressions are not necessarily all evaluated. ``not`` is an ordinary function.

As an example of how these are used, the condition that a number $$x$$ be in the range $$5 < x < 10$$ may be expressed as

```lisp
(and (> x 5) (< x 10))
```

As another example, we can define a predicate to test whether one number is greater than or equal to another as

```lisp
(defun gte (x y)
  (or (> x y) (== x y)))
```

or alternatively as

```lisp
(defun gte (x y)
  (not (< x y)))
```

----

[^1]: "Interpreted as either true or false" means this: In LFE, there are two distinguished values that are denoted by ``true`` and ``false``. When the interpreter checks a predicate's value, if the result cannot be interpreted as either ``true`` or ``false``, an error is raised (for ``cond`` and ``if`` you will see an ``if_clause`` exception).

[^2]: ``abs`` also uses the "minus" operator ``-``, which, when used with a single operand, as in ``(- x)``, indicates negation.

[^3]: A minor difference between ``if`` and ``cond`` is that the ``<e>`` part of each ``cond`` clause may be a sequence of expressions. If the corresponding ``<p>`` is found to be ``true``, the expressions ``<e>`` are evaluated in sequence and the value of the final expression in the sequence is returned as the value of the ``cond``. In an ``if`` expression, however, the ``<consequent>`` and ``<alternative>`` must be single expressions.

[^4]: The single underscore, anonymous variable, "blah", "don't care", or "throw-away" variable has a long history in computing. Many languages, including Prolog, C, Erlang, OCaml, Python, Ruby, etc., share a tradition of treating the underscore as a special variable; special in the sense that the value is not seen as being of any pertinent interest in the given context. In Prolog and Erlang, and thus LFE, the anonymous variable has a significant functional purpose: it is never bound to a value and as such can be used multiple times in pattern matching without throwing an error (which would happen in Prolog or Erlang if you tried to match multiple patterns with a regular, bound variable).




