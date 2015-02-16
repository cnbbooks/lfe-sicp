### What Is Meant by Data?

We began the rational-number implementation in the section [Example: Arithmetic Operations for Rational Numbers]() by implementing the rational-number operations ``add-rat/2``, ``sub-rat/2``, and so on in terms of three unspecified functions: ``make-rat/2``, ``numer/1``, and ``denom/1``. At that point, we could think of the operations as being defined in terms of data objects -- numerators, denominators, and rational numbers -- whose behavior was specified by the latter three functions.

But exactly what is meant by *data*? It is not enough to say "whatever is implemented by the given selectors and constructors." Clearly, not every arbitrary set of three functions can serve as an appropriate basis for the rational-number implementation. We need to guarantee that, if we construct a rational number ``x`` from a pair of integers ``n`` and ``d``, then extracting the ``numer`` and the ``denom`` of ``x`` and dividing them should yield the same result as dividing ``n`` by ``d``. In other words, ``make-rat/2``, ``numer/1``, and ``denom/1`` must satisfy the condition that, for any integer ``n`` and any non-zero integer ``d``, if ``x`` is ``(make-rat n d)``, then

$$
\begin{align}
\frac{\text{(numer x)}}{\text{(denom x)}} = \frac{\text{n}}{\text{d}}
\end{align}
$$

In fact, this is the only condition ``make-rat/2``, ``numer/1``, and ``denom/1`` must fulfill in order to form a suitable basis for a rational-number representation. In general, we can think of data as defined by some collection of selectors and constructors, together with specified conditions that these functions must fulfill in order to be a valid representation.[^1]

This point of view can serve to define not only "high-level" data objects, such as rational numbers, but lower-level objects as well. Consider the notion of a pair, which we used in order to define our rational numbers. We never actually said what a pair was, only that the language supplied functions ``cons``, ``car``, and ``cdr`` for operating on pairs. But the only thing we need to know about these three operations is that if we glue two objects together using ``cons`` we can retrieve the objects using ``car`` and ``cdr``. That is, the operations satisfy the condition that, for any objects ``x`` and ``y``, if ``z`` is ``(cons x y)`` then ``(car z)`` is ``x`` and ``(cdr z)`` is ``y``. Indeed, we mentioned that these three functions are included as primitives in our language. However, any triple of functions that satisfies the above condition can be used as the basis for implementing pairs. This point is illustrated strikingly by the fact that we could implement ``cons``, ``car``, and ``cdr`` without using any data structures at all but only using functions. Here are the definitions [^2]<sup>,</sup> [^3]:

```lisp
(defun cons2 (x y)
  (match-lambda
    ((0) x)
    ((1) y)
    ((n) (error (++ "cons2: argument must be 0 or 1 -- got "
                    (integer_to_list n))
                'bad-argument))))

(defun car2 (z)
  (funcall z 0))

(defun cdr2 (z)
  (funcall z 1))
```

This use of functions corresponds to nothing like our intuitive notion of what data should be. Nevertheless, all we need to do to show that this is a valid way to represent pairs is to verify that these functions satisfy the condition given above.[^4]

The subtle point to notice is that the value returned by ``(cons2 x y)`` is a function -- namely the anonymous dispatch function, which takes one argument and returns either ``x`` or ``y`` depending on whether the argument is 0 or 1. Correspondingly, ``(car2 z)`` is defined to apply ``z`` to ``0``. Hence, if ``z`` is the function formed by ``(cons2 x y)``, then ``z`` applied to 0 will yield ``x``. Thus, we have shown that ``(car2 (cons2 x y))`` yields ``x``, as desired. Similarly, ``(cdr2 (cons2 x y))`` applies the function returned by ``(cons2 x y)`` to 1, which returns ``y``. Therefore, this procedural implementation of pairs is a valid implementation, and if we access pairs using only ``cons2/2``, ``car2/1``, and ``cdr2/1`` we cannot distinguish this implementation from one that uses ``real'' data structures.

The point of exhibiting the procedural representation of pairs is not that our language works this way (Lisp systems in general, implement pairs directly, for efficiency reasons) but that it could work this way. The procedural representation, although obscure, is a perfectly adequate way to represent pairs, since it fulfills the only conditions that pairs need to fulfill. This example also demonstrates that the ability to manipulate functions as objects automatically provides the ability to represent compound data. This may seem a curiosity now, but procedural representations of data will play a central role in our programming repertoire. This style of programming is often called *message passing*; not only will we be using it as a basic tool in the next chapter when we address the issues of modeling and simulation, but Erlang -- and thus LFE -- are built upon concepts similar to these.

----

[^1]: Surprisingly, this idea is very difficult to formulate rigorously. There are two approaches to giving such a formulation. One, pioneered by C. A. R. Hoare (1972), is known as the method of abstract models. It formalizes the "functions plus conditions" specification as outlined in the rational-number example above. Note that the condition on the rational-number representation was stated in terms of facts about integers (equality and division). In general, abstract models define new kinds of data objects in terms of previously defined types of data objects. Assertions about data objects can therefore be checked by reducing them to assertions about previously defined data objects. Another approach, introduced by Zilles at MIT, by Goguen, Thatcher, Wagner, and Wright at IBM (see Thatcher, Wagner, and Wright 1978), and by Guttag at Toronto (see Guttag 1977), is called algebraic specification. It regards the "functions" as elements of an abstract algebraic system whose behavior is specified by axioms that correspond to our "conditions," and uses the techniques of abstract algebra to check assertions about data objects. Both methods are surveyed in the [paper by Liskov and Zilles](http://csg.csail.mit.edu/CSGArchives/memos/Memo-117.pdf) (1975).

[^2]: Note that LFE supports pattern mathcing in anonymous functions.

[^3]: In this case we have a very good justification for the use of pattern nmatching in the function arguments: value-based dispatch.

[^4]: We had to rename our functions from ``cons``, ``car``, and ``cdr``. to ``cons2``, etc. in order to avoid having LFE silently ignore them: LFE doesn't allow shadowing of certain forms (``cons``, etc. being among those).
