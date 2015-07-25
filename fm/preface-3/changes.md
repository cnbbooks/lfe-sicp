## Notes on Changes from the Original

The original text of *Structure and Interpretation of Computer Programs* was
published in the 80s, but has material in it preceding even that time, dating back to courses taught at MIT in the 60s. Though the *essence* of what was taught in those lecture halls -- and with the original SICP text -- remains just as relevant today, much context has changed in the field of computing since then. As such, some changes in a new edition are to be expected.

Furthermore, this edition of SICP is an adaptation for a different programming
language -- through still a Lisp -- whose syntax differs in varying degrees. More than syntax, though, LFE is built upon a very different VM, one whose focus is on such concerns as fault-tolerance, massive scale, and reliability.

As such, we have taken liberties in our edition of SICP, hopefully with little
to no impact on its essence. These liberties include the following:

* The Lisp-2 syntax of LFE is used instead of the Lisp-1 syntax of a Scheme.
* As an extension of that first point, features such as pattern matching in function heads, guards, multi-arity functions, etc., are used extensively, often leading to more concise functions that their original, Scheme counterparts.
* When pertinent, features from LFE and Erlang are introduced in conjunction
  with the subject matter of the original text.
* Very rarely some of the original text or footnotes might be omitted when not at all applicable
  to LFE.
* LFE doesn't support nested ``defun``s like Scheme supports nested ``define``s; ``flet`` and ``flectrec`` are used instead. However, as a result, these are introduced before ``let``. This is awkward, but not disastrous.
* The LFE Edition uses LaTeX, and as such, equations are much easier to read.
* Updating examples to modern, accepted forms (e.g., the Ackermann function).
* Exercises are broken out into their own sections (while preserving their original order and placement in the chapters).

Some of the terminology in the text has also been changed to maintain
consistency with that used by the Erlang and LFE community. Most prominently,
readers of the original text will notice that we do not use the term
"procedure" but have instead opted for the more common term "function."


