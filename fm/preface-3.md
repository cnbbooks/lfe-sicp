# Preface to the LFE Edition

<blockquote>
Unbound creativity is the power and the weakness of the Force. The Art of Programming Well lies in forging a balance between endless possibilities and strict discipline.
</blockquote>

*--Cristina Videira Lopes, "Jedi Masters", on the history of Lisp and programming*

TBD


## Notes on Changes from the Original

The original text of Structure and Interpretation of Computer Programs was
published in the 80s, but has material in it preceding even that time. Though
the essenve of what was taught in that course and with this text remains just
as relevant today, much context has changed in the field of computing since
then.

Furthermore, this edition of SICP is an adaptation for a different programming
language -- through still a Lisp -- whose syntax differs in varying degrees,
but perhaps most significant, is built upon a very different programming
language: Erlang.

As such, we have taken liberties in our edition of SICP, hopefully with little
to no impact on its essence. These liberties include the following:

* The Lisp-2 syntax of LFE is used instead of the Lisp-1 syntax of a Scheme.
* When pertienent, features from LFE and Erlang are introduced in conjunction
  with the subject matter of the original text.
* Very rarely some of the original text is omitted when not at all applicable
  to LFE.
* Updating examples to modern, accepted forms (e.g., the Ackermann function).
* Exercises are broken out into their own sections (while preserving their original order and placement in the chapters).

Some of the terminology in the text has also been changed to maintain
consistency with that used by the Erlang and LFE community. Most prominently,
readers of the original text will notice that we do not use the term
"procedure" but have instead opted for the more common term "function".
