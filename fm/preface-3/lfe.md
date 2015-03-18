### The Inspiration for LFE

It wasn't until 2007 that, after 20 years of contributions to Erlang, Virding decided to start experimenting with a Lisp running on the Erlang VM. [^1] Initially explored as a Lisp 1, Virding switched to separating the name spaces for functions and variables more like the Lisp 2 of Common Lisp. [^2] LFE currently supports such features as:

* Non-hygenic macros
* Various forms borrowed from Scheme, Maclisp, and Common Lisp
* A REPL which allows for the definition of functions, records, and macros [^3]
* Immutable data
* Pattern matching
* Modules
* Functional programming paradigm
* Erlang data types
* Light-weight language processes
* Hot-loading of code on running systems
* 100% compatibility with Core Erlang and OTP

As to its own origins, on the LFE mail list Virding shared the following as the primary motivating factors:

* He was an "old Lisper" and was therefore quite interested in implementing a Lisp.
* He was curious as to what a Lisp on the Erlang VM would look like and how it would run. (It had always been a goal of his to make a Lisp which was specially designed for running on the Erlang VM and able to fully interact with Erlang/OTP.)
* He wanted to experiment with compiling a language from Core Erlang (adopted by the Erlang compiler in 2001) [^4]
* He was looking for some interesting programming projects that were not too large to do in his spare time.
* He likes implementing languages.
* He also thought it would be a fun problem to solve: it was an open-ended problem with lots of interesting parts.

Once again, the spirit of exploration conspired with good, clean fun to bring something new and interesting into the world while at the same time reflecting a rich and varied history. On the one hand, LFE has a Lisp heritage stretching back through the $$\lambda$$-Calculus to Peano. On the other, it is a systems programming language benefiting from Prolog vestiages such as pattern-matching list comprehensions. Hardened through industrial use, it rests upon a VM which powers 40% of the worlds telecommunications traffic. [^5] This is a potentially powerful combination offering unique capabilities the distributed systems metaprogrammer.

----

[^1]: Thread on the LFE mail list entitled [LFE History](https://groups.google.com/d/msg/lisp-flavoured-erlang/XA5HeLbQQDk/TUHabZCHXB0J)

[^2]: LFE also distingueshes between functions basd on arity, thus it is regularly jokingly referred to as a Lisp 2+.

[^3]: This is in stark contrast to the Erlang shell which does not allow the developer to define functions (except as anonymous ones assigned to a variable); Erlang also does not have Lisp-style macros.

[^4]: See Carlsson's 2001 paper "An introduction to Core Erlang", page 2.

[^5]: This is something often quoted in Erlang marketing materials; the number is based upon the market share Ericsson has in deployed systems world-wide.


