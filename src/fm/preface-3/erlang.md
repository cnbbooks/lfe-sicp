### A Recap of Erlang's Genesis

Though the LFE edition of *Structure and Interpretation of Computer Programs* is a reworking of the Scheme original to LFE and while both version focus entirely upon Lisp, we would be remiss if a brief history of Erlang -- upon which LFE firmly rests -- was not covered as well. One of the most concise and informative sources of Erlang history is the paper that Joe Armstrong wrote[^1] for the third History of Programming Languages[^2] conference.

What evolved into Erlang started out as the simple task of "solving Ericsson's software problem." [^3] Practically, this involved a series of initial experiments in programming simple telephony systems in a variety of languages. The results of this, namely as follows, fueled the next round of experiments:

* Small languages seemed better at succinctly addressing the problem space.
* The functional programming paradigm was appreciated, if sometimes viewed as awkward.
* Logic programming provided the most elegant solutions in the given problem space.
* Support for concurrency was viewed as essential.

Joe Armstrong's first attempts at Erlang were done in 1985 using the  Smalltalk programming language. He switched away from this after Roger Skagervall observed that the logic Joe had developed was really just thinly veiled Prolog. The development of a robust systems programming language for telephony was further refined with advice from Mike Williams, already a veteran in programming concurrent systems. He said that for a concurrent programming language to be efficient it had to keep in mind three key things that dominated its problem space: [^4]

* The time needed to create a process
* The time required to perform a context switch between two processes
* The amount of time taken to copy a message from one process to another

Furthermore, Bjarne Däcker defined the following capabilities for a language to be useful when programming telecommunications switches:

* To handle high-concurrency
* To handle soft real-time
* To support non-local, distributed computing
* To enable hardware interaction
* To support very large scale software systems
* To support complex interactions
* To provide non-stop operation (on the order of years)
* To allow for system updates without downtime
* To provide high-nines reliability
* To provide fault-tolerance for both hardware and software

With these guiding principles, Erlang in its Prolog form emerged over the course of 1986 and 1987. Robert Virding joined Armstrong in this effort during this time period, [^5] helping with such tasks as rewriting the initial prototype and improving overall concurrency performance.

----

[^1]: See Armstrong's paper [here](http://webcem01.cem.itesm.mx:8005/erlang/cd/downloads/hopl_erlang.pdf).

[^2]: [HOPL III](http://research.ihost.com/hopl/HOPL-III.html) was held in San Diego, early June of 2007. From the perspective of LFE, it is interesting to note that each language comprising its essence have been covered at HOPL conferences: **1.** HOPL I, 1978 John McCarthy presented "History of Lisp"; **2.** HOPL II, 1993 Guy L. Steele, Jr. and Richard P. Gabriel presented "The Evolution of Lisp," a continuation of McCarthy's 1978 talk; **3.** HOPL III, 2007 Joe Armstrong presented "A History of Erlang."

[^3]: Armstrong 2007, page 3.

[^4]: Ibid., page 2.

[^5]: "Robert Virding and Joe Armstrong - The History of the Erlang Virtual Machine", Erlang Factory 2010, London.
