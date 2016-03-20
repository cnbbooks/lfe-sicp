# Structure and Interpretation of Computer Programs

*The LFE Edition*


## About

This Gitbook (available [here](http://lfe.gitbooks.io/sicp/))
is a work in progress, converting the MIT classic
[Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/sicp/)
to Lisp Flavored Erlang. We are forever indebted to Harold Abelson, Gerald
Jay Sussman, and Julie Sussman for their labor of love and intelligence.
Needless to say, our gratitude also extends to the MIT press for their
generosity in licensing this work as Creative Commons.


## Contributing

This is a huge project, and we can use your help! Got an idea? Found a bug?
[Let us know!](https://github.com/lfe/sicp/issues/new).


### Building the Book

Install the dependencies:

```bash
$ make deps
```

On Linux, you'll need to run that with ``sudo``.

Install the gitbook modules:

```bash
$ make setup
```

Build the book:

```bash
$ make book
```
