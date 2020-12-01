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

## Building the Book

To build a local copy of the book, you'll need to have the `mdbook` binary
installed (a small executable written in Rust). If you don't have it installed
and attempt to build, you'll be presented with an error message that includes
a download link.

To rebuild the content:

```bash
$ make
```

To run a local instance of the book on port `3000` (which will reload if you
make any changes to the Markdown source):

``` bash
$ make run
```

## Contributing

This is a huge project, and we can use your help! Got an idea? Found a bug?
[Let us know!](https://github.com/lfe/sicp/issues/new).

Once you've got a ticket, make the desired changes to the Markdown source
(don't commit changes generated in the `book` directory!), push to your fork +
branch, and then open a PR :-)
