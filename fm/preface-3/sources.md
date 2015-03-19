## Obtaining the Book and Related Code

The source code for the LFE edition of this book uses the same license as the original: [Creative Commons Attribution-Noncommercial 3.0 Unported License](http://creativecommons.org/licenses/by-nc/3.0/).

### Book Source

There are two related sources available for this book:

* The original text for the second edition is available on the [MIT Press site](http://mitpress.mit.edu/sicp/).
* The source code for the LFE edition (mostly Markdown) is available on [Github](https://github.com/lfe/sicp).

You may obtain a copy of the source code via ``git``:

```bash
$ git clone https://github.com/lfe/sicp.git
```

or direct download: [https://github.com/lfe/sicp/archive/master.zip](https://github.com/lfe/sicp/archive/master.zip).

### Code Used in the Book

Each chapter of this book defines functions and uses code from the previous chapters. The interactive LFE REPL sessions are saved as files in the book repository. They are available in each chapter directory as ``code.lfe``.

### Running the Code in the Book

To follow along in each chapter or to run the code that has been provided, you will need the following:

* A recent version of Erlang (the code in this book was tested with Erlang 17.4)
* LFE

The download and installation of Erlang is not covered in this book; information for that is available from many other sources with the kind help of your favourite Internet search engine.

#### Getting and Compiling LFE

You may obtain LFE via ``git``, creating a subdirectory in the ``scip`` directory:

```bash
cd sicp
git clone https://github.com/rvirding/lfe.git
```

or direct download: [https://github.com/rvirding/lfe/archive/develop.zip](https://github.com/rvirding/lfe/archive/develop.zip). If you use the direct download method, move and rename the unzipped directory to match the ``git clone`` method:

```bash
cd sicp
unzip ~/Downloads/lfe-develop.zip
mv lfe-develop ./lfe
```

Now you can compile LFE, start the REPL, and load some code (you can safely ignore compiler warnings):

```bash
cd lfe
make
cd ../
./lfe/bin/lfe
```

#### Loading the Chapter Code

```lisp
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] ...

LFE Shell V6.3 (abort with ^G)
> (run "ch1/code.lfe")
Loading Chapter 1 code ...
Chapter 1 loaded.
ok
```
Quick check to make sure that everything loaded as expected:

```lisp
> (square 2)
4
```