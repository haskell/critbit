Crit-bit trees for Haskell
====


How to contribute
====

I purposely published this package in an incomplete state, and I'd
like your help to round it out.  In return, you get to learn a little
Haskell, get your code reviewed by someone who wants to see you
succeed, and contribute to a rather nifty library.

Do you need any prior experience with Haskell to get started? No! All
you need is curiosity and the ability to pick things up from
context. Oh, and a github account.

My aim with this library is drop-in API compatibility with the widely
used Haskell [`containers`](https://github.com/haskell/containers)
library, which means that in almost every case there are pre-existing
functions that (from a user's perspective) do exactly what their
counterparts in *this* library ought to do.


Getting started
----

If you want to contribute or play around, please use the most modern
version of the [Haskell Platform](http://www.haskell.org/platform/).

Once you have the Platform installed, there are just a few more steps.

Set up your local database of known open source Haskell packages.

    cabal update

Install the latest version of the `cabal` command, without which you
won't be able to build or run benchmarks. You'll also want a sandbox
environment. I like `cabal-dev`, and there are plenty of others.

    cabal install cabal-install
    cabal install cabal-dev

Both the new `cabal` command and `cabal-dev` will install to
`$HOME/.cabal/bin`, so put that directory at the front of your shell's
search path before you continue.

Get the `critbit` source.

    git clone git://github.com/bos/critbit

Set up a sandbox.

The first time through, you need to download and install a ton of
dependencies, so hang in there.

    cd critbit
    cabal-dev install \
        --enable-tests \
        --enable-benchmarks \
        --only-dependencies \
        -j

The `cabal-dev` command is just a sandboxing wrapper around the
`cabal` command.  The `-j` flag above tells `cabal` to use all of your
CPUs, so even the initial build shouldn't take more than a few
minutes.

    cabal-dev configure \
        --enable-tests \
        --enable-benchmarks
    cabal-dev build


Running the test suite
----

Once you've built the code, you can run the entire test suite in a few
seconds.

    dist/build/tests/tests +RTS -N

(The `+RTS -N` above tells GHC's runtime system to use all available
cores.)

If you want to explore, the `tests` program accepts a `--help`
option. Try it out.


Running benchmarks
----

It is just as easy to benchmark stuff as to test it.

First, you need a dictionary. If your system doesn't have a file named
`/usr/share/dict/words`, you can [download a dictionary
here](http://www.cs.duke.edu/~ola/ap/linuxwords).

If you've downloaded a dictionary, tell the benchmark
suite where to find it by setting the `WORDS` environment variable.

    export WORDS=/my/path/to/linuxwords

You can then run benchmarks and generate a report. For instance, this
runs every benchmark that begins with `bytestring/lookup`.

    dist/build/benchmarks/benchmarks -o lookup.html \
        bytestring/lookup

Open the `lookup.html` file in your browser. [Here's an
example](http://htmlpreview.github.io/?https://github.com/bos/critbit/blob/master/doc/criterion-sample-lookup.html)
of what to expect.

As with `tests`, run the `benchmarks` program with `--help` if you
want to do some exploring.



What your code should look like
----

Okay, so you've bought into this idea, and would like to try writing a
patch. How to begin?

I've generally tried to write commits with a view to being readable,
so there are examples you can follow.

For instance, [here's the commit where I added the `keys`
function](https://github.com/bos/critbit/commit/48438b48ca9bc5d96c1987afe7acdf4dada823f3). This
commit follows a simple pattern:

* [Uncomment the export](https://github.com/bos/critbit/commit/48438b48ca9bc5d96c1987afe7acdf4dada823f3#L0L91) of the function.

* [Write the function
  definition](https://github.com/bos/critbit/commit/48438b48ca9bc5d96c1987afe7acdf4dada823f3#L0R503).
  In this case, the documentation is taken almost verbatim from the
  corresponding function in [the `Data.Map`
  module](https://github.com/haskell/containers/blob/342a95002822cca56f2d5b086cdd5a98592d5c10/Data/Map/Base.hs#L1889).

* [Write a
  test](https://github.com/bos/critbit/commit/48438b48ca9bc5d96c1987afe7acdf4dada823f3#L2R108)
  and [make sure it gets
  run](https://github.com/bos/critbit/commit/48438b48ca9bc5d96c1987afe7acdf4dada823f3#L2R124).

* [Add an entry to the benchmark
  suite](https://github.com/bos/critbit/commit/48438b48ca9bc5d96c1987afe7acdf4dada823f3#L1R179)
  so it's easy to see how this compares to other key/value map types.
