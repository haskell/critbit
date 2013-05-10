Crit-bit trees for Haskell
====

This is the first purely functional implementation of crit-bit trees
that I'm aware of.

This package exists in part with education in mind:

* The core data structures are simple.

* The core algorithms are easy to grasp.

* I have intentionally structured the source to be easy to follow and
  extend.

* I've *deliberately* left the package incomplete. Ever thought to
  yourself, "I'd write a bit of Haskell if only I had a project to
  work on"?  Well, here's your chance!  I will set aside time to
  review your code and answer what questions I can.

Education aside, crit-bit trees offer some interesting features
compared to other key/value container types in Haskell.

* For many operations, they are much faster than `Data.Map` from the
  `containers` package. For instance, [`lookup` is about 3x
  faster](http://htmlpreview.github.io/?https://github.com/bos/critbit/blob/master/doc/criterion-sample-lookup.html).

* Compared to `Data.HashMap`, you get about the same lookup
  performance, but also some features that a hash-based structure
  can't provide: prefix-based search, efficient neighbour lookup,
  ordered storage.

Of course crit-bit trees have some downsides, too. For example,
building a tree from randomly ordered inputs is somewhat slow, and of
course the set of usable key types is small (only types that can be
interpreted as bitstrings "for free").

Compared to the most easily findable crit-bit tree code you'll come
across that's [written in C](https://github.com/glk/critbit), the core
of this library has a lot less accidental complexity, and so may be
easier to understand. It also handles arbitrary binary data that will
cause the C library to go wrong.



How to contribute
====

I've purposely published this package in an incomplete state, and I'd
like your help to round it out.  In return, you get to learn a little
Haskell, have your code reviewed by someone who wants to see you
succeed, and contribute to a rather nifty library.

Do you need any prior experience with Haskell to get started? No! All
you need is curiosity and the ability to learn from context. Oh, and a
github account.

My aim with this library is drop-in API compatibility with the widely
used Haskell [`containers`](https://github.com/haskell/containers)
library, which has two happy consequences:

* There are lots of functions to write!

* In almost every case, you'll find a pre-existing function in
  `containers` that (from a user's perspective) does exactly what its
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

Naturally, you'll follow the prevailing coding and formatting style.
If you forget, I'll be sad and offer you only a terse "fix your
formatting" review, and then you'll be sad too.


Setting expectations
====

I have no idea whether this experiment will attract zero contributors
or a hundred. If the former, that's too bad, and I'll flesh the
library out at my own pace. If the latter, I'll do my best to keep up,
and we'll be more systematic if necessary (it would be a shame to see
several redundant pull requests implementing the same functions, is
what I'm thinking).

But the main point of this is: have fun!
