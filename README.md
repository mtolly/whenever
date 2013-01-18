whenever
========

Whenever is an esoteric programming language, created by David Morgan-Mar, which
has no sense of urgency. It does things whenever it feels like it, not in any
sequence specified by the programmer.

Interpreter in Haskell by [Michael Tolly](mailto:miketolly@gmail.com).
For language information, see [David's website](http://dangermouse.net/esoteric/whenever.html).

Installation
------------

1.  Install the [Haskell Platform](http://www.haskell.org/platform/). Or, install ghc, cabal, alex, and happy.

2.  To build and install:

        cd whenever
        cabal install
   
    By default this will place the executable in $HOME/.cabal/bin on Linux,
    or %appdata%\cabal\bin on Windows, so optionally add that to your $PATH.

3.  To run the interpreter:

        whenever program.whenever
