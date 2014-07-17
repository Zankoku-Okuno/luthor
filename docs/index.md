Luthor
======

Luthor is a Haskell library for lexing and utilizing lexemes.

If you're considering Parsec for your next big parser, Luthor will save you save hundreds of sloc, hours of debugging, and at least a few headaches. If you're considering a non-combinator approach, the savings cannot even be quantified.


Install
-------

You can install Luthor using cabal, either from Hackage:

```
cabal install luthor
```

or from our repository:

```
git clone https://github.com/Zankoku-Okuno/luthor.git
cd luthor
cabal install
```


Examples
--------

Luthor is structured as several independent tools which may be used individually or in any combination.

* [Composable Combinators](combinator.md)
* [Common Syntaxes](syntax.md)
* [Indentation](indent.md)
* [Building Lexers](lex.md)

There's also an [extended example](lisp.hs) of a Lisp parser.

More
----

* [API Reference](https://hackage.haskell.org/package/luthor)
* [Report Problems](https://github.com/Zankoku-Okuno/luthor/issues)
* [Contribute](https://github.com/Zankoku-Okuno/luthor)