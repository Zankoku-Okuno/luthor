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


Usage
-----

The API docs have sufficient examples for the [Combinator](https://hackage.haskell.org/package/luthor/TODO) and [Common Syntax](https://hackage.haskell.org/package/luthor/TODO) tools.

Here's an [extended example](lisp.md) of a Lisp parser. It touches on most of the library, except the lexing tools.

An example of the lexing tools should be on its way whenever I get around to it. If you ask for it in the [issues](https://github.com/Zankoku-Okuno/luthor/issues), I'll be right on it.


* [API Reference](https://hackage.haskell.org/package/luthor)
* [Report Problems](https://github.com/Zankoku-Okuno/luthor/issues)
* [Contribute](https://github.com/Zankoku-Okuno/luthor)