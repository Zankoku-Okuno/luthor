Luthor
======

Haskell library for lexing and utilizing lexemes.

If you're considering Parsec for your next big parser, Luthor will save you save hundreds of sloc, hours of debugging, and at least a few headaches. If you're considering a non-combinator approach, the savings cannot even be quantified.

For example, here's a Scheme parser:

```haskell
import Text.Luthor.Syntax
import Text.Luthor.Combinators

symbol = toLower <$> (it <||> oneOf "+_") `notFollowedBy` it
	where
	it = allowed `many1Not` charClass "0-9.+_"
	allowed = charClass "a-zA-Z0-9!$%&*/:<=>?~_^"
TODO
```

* [API Reference](https://hackage.haskell.org/package/luthor),
* [Documentation](http://zankoku-okuno.viewdocs.io/luthor/),
* [Issues](https://github.com/Zankoku-Okuno/luthor/issues)


Motivation
----------

Parser combinators are very nice, but as the complexity of the syntax grows, standard libraries begin to wear. In particular, the libraries slow down from backtracking. Worse, correctness becomes difficult to manage, as in Parsec where its a constant battle to figure out where an extra `try` is needed.

The thing is, the complexity of a language (such as indentation or heredocs) is often in the lexemes, not the syntax. By isolating that complexity, we can achieve faster, more correct code. In all likelihood, error reporting will get better too, since the parser will naturally report `unexpected indent` instead of `unexpected ' ', expecting '//' or '/*'`.

What's more, some lexemes are so common that they must absolutely be offered as part of a reusable library. How many times must we re-implement a string literal parser? A hex integer parser? A float parser? With Luthor, you can select from pre-built parsers as well as build your own customized parsers out of our components in moments. What's more, we also offer common patterns in parsing, like selecting the longest lexeme of many possible, ignoring whitespace, or using regexes (sometimes it really is the simplest way: `/[1-9][0-9]+/`).

Luthor is designed as an additional, transparent layer of tools built atop Parsec. This way, Luthor will integrate well with existing projects looking for cleanup as well as new projects looking for familiar tools.

Installation
------------

You can install Luthor using cabal, either from hackage:

```
cabal install luthor
```

or from our repository:

```
git clone https://github.com/Zankoku-Okuno/luthor.git
cd luthor
cabal install
```

Contribute
----------

I'm open to more tools, particularly new composable combinators and new common token parsers.

Our code is hosted on [Github](https://github.com/Zankoku-Okuno/luthor).

The fastest way to make contact is through our [issue tracker](https://github.com/Zankoku-Okuno/luthor/issues) or by [submitting a pull request](https://github.com/Zankoku-Okuno/luthor/pulls).

Support
-------

Our API reference is hosted on [Hackage](https://hackage.haskell.org/package/luthor) and additional documentation is available on [Viewdocs](http://zankoku-okuno.viewdocs.io/luthor/).

If you have questions about the library or find any flaws, please talk to us on our [issue tracker](https://github.com/Zankoku-Okuno/luthor/issues).

License
-------

The project is licensed under the 3-clause BSD license.