Luthor
======

Haskell library for lexing and utilizing lexemes.

Motivation
----------

Parser combinators are very nice, but as the complexity of the syntax grows, standard libraries begin to wear. In particular, the libraries slow down from backtracking. Or worse, correctness becomes difficult to manage, as in Parsec where its a constant battle to figure out where an extra `try` is needed.

The thing is, the complexity of a language (such as indentation or heredocs) is often in the lexemes, not the syntax. By isolating that complexity, we can achieve faster, more correct code. In all likelihood, error reporting will get better too, since the parser will naturally report `unexpected indent` instead of `unexpected ' ', expecting '//' or '/*'`.

What's more, some lexemes are so common that they must absolutely be offered as part of a reusable library. How many times must we re-implement a string literal parser? A hex integer parser? A float parser? With Luthor, never again. Further, some patterns really do occur often, like selecting the longest lexeme of many possible, ignoring whitespace, or using regexes (sometimes it really is the simplest way: `/[1-9][0-9]+/`).

Luthor is designed as an additional, transparent layer of tools built atop Parsec and Attoparsec. This way, Luthor will integrate well with existing projects looking for cleanup as well as new projects looking for familiar tools.