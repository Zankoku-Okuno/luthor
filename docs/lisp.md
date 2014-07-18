Extended Example
================

This document shows off the features of the
[luthor](https://hackage.haskell.org/package/luthor) package. Since this
is an overview of many features, we've decided to focus on a simple
example and not go too far in-depth on the possibilities luthor provides.
For that, see our other documentation, which goes into each toolset
independently.

It is highly recommended to read this file with the
[API Reference](https://hackage.haskell.org/package/luthor) on hand.
We use several functions from those packages but do not re-document
them here, so a serious understanding will require a little homework.

You might be reading this in html/markdown or in a Haskell file. The html
is generated directly from the Haskell, so you can be assured that the
code works as advertised, as well as run it yourself.

First off, let's import some usual stuff...

```haskell
import System.IO
import System.Environment
import System.Exit
import Data.List
import Data.Ratio
```

and also import the relevant luthor modules.

```haskell
import Text.Luthor
import Text.Luthor.Syntax
import Text.Luthor.Indent
```

Let's go ahead and define our abstract syntax right up front to give
context to our parsers.

```haskell
data Lisp = Atom Atom | List [Lisp]
data Atom = ASymbol String
          | ANumber Rational
          | AString String
```

A couple high-level shortcuts so we don't clutter the rest of the code.

```haskell
type Parser a = ParsecI String () a

parseLisp :: SourceName -> String -> Either ParseError [Lisp]
parseLisp = runParserI parseFile (DontMix " ") wss ()
```

Let's start with linear whitespace. This is whitespace that doesn't
affect indentation, but can separate tokens from each other.
It will especially come in handy when configuring the indentation part
of the parser to handle blank lines appropriately.

For our purposes, whitespace includes spaces and tabs (`lws`), 
line comments (starting with `;`) and line folds (backslash-newline).

```haskell
wss :: [Parser ()]
wss = [ void lws, void $ lineComment ";", bsnl ]

ws :: Parser ()
ws = many1_ $ choice wss

ws0 :: Parser ()
ws0 = optional_ ws
```

And now we can hop right into the heart of the parser.
We'll start with simple atoms...

```haskell
canStartAtom :: Parser ()
canStartAtom = void . aChar $ charClass "a-zA-Z_0-9\"+-"

lispSymbol :: Parser Atom
lispSymbol = ASymbol <$>
    let ident = charClass "a-zA-Z_0-9-" `many1Not` charClass "0-9-"
    in ident `notFollowedBy` canStartAtom

lispInteger :: Parser Atom
lispInteger = ANumber . fromIntegral <$>
    integer `notFollowedBy` (dot <|> canStartAtom)

lispDecimal :: Parser Atom
lispDecimal = ANumber <$>
    scientific `notFollowedBy` canStartAtom

lispString :: Parser Atom
lispString = AString <$>
    dqString cEscapes `notFollowedBy` canStartAtom
```

...and then expressions. Normal s-exprs are just an atom or
a parenthesized list of s-exprs. Adding indentation-sensitivity,
we also allow indented s-exprs separated by newlines.
Ah yes, and there's the special nil s-expr, spelled `()`.

There is one oddity: we want close parens and dedents to be
interchangeable. Therefore, a list expression can end with a dedent or
with a close paren, regardless of how it began.

```haskell
openParen :: Parser ()
openParen = char '(' *> ws0

closeParen :: Parser ()
closeParen = ws0 <* char ')'

close :: Parser ()
close = closeParen <||> dedent

lispNil :: Parser Lisp
lispNil = List [] <$ openParen <* closeParen


bareExpr :: Parser [Lisp]
bareExpr = expr `sepBy1` ws0

parenExpr :: Parser Lisp
parenExpr = between openParen close $ List <$> bareExpr 

indentExpr :: Parser Lisp
indentExpr = between indent close $
    List . (List <$>) <$> bareExpr `sepBy1` nextline
```

Finally, we tie it all together.

```haskell
atom :: Parser Lisp
atom = lispNil <||> Atom <$>
    choice [ lispSymbol
           , lispInteger
           , lispDecimal
           , lispString
           ]

expr :: Parser Lisp
expr = atom <||> parenExpr <||> indentExpr

parseFile :: Parser [Lisp]
parseFile = between (pure ()) endOfInput $
        (List <$> bareExpr) `sepAroundBy` nextline
```

...and we're done. We've already built our `parseLisp`
shortcut, so we can move on to setting up a program to actually use our
new parser, but before we build our main, I'll set up some `Show`
instances...

```haskell
instance Show Atom where
    show (ASymbol name) = name
    show (ANumber n) | denom == 1 = show numer
                     | otherwise = show numer ++ "/" ++ show denom
        where
        numer = numerator n
        denom = denominator n
    show (AString str) = show str
instance Show Lisp where
    show (Atom a) = show a
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
```

...because the main really is just a transpiler from this
indentation-sensitive Lisp to a fully-parenthesized Lisp.

```haskell
main :: IO ()
main = do
    inFile <- getArgs >>= \args -> case args of
        [inFile] -> return inFile
        _ -> hPutStrLn stderr ("usage: lisp.hs filename") *> exitFailure
    results <- parseLisp inFile <$> readFile inFile
    case results of
        Right exprs -> mapM_ print exprs
        Left err -> print err *> exitFailure
```

And there you have it. Try this out on some of the example files included
in the package (`docs/*.l`), or experiment with your own.

Going further, it would be a simple matter to introduce the rest of the
familiar Lisp syntax:

* Add quotation and quasiquotation by adding appropriate token sorts,
    tokenizers and parser.
* Add dotted-expressions with a token sort, tokenizer and a
    chainr-based parser.
* Comment out s-exprs the same way you might quote an s-expr.

And of course, it's not hard to build a Lisp interpreter. You could always
build a driver that interprets instead of transpiles.
