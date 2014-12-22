-- Extended Example
-- ================
--
-- This document shows off the features of the
-- [luthor](https://hackage.haskell.org/package/luthor) package. Since this
-- is an overview of every feature, we've decided to focus on a simple
-- example and not go too far in-depth on the possibilities luthor provides.
-- For that, see our other documentation, which goes into each toolset
-- independently. Also, since our example is actually a fairly small parser,
-- the benefits of using `Lex` aren't clear, but it does pay off once your
-- grammar gets perhaps two or three times this size.
--
-- It is highly recommended to read this file with the
-- [API Reference](https://hackage.haskell.org/package/luthor) on hand.
-- We use several functions from those packages but do not re-document
-- them here, so a serious understanding will require a little homework.
--
-- You might be reading this in html/markdown or in a Haskell file. The html
-- is generated directly from the Haskell, so you can be assured that the
-- code works as advertized, as well as run it yourself.
--
-- First off, let's import some usual stuff...

import System.IO
import System.Environment
import System.Exit
import Data.List

-- and also import the relevant luthor modules.

import Text.Luthor
import Text.Luthor.Syntax
import Text.Luthor.Indent
import Text.Luthor.Lex as Lex

import Data.Functor.Identity (Identity)

-- Let's go ahead and define our abstract syntax right up front to give
-- context to our parsers.

data Lisp = Atom Atom | List [Lisp]
data Atom = ASymbol String
          | ANumber Rational
          | AString String

-- Since we're using a scannerful (lexing) parser, we'll need to connect the
-- two processing segments with a data type for tokens.

data Token = Space
           | AtomTok Atom
           | OpenParen | CloseParen
           | Indent | Nextline | Dedent
    deriving (Show)

-- A couple high-level shortcuts so we don't clutter the rest of the code.

type ParserST = ((),IndentState String () Identity)
type Lexer a = ParsecI String () a
type Lexed = Lex String ParserST Token
type Parser a = Luthor Token ParserST a

parseLisp :: SourceName -> String -> Either ParseError [Lisp]
parseLisp = runLuthor lexer parser ((),startIndent (DontMix " ") wss)

-- And now we can hop right into the lexer.
-- We'll start with simple atoms...

lispSymbol :: Lexed
lispSymbol = lexeme $ AtomTok . ASymbol <$>
    let ident = charClass "a-zA-Z_0-9-" `many1Not` charClass "0-9-"
    in ident `notFollowedBy` canStartAtom

lispInteger :: Lexed
lispInteger = lexeme $ AtomTok . ANumber . fromIntegral <$>
    integer `notFollowedBy` (dot <|> void canStartAtom)

lispDecimal :: Lexed
lispDecimal = lexeme $ AtomTok . ANumber <$>
    scientific `notFollowedBy` canStartAtom

lispString :: Lexed
lispString = lexeme $ AtomTok . AString <$>
    dqString cEscapes `notFollowedBy` canStartAtom

canStartAtom :: Lexer Char
canStartAtom = aChar $ charClass "a-zA-Z_0-9\"+-"

-- and then handle punctuation.

lispPunct :: Lexed
lispPunct = lexeme $ dispatch
    [ (void $ char '(', pure OpenParen)
    , (void $ char ')', pure CloseParen)
    , (indent, pure Indent)
    , (nextline, pure Nextline)
    , (dedent, pure Dedent)
    ]

-- We'll also need to deal with whitespace.
-- It will especially come in handy when configuring the indentation part
-- of the parser to handle blank lines appropriately.
--
-- For our purposes, whitespace includes spaces and tabs (`lws`),
-- line comments (starting with `;`) and line folds (backslash-newline).

wss :: [Lexer ()]
wss = [ void lws, void $ lineComment ";", bsnl ]

ws :: Lexed
ws = lexeme $ Space <$ many1_ (choice wss)

-- Finally, we tie it all together into a token recognizer.

lispAtom :: Lexed
lispAtom = choice [ lispSymbol, lispInteger, lispDecimal, lispString ]

lexer :: Lexed
lexer = choice [ lispAtom, lispPunct, ws ]

-- Now, we can move onto parsing. Normal s-exprs are just an atom or
-- a parenthesized list of s-exprs. Adding indentation-sensitivity,
-- we also allow indented s-exprs separated by newlines.
-- Ah yes, and there's the special nil s-expr, spelled `()`.
--
-- There is one oddity: we want close parens and dedents to be
-- interchangeable. Therefore, a list expression can end with a dedent or
-- with a close paren, regardless of how it began.
--
-- Out first step is to provide a clean way to extract particular payloads
-- from our lexeme stream, the same way the Parsec implementation has to
-- define a way to get at the `Char`s when parsing `String`s.

atom :: Parser Lisp
atom = unlexWith $ \t -> case t of
    AtomTok x -> Just $ Atom x
    _ -> Nothing

openParen :: Parser ()
openParen = unlexWith $ \t -> case t of { OpenParen -> Just (); _ -> Nothing }

openIndent :: Parser ()
openIndent = unlexWith $ \t -> case t of { Indent -> Just (); _ -> Nothing }

close :: Parser ()
close = (endOfLexemes <|>) $ unlexWith $ \t -> case t of
    CloseParen -> Just ()
    Dedent -> Just ()
    _ -> Nothing

next :: Parser ()
next = unlexWith $ \t -> case t of { Nextline -> Just (); _ -> Nothing }

nil :: Parser Lisp
nil = List [] <$ do
    openParen
    unlexWith $ \t -> case t of { CloseParen -> Just (); _ -> Nothing }

-- Now, we get down to the business of grammar:

bareExpr :: Parser [Lisp]
bareExpr = many1 expr

expr :: Parser Lisp
expr = atom <||> nil <||> parenExpr <||> indentExpr

parenExpr :: Parser Lisp
parenExpr = between openParen close $ List <$> bareExpr

indentExpr :: Parser Lisp
indentExpr = between openIndent close $ do
    inner <- bareExpr `sepBy1` next
    return $ case inner of
        [e] -> List e
        es -> List (List <$> es)

-- Finally, we filter out extraneous whitespace and parse a file full of
-- s-exprs.

isExtraSpace :: Token -> Bool
isExtraSpace t = case t of { Space -> True; _ -> False }

parser :: Parser [Lisp]
parser = between (ignore isExtraSpace) endOfLexemes $
    (wrap <$> bareExpr) `sepEndBy` next
    where
    wrap [e] = e
    wrap es = List es

-- ...and we're done with the parser. We've already built our `parseLisp`
-- shortcut, so we can move on to setting up a program to actually use our
-- new parser, but before we build our main, I'll set up some `Show`
-- instances...

instance Show Atom where
    show (ASymbol name) = name
    show (ANumber n) = show n
    show (AString str) = show str
instance Show Lisp where
    show (Atom a) = show a
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"

-- ...because the main really is just a transpiler from this
-- indentation-sensitive Lisp to a fully-parenthesized Lisp.

main :: IO ()
main = do
    inFile <- getArgs >>= \args -> case args of
        [inFile] -> return inFile
        _ -> hPutStrLn stderr ("usage: lisp.hs filename") *> exitFailure
    results <- parseLisp inFile <$> readFile inFile
    case results of
        Right exprs -> mapM_ print exprs
        Left err -> print err *> exitFailure

-- And there you have it. Try this out on some of the example files included
-- in the package (`docs/*.l`), or experiment with your own.
--
-- Going further, it would be a simple matter to introduce the rest of the
-- familiar Lisp syntax:
--
-- * Add quotation and quasiquotation by adding appropriate token sorts,
--     tokenizers and parser.
-- * Add dotted-expressions with a token sort, tokenizer and a
--     chainr-based parser.
-- * Comment out s-exprs the same way you might quote an s-expr.
--
-- And of course, it's not hard to build a Lisp interpreter. You could always
-- build a driver that interprets instead of transpiles.
