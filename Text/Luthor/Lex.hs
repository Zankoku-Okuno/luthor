{-| The usual way to use Parsec, at least as far as the tutorials are concerned, is
    to do scannerless parsing. That is, the parser operates directly on input characters
    without first parsing out tokens. Scanner parsing certainly has its advantages,
    particularly in casual parsing, but it comes with its disadvantages too, especially
    when the complexity of the grammar increases.

    Oftentimes, complex grammars can isolate that complexity into contiguous portions
    of input which are called lexemes (also called tokens). In these cases, it is
    beneficial on many fronts to introduce a lexer (also called tokenizer or scanner)
    before parsing. The lexer and parser stand in a producer-consumer relationship.

    This module implements the lexer-parser pattern on top of Parsec. Thus, you can take
    advantage of a familiar and battle-tested library to implement both your lexer and
    parser.

    The benefits of isolating complexity into the parser are:

    * More reliability: because Parsec does not backtrack by default, it is easy to
        accidentally forget a 'try' combinator and introduce bugs that are difficult to
        detect and diagnose. Isolating complexity into the lexemes drastically reduces the
        scope of debugging: from the entire grammar down to only the part of the lexer
        responsible for a single kind of token.
    
    * Increased performance: backtracking can be isolated to the lexer. This means that
        backtracking has limited scope, and the parser can take advantage of no-lookahead
        parsing algorithms.
    
    * Enhanced error reports: many places where a scannerless parser might report an
        unexpected character, a lexer-parser will naturally report an entire token.
        The added context is much easier to use for debugging.

    The most important definitions for understanding this module are
    'Lexeme', 'runLuthorT', 'lexeme', and 'satisfy'. The general structure of a
    Luthor-based parser is: 1) create a data type for 'Lexeme' payloads, 2) write your
    lexers and wrap them all in 'lexeme', 3) create parsers for each case of payload
    using 'unlex' and satisfy', 4) write your parsers and connect them with your lexers
    using 'runLuthorT'.
-}
{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module Text.Luthor.Lex (
    -- * Basic Concepts
      LexT
    , LuthorT
    , Lexeme
    , runLuthorT
    -- ** Shortcuts
    , Lex, Luthor
    , runLuthor
    -- * Produce Lexemes
    , lexeme
    , ignore
    -- * Consume Lexemes
    , unlex
    , unlexWith
    , satisfy
    , endOfLexemes
    , isAtEnd
    ) where

import Data.Maybe

import Control.Monad.Identity

import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (many)
import Text.Parsec.Combinator (eof)
import Text.Luthor.Combinator


{-| A lexer: producer of 'Lexeme's -}
type LexT s u m t = ParsecT s u m (Lexeme t)
{-| A parser: consumer of 'Lexeme's -}
type LuthorT t = ParsecT [Lexeme t]

{-| The lexer and parser communicate using the 'Lexeme' type.

    Lexemes carry position information as well as a payload.
    Generally, you will want to define a data type for the
    payload and use 'satisfy' to extract particular cases of
    payloads for parsing.
-}
data Lexeme a = Lexeme SourcePos a
              | EndOfLexemes SourcePos
    deriving (Eq, Functor, Show)

-- |Synonym for lexers over the 'Identity' monad.
type Lex s u a = LexT s u Identity a
-- |Synonym for parsers over the 'Identity' monad.
type Luthor s u = LuthorT s u Identity


{-| Connect and run a lexer and parser together. -}
runLuthorT :: (Monad m, Stream s m x, Stream [Lexeme t] m y, Show x)
           => LexT s u m t -- ^ lexer: transform raw input stream into many tokens
           -> LuthorT t u m a -- ^ parser: transform token stream into parsed data
           -> u
           -> SourceName
           -> s
           -> m (Either ParseError a)
runLuthorT lexer parser s source input = do
    e_lexResult <- runPT (_wrap lexer) s source input
    case e_lexResult of
        Left err -> return $ Left err
        Right (input', s') -> runPT parser s' source input'

{-| As 'runLuthorT' in the 'Identity' monad. -}
runLuthor :: (Stream s Identity x, Stream [Lexeme t] Identity y, Show x)
          => Lex s u t -- ^ lexer: transform raw input stream into many tokens
          -> Luthor t u a -- ^ parser: transform token stream into parsed data
          -> u
          -> SourceName
          -> s
          -> Either ParseError a
runLuthor lexer parser s source input =
    case runP (_wrap lexer) s source input of
        Left err -> Left err
        Right (input', s') -> runP parser s' source input'


{-| Wrap a normal parser into a parser for a 'Lexeme'.
    The passed parser determines the payload. Use this function
    at the boundary of the lexer and parser: when a complete lexeme
    has been recognized, but parsing has not been performed.
    
    On failure, the resulting parser does not consume input, even if
    the passed parser would.
-}
lexeme :: (Monad m) => ParsecT s u m a -> ParsecT s u m (Lexeme a)
lexeme parser = Lexeme <$> getPosition <*> try parser

{-| Drop lexemes from the input stream whose payloads that satisfy the passed
    predicate. This is useful, for example, in whitespace-insensitive languages
    to remove extraneous whitespace so it need not clutter the parser definition.

    WARNING: use only as the first action in a parsing monad.
-}
ignore :: (Monad m) => (t -> Bool) -> LuthorT t u m ()
ignore p = setInput =<< filter p' <$> getInput
    where
    p' (Lexeme _ x) = not $ p x
    p' (EndOfLexemes _) = True



{-| Obtain a lexeme from the stream only if it satisfies the passed predicate.

    This is a very important function for building parsers. Generally,
    after defining your data type for 'Lexeme' payloads, you will implement
    several combinators for recognizing each case of your data type.
-}
satisfy :: (Show a, Stream [Lexeme a] m (Lexeme a))
        => (a -> Bool)
        -> LuthorT a u m a
satisfy p = tokenPrim _lexShow _lexUpdatePos _lexCheck
    where
    _lexCheck (Lexeme _ x) | p x = Just x
    _lexCheck _ = Nothing

{- |Unpacks a payload from the 'Lexeme' stream and attempts to transform it.
    If the transformation fails (evaluates to 'Nothing'), then this parser fails.
-}
unlexWith :: (Show a, Stream [Lexeme a] m (Lexeme a)) => (a -> Maybe b) -> LuthorT a u m b
unlexWith f = fromJust . f <$> tokenPrim _lexShow _lexUpdatePos _lexCheck
    where
    _lexCheck (Lexeme _ x) = case f x of
        Just _ -> Just x
        Nothing -> Nothing
    _lexCheck _ = Nothing

{-| Unpacks a payload from the 'Lexeme' stream. Only fails at the end of the lexeme stream. -}
unlex :: (Show a, Stream [Lexeme a] m (Lexeme a))
          => LuthorT a u m a
unlex = tokenPrim _lexShow _lexUpdatePos _lexCheck
    where
    _lexCheck (Lexeme _ x) = Just x
    _lexCheck _ = Nothing

{-| Succeed only at the end of the lexeme stream. -}
endOfLexemes :: (Show a, Stream [Lexeme a] m (Lexeme a)) => LuthorT a u m ()
endOfLexemes = expect "end of input" $ do
    t <- getInput
    case t of
        (Lexeme _ x:_) -> unexpected $ show x
        (EndOfLexemes _:_) -> return ()
        [] -> return ()

{-| Detect whether the parser is at the end of the lexeme stream
    without consuming input.
-}
isAtEnd :: (Monad m) => LuthorT a u m Bool
isAtEnd = do
    t <- getInput
    return $ case t of
        (Lexeme _ _:_) -> False
        (EndOfLexemes _:_) -> True
        [] -> True


_lexShow :: (Show a) => Lexeme a -> String
_lexShow = maybe "end of stream" show . _lexPayload

_lexUpdatePos :: SourcePos -> Lexeme a -> [Lexeme a] -> SourcePos
_lexUpdatePos _ _ [] = error "malformed Lexeme stream"
_lexUpdatePos _ _ (x:_) = _lexPos x

_lexPayload :: Lexeme a -> Maybe a
_lexPayload (Lexeme _ x) = Just x
_lexPayload (EndOfLexemes _) = Nothing

_lexPos :: Lexeme a -> SourcePos
_lexPos (Lexeme pos _) = pos
_lexPos (EndOfLexemes pos) = pos

_wrap :: (Monad m, Stream s m t, Show t) => LexT s u m a -> ParsecT s u m ([Lexeme a], u)
_wrap lexer = do
    result <- many lexer
    end <- EndOfLexemes <$> getPosition <* eof
    s' <- getState
    return (result ++ [end], s')

