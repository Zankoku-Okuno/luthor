{-# LANGUAGE FlexibleContexts #-}
module Text.Luthor.Lexeme (
      Lexeme(..)
    --creating lexeme streams
    , runLuthorT
    , lexeme
    --consuming lexeme streams
    , satisfy
    , anyLexeme
    , endOfLexemes
    , isAtEnd
    ) where

import Control.Applicative hiding (many)

import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Combinator (eof)

data Lexeme a = Lexeme SourcePos a
              | EndOfLexemes SourcePos
    deriving (Eq)


type LexerT s u m t = ParsecT s u m (Lexeme t)
type LuthorT t u m a = ParsecT [Lexeme t] u m a

runLuthorT :: (Monad m, Stream s m x, Stream [Lexeme t] m y, Show x)
           => LexerT s u m t
           -> LuthorT t u m a
           -> u
           -> SourceName
           -> s
           -> m (Either ParseError a)
runLuthorT lexer parser s source input = do
        e_lexResult <- runPT wrapLexer s source input
        case e_lexResult of
            Left err -> return $ Left err
            Right (input', s') -> runPT parser s' source input'
    where
    wrapLexer = do
        result <- many lexer
        end <- EndOfLexemes <$> getPosition <* eof
        s' <- getState
        return (result ++ [end], s')

lexeme :: (Monad m) => ParsecT s u m a -> ParsecT s u m (Lexeme a)
lexeme parser = Lexeme <$> getPosition <*> try parser


satisfy :: (Show a, Stream [Lexeme a] m (Lexeme a))
        => (a -> Bool)
        -> LuthorT a u m a
satisfy p = tokenPrim _lexShow _lexUpdatePos _lexCheck
    where
    _lexCheck (Lexeme _ x) | p x = Just x
    _lexCheck _ = Nothing

anyLexeme :: (Show a, Stream [Lexeme a] m (Lexeme a))
          => LuthorT a u m a
anyLexeme = tokenPrim _lexShow _lexUpdatePos _lexCheck
    where
    _lexCheck (Lexeme _ x) = Just x
    _lexCheck _ = Nothing

endOfLexemes :: (Show a, Stream [Lexeme a] m (Lexeme a)) => LuthorT a u m ()
endOfLexemes = (<?> "end of stream") $ do
    t <- getInput
    case t of
        (Lexeme _ x:_) -> unexpected $ show x
        (EndOfLexemes _:_) -> return ()

isAtEnd :: (Monad m) => LuthorT a u m Bool
isAtEnd = do
    t <- getInput
    return $ case t of
        (Lexeme _ x:_) -> False
        (EndOfLexemes _:_) -> True


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

