{-# LANGUAGE FlexibleContexts #-}
module Text.Luthor.Indent (
      ParsecIT, ParsecI, IndentState
    , mkWs
    , indent, nextline, dedent
    , isIndentEnabled
    , getIndentDepth
    , withIndentation, withoutIndentation
    , getState, putState, modifyState
    ) where

import Control.Monad
import Text.Parsec.Prim (ParsecT, Parsec, Stream, State(..), parserZero)
import qualified Text.Parsec.Prim as P
import Text.Luthor.Combinator
import qualified Text.Parsec.Combinator as P
import Text.Luthor.Syntax


-- |Opaque type tracking indentation state.
data IndentState = IS { _policy :: IndentPolicy
                      , _depth :: [Int]
                      , _enabled :: Bool
                      }

-- |Type for Parsec parsers tracking indentation.
type ParsecIT s u m a = ParsecT s (u, IndentState) m a
-- |'ParsecIT' over the identity monad.
type ParsecI s u a = Parsec s (u, IndentState) a


indent :: (Stream s m Char) => ParsecIT s u m ()
indent = try $ do
    n <- getIndentDepth
    policy <- _policy . snd <$> P.getState
    n' <- dentation policy
    case n `compare` n' of
        LT -> unexpected "dedent"
        EQ -> unexpected "nextline"
        GT -> return ()
    (u, s) <- P.getState
    let s' = s { _depth = n' : _depth s }
    P.putState (u, s')

nextline :: (Stream s m Char) => ParsecIT s u m ()
nextline = try $ do
    n <- getIndentDepth
    policy <- _policy . snd <$> P.getState
    n' <- dentation policy
    case n `compare` n' of
        LT -> unexpected "dedent"
        EQ -> return ()
        GT -> unexpected "indent"

dedent :: (Stream s m Char) => ParsecIT s u m ()
dedent = try $ do
    n <- getIndentDepth
    policy <- _policy . snd <$> P.getState
    (n', State rest pos (u, s)) <- lookAhead $ dentation policy <$$> (,) <*> P.getParserState
    case n `compare` n' of
        LT -> return ()
        EQ -> unexpected "nextline"
        GT -> unexpected "indent"
    let depth' = tail $ _depth s
        s' = s { _depth = depth' }
    if null depth' || head depth' == n'
        then void $ P.setParserState (State rest pos (u, s'))
        else P.putState (u, s')


isIndentEnabled :: (Stream s m t) => ParsecIT s u m Bool
isIndentEnabled = _enabled . snd <$> P.getState

getIndentDepth :: (Stream s m t) => ParsecIT s u m Int
getIndentDepth = do
    s <- snd <$> P.getState
    when (not $ _enabled s) parserZero
    let stack = _depth s
    if null stack then parserZero else return (head stack)


withIndentation :: (Stream s m t) => ParsecIT s u m a -> ParsecIT s u m a
withIndentation p = do
    (u, s) <- P.getState
    let enabled0 = _enabled s
        s' = s { _enabled = True }
    try $ do
        P.putState (u, s')
        result <- p
        (u, s) <- P.getState
        let s' = s { _enabled = enabled0 }
        P.putState (u, s')
        return result

withoutIndentation :: (Stream s m t) => ParsecIT s u m a -> ParsecIT s u m a
withoutIndentation p = do
    (u, s) <- P.getState
    let enabled0 = _enabled s
        s' = s { _enabled = False }
    try $ do
        P.putState (u, s')
        result <- p
        (u, s) <- P.getState
        let s' = s { _enabled = enabled0 }
        P.putState (u, s')
        return result


{-| Take a list of some linear whitespace tokens and return
    a parser that advances over linear whitespace and blank lines.
    Also, when indentation is disabled, also advance over 'lineBreak's.

    You will almost always want to use this combinator before 'indent',
    'nextline' and 'dedent' to make sure indentation is always detected.
    Actual linear whitespace is usually not enough: remember to add
    parsers for comments as well. Line comments shouldn't eat the
    newline; the 'lineComment' combinator is acceptable.
-}
mkWs :: (Stream s m Char) => [ParsecIT s u m ()] -> ParsecIT s u m ()
mkWs ps = manyOf_ $ ps ++ [blankline]
    where
    blankline = do
        P.notFollowedBy P.eof
        try $ lineBreak *> manyOf ps *> lookAhead lineBreak


getState :: (Monad m) => ParsecIT s u m u
getState = fst <$> P.getState

putState :: (Monad m) => u -> ParsecIT s u m ()
putState x = void $ P.updateParserState $
    \s@State {stateUser = (_, i)} -> s { stateUser = (x, i) }

modifyState :: (Monad m) => (u -> u) -> ParsecIT s u m ()
modifyState f = void $ P.updateParserState $
    \s@State {stateUser = (u, i)} -> s { stateUser = (f u, i) }

