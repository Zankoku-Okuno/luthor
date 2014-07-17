{-# LANGUAGE FlexibleContexts #-}
module Text.Luthor.Indent (
    -- * Types
      ParsecIT, ParsecI, IndentState
    , IndentPolicy(..)
    -- * Run Indentation-sensitive Parsers
    , runParserIT, runParserI, runPIT, runPI
    -- * Parse Indentation
    , mkWs
    , indent, nextline, dedent
    -- * Read/Write Indentation State
    , isIndentEnabled
    , getIndentDepth
    , withIndentation, withoutIndentation
    -- * Re-exports and Overrides
    , module Text.Luthor
    , getState, putState, modifyState
    ) where

import Control.Monad
import Control.Monad.Identity
import Text.Luthor hiding (getState, putState, modifyState)
import Text.Luthor.Syntax
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.Combinator as P


-- |Opaque type tracking indentation state.
data IndentState = IS { _policy :: IndentPolicy
                      , _depth :: [Int]
                      , _enabled :: Bool
                      }
startIndent :: IndentPolicy -> IndentState
startIndent policy = IS policy [0] True

-- |Type for Parsec parsers tracking indentation.
type ParsecIT s u m a = ParsecT s (u, IndentState) m a
-- |'ParsecIT' over the identity monad.
type ParsecI s u a = Parsec s (u, IndentState) a

{-| The most general way to run a parser. @runParserIT p state filePath input@
    runs parser @p@ on the input list of tokens @input@, obtained from source
    @filePath@ with the initial user state @st@. Indentation is initially
    enabled and the depth stack begins as @[0]@.
    The @filePath@ is only used in error messages and may be the empty string.
    Returns a computation in the underlying monad @m@ that return either a
    @ParseError@ ('Left') or a value of type @a@ ('Right'). 
-}
runParserIT :: Stream s m t => ParsecIT s u m a -> IndentPolicy -> u -> SourceName -> s -> m (Either ParseError a)
runParserIT p policy u = runParserT p (u, startIndent policy)

{-| As 'runParserIT', but over the Identity monad. -}
runParserI :: Stream s Identity t => ParsecI s u a -> IndentPolicy -> u -> SourceName -> s -> Either ParseError a
runParserI p policy u = runParser p (u, startIndent policy)

-- |Shortcut for 'runParserIT'
runPIT :: Stream s m t => ParsecIT s u m a -> IndentPolicy -> u -> SourceName -> s -> m (Either ParseError a)
runPIT = runParserIT

-- |Shortcut for 'runParserI'
runPI :: Stream s Identity t => ParsecI s u a -> IndentPolicy -> u -> SourceName -> s -> Either ParseError a
runPI = runParserI


-- |Parse an indent: as 'dentation' ensuring the result is greater than
--  the current indentation level. Pushes the indentation depth stack.
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

-- |Parse an indent: as 'dentation' ensuring the result is equal to
--  the current indentation level.
nextline :: (Stream s m Char) => ParsecIT s u m ()
nextline = try $ do
    n <- getIndentDepth
    policy <- _policy . snd <$> P.getState
    n' <- dentation policy
    case n `compare` n' of
        LT -> unexpected "dedent"
        EQ -> return ()
        GT -> unexpected "indent"

-- |Parse an indent: as 'dentation' ensuring the result is less than
--  the current indentation level. Pops the indentation depth stack.
--  If more dedents could be parsed, then no input is consumed.
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


-- |Test if indentation is enabled.
isIndentEnabled :: (Stream s m t) => ParsecIT s u m Bool
isIndentEnabled = _enabled . snd <$> P.getState

-- |Obtain the current indentation depth.
--  Fails if indentation is disabled.
getIndentDepth :: (Stream s m t) => ParsecIT s u m Int
getIndentDepth = do
    s <- snd <$> P.getState
    when (not $ _enabled s) parserZero
    let stack = _depth s
    if null stack then parserZero else return (head stack)


-- |Run the passed parser with indentation enabled.
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

-- |Run the passed parser with indentation disabled.
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


-- |Alternate version of Parsec's @getState@ suited for indentation-sensitive parsers.
getState :: (Monad m) => ParsecIT s u m u
getState = fst <$> P.getState

-- |Alternate version of Parsec's @puttState@ suited for indentation-sensitive parsers.
putState :: (Monad m) => u -> ParsecIT s u m ()
putState x = void $ P.updateParserState $
    \s@State {stateUser = (_, i)} -> s { stateUser = (x, i) }

-- |Alternate version of Parsec's @modifyState@ suited for indentation-sensitive parsers.
modifyState :: (Monad m) => (u -> u) -> ParsecIT s u m ()
modifyState f = void $ P.updateParserState $
    \s@State {stateUser = (u, i)} -> s { stateUser = (f u, i) }

