{-| Provides a set of tools built atop Parsec's user state mechanism
    to aid in building indentation-sensitive parsers. Also redefines some
    familiar functions to hide the state tracking from consideration.

    The indentation state tracks a stack of indentation depth. It is configured
    to know about what characters are allowed as indentation and how to count them.
    It should also be configured with a list of linear whitespace parsers (this
    including comments). With those, the algorithms in this module will be able to
    skip over blank lines.
    Indentation may also be enabled/disabled, such as when parsing between parens
    or braces.

    WARNING: do not attempt to build an indentation-sensitive lexer using @Lex@
    and this module. It is tially broken, and I don't want to sink that much time
    into figuring out what's wrong with it.
-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Luthor.Indent (
    -- * Types
      ParsecIT, ParsecI, IndentState
    , IndentPolicy(..)
    -- * Run Indentation-sensitive Parsers
    , runParserIT, runParserI, runPIT, runPI
    -- * Parse Indentation
    , plusBlankline
    , indent, nextline, dedent
    , dedent'
    --, lexIndent, lexNextline, lexDedent --FIXME
    , startIndent, endIndent
    -- * Read/Write Indentation State
    , isIndentEnabled
    , getIndentDepth
    , withIndentation, withoutIndentation
    -- * State Manipulation
    , getState, putState, modifyState
    , peekIndentation, popIndentation, pushIndentation
    -- * Re-exports
    , module Text.Parsec.Prim
    , ParseError, errorPos
    , SourcePos
    , SourceName, Line, Column
    , sourceName, sourceLine, sourceColumn
    ) where

import Text.Parsec.Prim hiding (lookAhead, getState, putState, modifyState)
import Text.Parsec.Error (ParseError, errorPos)
import Text.Parsec.Pos ( SourcePos
                       , SourceName, Line, Column
                       , sourceName, sourceLine, sourceColumn
                       )

import Control.Monad
import Control.Monad.Identity
import Text.Luthor hiding (getState, putState, modifyState)
import Text.Luthor.Syntax
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.Combinator as P


-- |Opaque type tracking indentation state.
data IndentState s u m = IS { _policy :: IndentPolicy
                            , _depth :: [Int]
                            , _enabled :: Bool
                            , _ws :: ParsecIT s u m ()
                            }
{-| Create a starting 'IndentationState': indentation is initially
    enabled and the indentation depth stack starts with @[0]@.
-}
startIndent :: (Stream s m Char) => IndentPolicy -> [ParsecIT s u m ()] -> IndentState s u m
startIndent policy ws = IS policy [0] True (plusBlankline ws)

{-| Succeed only when the indentation stack is suitably empty:
    is empty or equal to @[0]@, or if indentation is disabled.
-}
endIndent :: (Stream s m Char) => ParsecIT s u m ()
endIndent = do
    s <- snd <$> P.getState
    if not (_enabled s) || null (_depth s) || _depth s == [0]
        then return () else dedent *> endIndent


-- |Type for Parsec parsers tracking indentation.
type ParsecIT s u m = ParsecT s (u, IndentState s u m) m
-- |'ParsecIT' over the identity monad.
type ParsecI s u = Parsec s (u, IndentState s u Identity)

{-| The most general way to run a parser. @runParserIT p state filePath input@
    runs parser @p@ on the input list of tokens @input@, obtained from source
    @filePath@ with the initial user state @st@. Indentation is initially
    enabled and the depth stack begins as @[0]@.
    The @filePath@ is only used in error messages and may be the empty string.
    Returns a computation in the underlying monad @m@ that return either a
    @ParseError@ ('Left') or a value of type @a@ ('Right'). 
-}
runParserIT :: Stream s m Char
            => ParsecIT s u m a -- ^ the parser to run
            -> IndentPolicy -- ^ what characters count as leading space and how they should be counted
            -> [ParsecIT s u m ()] -- ^ a list of linear whitespace parsers
            -> u -- ^ an initial user state
            -> SourceName -- ^ name of the source file from which the input was gathered
            -> s -- ^ input stream
            -> m (Either ParseError a)
runParserIT p policy ws u = runParserT p (u, startIndent policy ws)

{-| As 'runParserIT', but over the Identity monad. -}
runParserI :: Stream s Identity Char
           => ParsecI s u a -- ^ the parser to run
           -> IndentPolicy -- ^ what characters count as leading space and how they should be counted
           -> [ParsecI s u ()] -- ^ a list of linear whitespace parsers
           -> u -- ^ an initial user state
           -> SourceName -- ^ name of the source file from which the input was gathered
           -> s -- ^ input stream
           -> Either ParseError a
runParserI p policy ws u = runParser p (u, startIndent policy ws)

-- |Shortcut for 'runParserIT'
runPIT :: Stream s m Char
       => ParsecIT s u m a
       -> IndentPolicy -> [ParsecIT s u m ()]
       -> u -> SourceName -> s -> m (Either ParseError a)
runPIT = runParserIT

-- |Shortcut for 'runParserI'
runPI :: Stream s Identity Char
      => ParsecI s u a
      -> IndentPolicy -> [ParsecI s u ()]
      -> u -> SourceName -> s -> Either ParseError a
runPI = runParserI


--lexIndent :: (Stream s m Char) => ParsecIT s u m ()
--lexIndent = _indent lexDentation

-- |Parse an indent: as 'dentation' ensuring the result is greater than
--  the current indentation level. Pushes the indentation depth stack.
indent :: (Stream s m Char) => ParsecIT s u m ()
indent = _indent dentation

_indent :: (Stream s m Char) => (IndentPolicy -> ParsecIT s u m Int) -> ParsecIT s u m ()
_indent dent = expect "indent" . try $ do
    () <- _ws . snd =<< P.getState
    n <- getIndentDepth
    policy <- _policy . snd <$> P.getState
    n' <- dent policy
    case n' `compare` n of
        LT -> unexpected "dedent"
        EQ -> unexpected "nextline"
        GT -> return ()
    (u, s) <- P.getState
    let s' = s { _depth = n' : _depth s }
    P.putState (u, s')

-- |Parse an indent: as 'dentation' ensuring the result is equal to
--  the current indentation level.
nextline :: (Stream s m Char) => ParsecIT s u m ()
nextline = _nextline dentation

--lexNextline :: (Stream s m Char) => ParsecIT s u m ()
--lexNextline = _nextline lexDentation

_nextline :: (Stream s m Char) => (IndentPolicy -> ParsecIT s u m Int) -> ParsecIT s u m ()
_nextline dent = expect "nextline" . try $ do
    () <- _ws . snd =<< P.getState
    n <- getIndentDepth
    policy <- _policy . snd <$> P.getState
    n' <- dent policy
    case n' `compare` n of
        LT -> unexpected "dedent"
        EQ -> return ()
        GT -> unexpected "indent"

-- |Parse an indent: as 'dentation' ensuring the result is less than
--  the current indentation level. Pops the indentation depth stack.
--  If more dedents could be parsed, then no input is consumed.
dedent :: (Stream s m Char) => ParsecIT s u m ()
dedent = _dedent dentation

dedent' :: (Stream s m Char) => ParsecIT s u m ()
dedent' = _dedent dentation *> optional_ nextline

--lexDedent :: (Stream s m Char) => ParsecIT s u m ()
--lexDedent = _dedent lexDentation

_dedent :: (Stream s m Char) => (IndentPolicy -> ParsecIT s u m Int) -> ParsecIT s u m ()
_dedent dent = expect "dedent" . try $ do
    () <- _ws . snd =<< P.getState
    n <- getIndentDepth
    policy <- _policy . snd <$> P.getState
    n' <- lookAhead $ dent policy
    case n' `compare` n of
        LT -> return ()
        EQ -> unexpected "nextline"
        GT -> unexpected "indent"
    (u, s) <- P.getState
    let depth' = tail $ _depth s
        s' = s { _depth = depth' }
    when (n' `notElem` depth') $ fail "dedent has no corresponding indent"
    P.putState (u, s')


-- |Test if indentation is enabled.
isIndentEnabled :: (Stream s m t) => ParsecIT s u m Bool
isIndentEnabled = _enabled . snd <$> P.getState

-- |Obtain the current indentation depth.
--  Fails if indentation is disabled.
getIndentDepth :: (Stream s m t) => ParsecIT s u m Int
getIndentDepth = do
    s <- snd <$> P.getState
    when (not $ _enabled s) (fail "indentation disabled")
    let stack = _depth s
    if null stack
        then fail "empty indent depth stack"
        else return (head stack)


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
plusBlankline :: (Stream s m Char) => [ParsecIT s u m ()] -> ParsecIT s u m ()
plusBlankline ps = manyOf_ $ ps ++ [blankline]
    where
    blankline = expect "" . try $ newline *> manyOf ps *> lookAhead lineBreak


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


-- |Peek the top of the depth stack.
peekIndentation :: (Monad m) => ParsecIT s u m Int
peekIndentation = do
    stack <- _depth . snd <$> P.getState
    when (null stack) $ fail "empty indent depth stack"
    return $ head stack

-- |Pop the top of the depth stack and return the popped depth.
popIndentation :: (Monad m) => ParsecIT s u m Int
popIndentation = do
    (u, s) <- P.getState
    let stack = _depth s
    when (null stack) $ fail "empty indent depth stack"
    P.putState (u, s { _depth = tail stack })
    return $ head stack

-- |Push to the top of the depth stack.
pushIndentation :: (Monad m) => Int -> ParsecIT s u m ()
pushIndentation n = do
    (u, s) <- P.getState
    let stack = _depth s
    P.putState (u, s { _depth = n:stack })
