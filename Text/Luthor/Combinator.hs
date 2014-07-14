{-| Parsec combinators are not composable by default. The idea is that this increases efficiency
    by eliminating unnecessary backtracking. The practical problem with this approach is that it
    is difficult to determine exactly where backtracking really is necessary and insert the needed
    'try' combinators. Any mistakes are almost always subtle and difficult to diagnose and fix.

    Compute power is cheap and programmers are expensive, so it only makes sense to make it easy on
    the programmer first and on the computer second. This module is mostly full of composable drop-in
    replacements for Parsec combinators. There's also some renaming, to make the API more idiomatic.
    Also, some additional combinators are exported.

    When migrating to this API, it is recommended to import Parsec modules qualified so that
    your code will be composable by default. Where efficiency really is needed, you can carefully
    fall back to non-composable Parsec combinators by namespacing the appropriate function calls.
    
    One place where we could not provide a replacement was the '<|>' operator. This is exported by
    the 'Alternative' typeclass, which we really don't want to mess with. The composable alternative
    is spelled '<||>'
-}
module Text.Luthor.Combinator (
    -- * Applicative Parsing
      (<$>), (<$$>), (<*>), (*>), (<*), (<**>)
    , pure
    -- * Choices
    , (<||>), choice, dispatch
    -- ** Zero or One
    , option, optional, optional_
    -- * Many
    , P.many, P.many1
    -- * Common Structures
    -- ** Surround
    , chomp
    , P.between
    , between2
    -- ** Intercalate
    -- ** Chaining
    -- * Lookahead
    , lookAhead
    , notFollowedBy
    -- * Additional Data
    , (<?>), expect
    , withPosition, withPositionEnd, withPositions
    , withState
    -- * Re-exports
    , try, (<|>), P.unexpected
    ) where

import Text.Parsec.Prim (ParsecT, Stream, try, (<|>), (<?>))
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.Combinator as P
import Text.Parsec.Pos

import Control.Applicative hiding ((<|>), optional)
import Control.Monad


infixl 3 <||>
infixl 4 <$$>


{-| Flipped '<$>'. -}
(<$$>) :: Functor f => f a -> (a -> b) -> f b
x <$$> f = f <$> x


{-| @p \<||\> q@ tries to parse @p@, but if it fails, parses @q@.
    
    Unlike the 'Alternative' instance for 'ParsecT', backtracking will occur if @p@ fails.
    That is, a parser such as @string \"flange\" \<||\> string \"fly\"@ will succeed on
    the input @\"fly\"@, whereas its Parsec counterpart will unintuitively fail.
-}
(<||>) :: Stream s m t => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p <||> q = try p <|> q

choice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choice = P.choice . map try

dispatch :: Stream s m t => [(ParsecT s u m a, ParsecT s u m b)] -> ParsecT s u m b
dispatch [] = P.choice []
dispatch ((canary, payload):rest) = do
    go <- maybe False (const True) <$> optional canary
    if go then payload else dispatch rest


{-| @option x p@ tries to apply parser @p@. If @p@ fails, no input is consumed and @x@
    is returned.
-}
option :: Stream s m t => a -> ParsecT s u m a -> ParsecT s u m a
option x p = p <||> pure x

{-| @optional p@ tries to parse @p@, but does not fail or consume input of @p@ fails.

    This is like 'Text.Parsec.Combinator.optionMaybe', but is easier to type. See 'optional_'.
-}
optional :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
optional p = Just <$> p <||> pure Nothing
{-| @optional_ p@ tries to parse @p@, but does not fail or consume input if @p@ fails.
    
    This is like 'Text.Parsec.Combinator.optional', but the use of underscore is more idiomatic
    for actions whose results are ignored.
-}
optional_ :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
optional_ p = void p <||> pure ()


{-
count :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
manyNM
skipMany1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
skipMany

manyTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyThru
-}


chomp :: Stream s m t => ParsecT s u m a -> ParsecT s u m trash -> ParsecT s u m a
chomp p trash = p <* optional_ trash

between2 :: Stream s m t => ParsecT s u m around -> ParsecT s u m a -> ParsecT s u m a
between2 p = P.between p p

{-
sepBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]

chainl :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainl1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainr :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
-}


lookAhead :: Stream s m t => ParsecT s u m a -> ParsecT s u m a
lookAhead = P.lookAhead . try

notFollowedBy :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m t -> ParsecT s u m a
notFollowedBy p la = try $ p <* P.notFollowedBy la


expect :: Stream s m t => String -> ParsecT s u m a -> ParsecT s u m a
expect = flip (<?>)

withPosition :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a, SourcePos)
withPosition p = flip (,) <$> P.getPosition <*> p

withPositionEnd :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a, SourcePos)
withPositionEnd p = (,) <$> p <*> P.getPosition

withPositions :: Stream s m t => ParsecT s u m a -> ParsecT s u m (SourcePos, a, SourcePos)
withPositions p = (,,) <$> P.getPosition <*> p <*> P.getPosition

withState :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a, u)
withState p = (,) <$> p <*> P.getState
