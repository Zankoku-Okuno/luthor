{-| Parsec combinators are not composable by default. The idea is that this increases efficiency
    by eliminating unnecessary backtracking. The practical problem with this approach is that it
    is difficult to determine exactly where backtracking really is necessary and insert the needed
    'try' combinators. Any mistakes are almost always subtle and difficult to diagnose and fix.

    Compute power is cheap and programmers are expensive, so it only makes sense to make it easy on
    the programmer first and on the computer second. This module is mostly full of composable drop-in
    replacements for Parsec combinators. There's also some renaming, to make the API more idiomatic.
    Also, some additional combinators are exported.

    When migrating to this API, it is recommended to import Parsec modules qualified so that
    your code will be composable by default. Where efficiency really is needed, you can /carefully/
    but easily fall back to non-composable Parsec combinators by namespacing the appropriate
    combinators.
    
    One place where we could not provide a replacement was the '<|>' operator. This is exported by
    the 'Alternative' typeclass, which we really don't want to mess with. The composable alternative
    is spelled '<||>'.

    The re-named parsers are 'option', 'optional', 'optional_', 'many_', and 'many1_'. It may not
    be easiest to switch old habits, but these names are more idiomatic and also reduce the
    amount of typing necessary. Also, we've altered the semantics of 'manyTill' to make room for the
    new 'manyThru' combinator. This gives the user an easy choice whether to consume the terminating
    element. Finally, we've changed the type of 'notFollowedBy' to allow writing
    @x \`notfollowedBy\` y@ in place of @x <* notFollowedBy y@.

    Below are some selected examples where this library is more intuitive:

    * In Parsec, @string \"aaa\" \<|\> string \"aa\"@, will fail on input @\"aa\"@.
        Using this module's '<||>' will succeed.

    * In Parsec, @char 'a' \`sepBy\` char ' ' *> optional (char ' ') *> eof@ will fail on input
        @\"a a \"@. Using this module's 'sepBy' will succeed. Similar results hold for 'sepBy1',
        'chainl', 'chainl1', 'chainr', and 'chainr1'.

    * In Parsec, @lookAhead (many1 digit) *> many1 digit@ will fail on input such as @\"123\"@.
        Similarly, @notFollowedBy (string \"letter\") *> string \"let\"@ will fail on @\"let\"@.
        Using this module's 'lookAhead' or 'notFollowedBy' will succeed in the respective cases.

    * In Parsec, @anyChar \`manyTill\` string \"-->\" *> eof@ will fail on input
        @\"part1 -- part2-->\"@. Using this module's 'manyThru' will succeed with the same semantics.
        This modules 'manyTill' will not consume the @\"-->\"@.
    
    While we're at it, we've also re-exported applicative parsing functions and defined some of our
    own combinators that have been found useful. Applicative parsing is recommended over monadic
    parsing where it will suffice, so we'd rather eliminate the extra @Control.Applicative@ import.
    Among the additional combinators defined here are 'dispatch', 'count', 'atLeast', 'atMost', 'manyNM',
    'chomp', and 'between2'.
-}
module Text.Luthor.Combinator (
    -- * Applicative Parsing
      (<$>), (<$$>), (<*>), (*>), (<*), (<**>), (<$)
    , pure
    -- * Choices
    , (<||>), choice, dispatch
    , longestOf
    -- ** Zero or One
    , option, optional, optional_
    -- * Many
    , P.many, P.many1
    , many_, many1_
    , P.count, atLeast, atMost, manyNM
    , manyOf, manyOf_
    -- * Common Structures
    -- ** Terminate
    , manyTill
    , manyThru
    , chomp
    -- ** Surround
    , P.between
    , between2
    -- ** Intercalate
    , sepBy, sepBy1
    , sepEndBy, sepEndBy1
    , endBy, endBy1
    , sepAroundBy, sepAroundBy1
    -- ** Chaining
    , chainl, chainl1
    , chainr, chainr1
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

import Data.Function (on)
import Data.Maybe
import Data.List
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

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser. Unlike the Parsec version, this one ensures that parsers
-- do not consume input if they fail.
choice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choice = P.choice . map try

{-| Given a map of parsers to parsers, attempt each key parser until one succeeds,
    then perform the value parser. Return the result of the value parser.
-}
dispatch :: Stream s m t => [(ParsecT s u m a, ParsecT s u m b)] -> ParsecT s u m b
dispatch [] = P.choice []
dispatch ((canary, payload):rest) = do
    go <- not . isNothing <$> optional canary
    if go then payload else dispatch rest

longestOf :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
longestOf [] = P.choice []
longestOf ps = do
    results <- catMaybes <$> sequence (wrap <$> ps)
    when (null results) $ P.choice []
    let (result, s') = maximumBy (compare `on` P.statePos . snd) results
    P.setParserState s'
    return result
    where
    wrap p = optional $ do
        s0 <- P.getParserState
        result <- p
        s' <- P.getParserState
        P.setParserState s0
        return (result, s')


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

-- | @atLeast n p@ applies the parser @p@ @n@ or more times.
atLeast :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
atLeast l p = P.count l p <$$> (++) <*> P.many p

-- | @atMost n p@ applies the parser @p@ up to @n@ times.
atMost :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
atMost m p | m <= 0    = pure []
           | otherwise = option [] $ p <$$> (:) <*> atMost (m-1) p

-- | @manyNM n m p@ applies the parser @p@ @n@ or more times up to @m@ times.
manyNM :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
manyNM l m p = P.count l p <$$> (++) <*> atMost (m-l) p

manyOf :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m [a]
manyOf = P.many . choice

manyOf_ :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m ()
manyOf_ = many_ . choice

-- | @many1_ p@ applies the parser @p@ /one/ or more times, skipping
-- its result.
many1_ :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
many1_ = P.skipMany1

-- | @many1_ p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
many_ :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
many_ = P.skipMany

{-| @manyTill p end@ applies parser @p@ /zero/ or more times until
    parser @end@ succeeds. Returns the list of values returned by @p@.
    The @end@ parse /does not/ consume input, c.f. 'manyThru'.
    This parser can be used to scan comments:
   
    >  simpleComment = do{ string "//"
    >                    ; anyChar `manyThru` char '\n'
    >                    }
   
    Note that despite the overlapping parsers @anyChar@ and @char \'\\n\'@,
    there is never a need to add a 'try': the @end@ parser does not consume
    input on failure.
-}
manyTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill p end = p `P.manyTill` lookAhead end

{-| @manyThru p end@ applies parser @p@ /zero/ or more times until
    parser @end@ succeeds. Returns the list of values returned by @p@.
    The @end@ parse /does/ consume input, c.f. 'manyTill'.
    This parser can be used to scan comments:
   
    >  simpleComment = do{ string "<!--"
    >                    ; anyChar `manyThru` string "-->"
    >                    }
   
    Note that despite the overlapping parsers @anyChar@ and @string \"--\>\"@,
    there is no need to add a 'try': the @end@ parser does not consume input
    on failure.
-}
manyThru :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyThru = P.manyTill

{-| @chomp p x@ will parse @p@, then throw away a subsequent
    parse by @x@ provided it succeeds.
    This combinator will only fail when @p@ fails, not when @x@ does.
-}
chomp :: Stream s m t => ParsecT s u m a -> ParsecT s u m trash -> ParsecT s u m a
chomp p trash = p <* optional_ trash

-- | @between2 p q@ is equivalent to @'between' p p q@
between2 :: Stream s m t => ParsecT s u m around -> ParsecT s u m a -> ParsecT s u m a
between2 p = P.between p p


-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- >  commaSep p  = p `sepBy` (symbol ",")
sepBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@. 
sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 p sep = p <$$> (:) <*> (many . try $ sep *> p)

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi
sepEndBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@. 
sepEndBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 p sep = p <$$> (:) <*> option [] (sep *> sepEndBy p sep)

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, seperated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- >   cStatements  = cStatement `endBy` semi
endBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy p sep = P.many $ p <* sep

-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, seperated
-- and ended by @sep@. Returns a list of values returned by @p@. 
endBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy1 p sep = P.many1 $ p <* sep

sepAroundBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepAroundBy p sep = optional_ sep *> sepEndBy p sep

sepAroundBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepAroundBy1 p sep = optional_ sep *> sepEndBy1 p sep


-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are zero occurrences of @p@, the value @x@ is
-- returned.
--
-- C.f. 'chainr'
chainl :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainl p op zero = chainl1 p op <|> pure zero

-- | @chainl1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr    = term   `chainl1` mulop
-- >  term    = factor `chainl1` addop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }
--
-- C.f. 'chainr1'
chainl1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainl1 p op = p >>= rest
    where rest x = (op <*> pure x <*> p >>= rest) <||> pure x

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are no occurrences of @p@, the value @x@ is
-- returned.
--
-- C.f. 'chainl'
chainr :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr p op zero = chainr1 p op <|> pure zero

-- | @chainr1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@.
--
-- C.f. 'chainl1'
chainr1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainr1 p op = p >>= rest
    where rest x = op <*> pure x <*> (p >>= rest) <||> pure x


-- | @lookAhead p@ parses @p@ without consuming any input, even if @p@ fails.
lookAhead :: Stream s m t => ParsecT s u m a -> ParsecT s u m a
lookAhead = P.lookAhead . try

{-| @notFollowedBy p q@ parses @p@, but only when @q@ will fail immediately
    after parsing @p@. Parsing @q@ never consumes input, and if this
    combinator fails, no input is consumed.

    This combinator can be used to implement the \'longest match\' rule.
    For example, when recognizing keywords (for example @let@), we want
    to make sure that a keyword is not followed by a legal identifier
    character, in which case the keyword is actually an identifier
    (for example @lets@). We can program this behavior as follows:
   
    >  keywordLet = string "let" `notFollowedBy` alphaNum
-}
notFollowedBy :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m t -> ParsecT s u m a
notFollowedBy p la = try $ p <* P.notFollowedBy la


-- |Flipped '<?>'.
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
