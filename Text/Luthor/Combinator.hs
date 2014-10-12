{-# LANGUAGE ConstraintKinds #-}
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

    * In Parsec, @(char \'a\' \`sepBy\` char \' \') *> optional (char \' \') *> eof@ will fail on input
        @\"a a \"@. Using this module's 'sepBy' will succeed. Similar results hold for 'sepBy1',
        'chainl', 'chainl1', 'chainr', and 'chainr1'.

    * In Parsec, @anyChar \`manyTill\` string \"-->\" *> eof@ will fail on input
        @\"part1 -- part2-->\"@. Using this module's 'manyThru' will succeed with the same semantics.
        This module's 'manyTill' will not consume the @\"-->\"@.
    
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
    , many, many1
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
    , atEndOfInput, endOfInput
    -- * Input Stream
    , allInput
    , withRemainingInput
    -- * Additional Data
    , (P.<?>), expect
    , withPosition, withPositionEnd, withPositions
    -- * Re-exports
    , P.try, (P.<|>), P.unexpected, void
    ) where

import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.Combinator as P
import Text.Parsec.Pos

import Data.Function (on)
import Data.Maybe
import Data.List
import Control.Applicative hiding ((<|>), optional, many)
import Control.Monad

--because apparently, methods are imported whether you like it or not?
type ParsecT = P.ParsecT
type Stream = P.Stream

infixl 3 <||>
infixl 4 <$$>


{-| Flipped '<$>'.

    Great for parsing infixes, e.g. @addExpr = expr <$$> (+) <*> expr@.
-}
(<$$>) :: Functor f => f a -> (a -> b) -> f b
x <$$> f = f <$> x


{-| @p \<||\> q@ tries to parse @p@, but if it fails, parses @q@.
    
    Unlike the 'Alternative' instance for 'ParsecT', backtracking will occur if @p@ fails.
    That is, a parser such as @string \"flange\" \<||\> string \"fly\"@ will succeed on
    the input @\"fly\"@, whereas its Parsec counterpart will unintuitively fail.
-}
(<||>) :: Stream s m t => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p <||> q = P.try p P.<|> q

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser. Unlike the Parsec version, this one ensures that parsers
-- do not consume input if they fail.
choice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choice = P.choice . map P.try

{-| Given a map of parsers to parsers, attempt each key parser until one succeeds,
    then perform the value parser. Return the result of the value parser.
-}
dispatch :: Stream s m t => [(ParsecT s u m a, ParsecT s u m b)] -> ParsecT s u m b
dispatch [] = P.choice []
dispatch ((recognize, payload):rest) = do
    go <- isJust <$> optional recognize
    if go then payload else dispatch rest

{-| Attempt all of the passed parsers under the current conditions and
    return the value of the parser which makes it furthest into the
    input stream (and updates the parser's internals as if that were the only
    parser parsed).

    >longestOf [string "do", string "don't"]
-}
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

-- |Parse /zero/ or more of any mix of the passed parsers.
manyOf :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m [a]
manyOf = P.many . choice

-- |As 'manyOf', but ignoring the results.
manyOf_ :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m ()
manyOf_ = many_ . choice

many :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
many = P.many . P.try

many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
many1 = P.many1 . P.try

-- | @many1_ p@ applies the parser @p@ /one/ or more times, skipping
-- its result.
many1_ :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
many1_ = P.skipMany1 . P.try

-- | @many1_ p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
many_ :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
many_ = P.skipMany . P.try

{-| @manyTill p end@ applies parser @p@ /zero/ or more times until
    parser @end@ succeeds. Returns the list of values returned by @p@.
    The @end@ parse /does not/ consume input, c.f. 'manyThru'.
    This parser can be used to scan comments:
   
    >  simpleComment = do { string "//"
    >                     ; anyChar `manyTill` char '\n'
    >                     }
   
    Note that despite the overlapping parsers @anyChar@ and @char \'\\n\'@,
    there is never a need to add a 'try': the @end@ parser does not consume
    input on failure.
-}
manyTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill p end = p `P.manyTill` lookAhead end

{-| @manyThru p end@ applies parser @p@ /zero/ or more times until
    parser @end@ succeeds. Returns the list of values returned by @p@.
    The @end@ parse /does/ consume input, c.f. 'manyTill', but is not
    included in the result.
    This parser can be used to scan comments:
   
    >  simpleComment = do { string "<!--"
    >                     ; anyChar `manyThru` string "-->"
    >                     }
   
    Note that despite the overlapping parsers @anyChar@ and @string \"--\>\"@,
    there is no need to add a 'try': the @end@ parser does not consume input
    on failure.
-}
manyThru :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyThru p end = p `P.manyTill` P.try end

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
sepBy p sep = option [] $ sepBy1 p sep

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@. 
sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 p sep = p <$$> (:) <*> (many $ sep *> p)

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi
sepEndBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy p sep = option [] $ sepEndBy1 p sep

{-| @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
    separated and optionally ended by @sep@. Returns a list of values
    returned by @p@.
-}
sepEndBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 p sep = p <$$> (:) <*> option [] (sep *> sepEndBy p sep)

{-| @endBy p sep@ parses /zero/ or more occurrences of @p@, seperated
    and ended by @sep@. Returns a list of values returned by @p@.
    
    >   cStatements  = cStatement `endBy` semi
-}
endBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy p sep = many $ p <* sep

{-| @endBy1 p sep@ parses /one/ or more occurrences of @p@, seperated
    and ended by @sep@. Returns a list of values returned by @p@.
-}
endBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy1 p sep = many1 $ p <* sep

{-| @sepAroundBy p sep@ parses /zero/ or more occurrences of @p@,
    separated and optionally starting with and ended by @sep@. Returns
    a list of values returned by @p@. 
-}
sepAroundBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepAroundBy p sep = option [] $ sepAroundBy1 p sep

{-| @sepAroundBy1 p sep@ parses /one/ or more occurrences of @p@,
    separated and optionally starting with and ended by @sep@. Returns
    a list of values returned by @p@. 
-}
sepAroundBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepAroundBy1 p sep = optional_ sep *> sepEndBy1 p sep


{-| @chainl p op x@ parses /zero/ or more occurrences of @p@,
    separated by @op@. Returns a value obtained by a /left/ associative
    application of all functions returned by @op@ to the values returned
    by @p@. If there are zero occurrences of @p@, the value @x@ is
    returned.
    
    C.f. 'chainr'
-}
chainl :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainl p op zero = chainl1 p op P.<|> pure zero

{-| @chainl1 p op x@ parses /one/ or more occurrences of @p@,
    separated by @op@ Returns a value obtained by a /left/ associative
    application of all functions returned by @op@ to the values returned
    by @p@. This parser can for example be used to eliminate left
    recursion which typically occurs in expression grammars.
    
    >  expr    = term   `chainl1` mulop
    >  term    = factor `chainl1` addop
    >  factor  = parens expr <|> integer
    >
    >  mulop   =   do{ symbol "*"; return (*)   }
    >          <|> do{ symbol "/"; return (div) }
    >
    >  addop   =   do{ symbol "+"; return (+) }
    >          <|> do{ symbol "-"; return (-) }
    
    C.f. 'chainr1'
-}
chainl1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainl1 p op = p >>= rest
    where rest x = (op <*> pure x <*> p >>= rest) <||> pure x

{-| @chainr p op x@ parses /zero/ or more occurrences of @p@,
    separated by @op@ Returns a value obtained by a /right/ associative
    application of all functions returned by @op@ to the values returned
    by @p@. If there are no occurrences of @p@, the value @x@ is
    returned.
    
    C.f. 'chainl'
-}
chainr :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr p op zero = chainr1 p op P.<|> pure zero

{-| @chainr1 p op x@ parses /one/ or more occurrences of @p@,
    separated by @op@ Returns a value obtained by a /right/ associative
    application of all functions returned by @op@ to the values returned
    by @p@.
    
    C.f. 'chainl1'
-}
chainr1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainr1 p op = p >>= rest
    where rest x = op <*> pure x <*> (p >>= rest) <||> pure x


{-| @lookAhead p@ parses @p@ without consuming any input, even if @p@ fails. -}
lookAhead :: Stream s m t => ParsecT s u m a -> ParsecT s u m a
lookAhead = P.lookAhead . P.try

-- |As @lookAhead@, but throw away result.
lookAhead_  :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
lookAhead_ = void . lookAhead

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
notFollowedBy :: (Stream s m t, Show trash) => ParsecT s u m a -> ParsecT s u m trash -> ParsecT s u m a
notFollowedBy p la = P.try $ p <* P.notFollowedBy la

-- |Returns 'True' if there is no input left, 'False' if there is.
atEndOfInput :: (Stream s m t, Show t) => ParsecT s u m Bool
atEndOfInput = option False $ True <$ endOfInput

-- |Succeed only when at the end of the input stream.
endOfInput :: (Stream s m t, Show t) => ParsecT s u m ()
endOfInput = P.eof


-- |Uses the passed parser, but succeeds only if it consumes all of the input.
allInput :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
allInput = (<* endOfInput)

-- |Parse using the passed parser, but also return the input that was not consumed.
withRemainingInput :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a, s)
withRemainingInput p = p <$$> (,) <*> P.getInput


-- |Flipped '<?>'.
expect :: Stream s m t => String -> ParsecT s u m a -> ParsecT s u m a
expect = flip (P.<?>)

{-| Annotate the return value of the passed parser with the position
    just before parsing.
-}
withPosition :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a, SourcePos)
withPosition p = flip (,) <$> P.getPosition <*> p

{-| Annotate the return value of the passed parser with the position
    just after parsing.
-}
withPositionEnd :: Stream s m t => ParsecT s u m a -> ParsecT s u m (a, SourcePos)
withPositionEnd p = (,) <$> p <*> P.getPosition

{-| Annotate the return value of the passed parser with the position
    just before and after parsing respectively.
-}
withPositions :: Stream s m t => ParsecT s u m a -> ParsecT s u m (SourcePos, a, SourcePos)
withPositions p = (,,) <$> P.getPosition <*> p <*> P.getPosition
