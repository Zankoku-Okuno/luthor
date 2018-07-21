{-| Parsec parsers for common tokens. This module is written with bottom-up programming in mind
    so that is stays flexible. For example, although we export several numerical token parsers,
    we also export the sub-token parsers they were built out of: 'numSign', 'numNatural',
    'numExponent', &c.
-}
module Text.Luthor.Syntax (
    -- * Basic Characters and Strings
      char, string, charI, stringI
    , P.anyChar, P.oneOf, P.noneOf
    , aChar, manyChar, many1Char
    -- ** Common Classes
    , upAlpha, loAlpha, alpha
    , digit, P.hexDigit, P.octDigit, binDigit
    , ctl
    , asciiText, uniText
    -- ** Common Special Literals
    , cr, lf, sp, ht, sq, dq
    , colon, semicolon, dot, comma
    , ellipsis2, ellipsis3
    , bsEsc
    , yes, no, yesno
    -- * Programming Idioms
    -- ** Whitespace
    , lws, newline, lineBreak, crlf
    , bsnl, bsnlwsbs
    , IndentPolicy(..), dentation
    -- ** Identifiers
    , many1Not
    , sigilized
    -- ** Punctuation
    , inParens, inBrackets, inBraces, inAngles
    -- ** Number Parts
    , numSign, numBase, numNatural, numAfterPoint, numDenominator
    , numOptSign, numInteger
    , xDigit, stringToInteger, stringToMantissa
    -- ** Numbers
    , integer, rational, scientific
    , hexOctet
    -- ** Character Escapes
    , letterEsc
    , decimalEsc
    , asciiEsc, loUniEsc, hiUniEsc, uniEsc
    , cEscapes
    -- ** String Literals
    , sqString, dqString
    -- ** Comments
    , lineComment
    , blockComment
    , nestingComment
    -- * Character Classes
    , charClass
    , uniPrint, uniPrintMinus
    , uniId, uniIdMinus
    ) where

import Data.Ratio
import Data.Char
import Data.String (IsString(..))
import Data.CaseInsensitive (CI, mk)
import Data.Maybe
import Data.List

import Text.Parsec (ParsecT, Stream)
import qualified Text.Parsec as P
import Text.Parsec.Char (satisfy, char, oneOf, noneOf)
import Text.Luthor.Combinator

import Control.Monad


-- |@string s@ parses a sequence of characters given by @s@. Returns
--  the parsed string (i.e. @s@). Unlike the Parsec version, this
--  combinator never consumes input on failure.
--
--  >  adrenalineWord  =  string "fight" 
--  >                 <|> string "flight"
string :: (Stream s m Char) => String -> ParsecT s u m String
string = try . P.string

-- |Parse a single character, case-insensitive.
charI :: (Stream s m Char) => Char -> ParsecT s u m (CI Char)
charI c = mk <$> _charI c

-- |Parse a string, case-insensitive. If this parser fails, it consumes no input.
--  Normalized to lowercase.
stringI :: (Stream s m Char) => String -> ParsecT s u m (CI String)
stringI str = expect (show $ mk str) . try $ mk <$> mapM _charI str

_charI :: (Stream s m Char) => Char -> ParsecT s u m Char
_charI (mk -> c) = expect (show c) . satisfy $ (== c) . mk

-- |Parse a single char when it satisfies the predicate.
--  Fails when the next input character does not satisfy the predicate.
aChar :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
aChar = satisfy

-- | Parse /zero/ or more characters satisfying the predicate, c.f. 'many1Char'.
manyChar :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m String
manyChar = many . aChar

-- | Parse /one/ or more characters satisfying the predicate, c.f. 'manyChar'.
many1Char :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m String
many1Char = many1 . aChar


-- |Rule `UPALPHA` from RFC2616 §2.2
upAlpha :: (Stream s m Char) => ParsecT s u m Char
upAlpha = expect "uppercase character" $ satisfy _upAlpha

-- |Rule `LOALPHA` from RFC2616 §2.2
loAlpha :: (Stream s m Char) => ParsecT s u m Char
loAlpha = expect "lowercase character" $ satisfy _loAlpha

-- |Rule `ALPHA` from RFC2616 §2.2
alpha :: (Stream s m Char) => ParsecT s u m Char
alpha = expect "alphabetic character" $ satisfy _alpha

-- |Rule `DIGIT` from RFC2616 §2.2
digit :: (Stream s m Char) => ParsecT s u m Char
digit = expect "digit" $ satisfy _digit

-- |Parse a binary digit ('0' or '1')
binDigit :: (Stream s m Char) => ParsecT s u m Char
binDigit = expect "binary digit" $ oneOf "01"

-- |Rule `CTL` from RFC2616 §2.2
ctl :: (Stream s m Char) => ParsecT s u m Char
ctl = expect "control character" $ satisfy _asciiControl

-- |A single printable ASCII character, as the `TEXT` rule from RFC2616 §2.2 
asciiText :: (Stream s m Char) => ParsecT s u m Char
asciiText = expect "printable ascii character" $ satisfy (\c -> '\32' <= c && c <= '\126')

-- |A single printable unicode character, a generalization of 'text'. See 'uniPrint'.
uniText :: (Stream s m Char) => ParsecT s u m Char
uniText = expect "printable unicode character" $ satisfy uniPrint


-- |A carriage return (ASCII 13)
cr :: (Stream s m Char) => ParsecT s u m ()
cr = expect "carraige return" . void $ char '\r'
-- |A line feed (ASCII 10)
lf :: (Stream s m Char) => ParsecT s u m ()
lf = expect "linefeed" . void $ char '\n'
-- |A space (ASCII 32)
sp :: (Stream s m Char) => ParsecT s u m ()
sp = expect "space" . void $ char ' '
-- |A horizonal tab (ASCII 9)
ht :: (Stream s m Char) => ParsecT s u m ()
ht = expect "tab" . void $ char '\t'
-- |A single quote
sq :: (Stream s m Char) => ParsecT s u m ()
sq = expect "single quote" . void $ char '\''
-- |A double quote
dq :: (Stream s m Char) => ParsecT s u m ()
dq = expect "double quote" . void $ char '\"'
-- |A colon (:)
colon :: (Stream s m Char) => ParsecT s u m ()
colon = expect "colon" . void $ char ':'
-- |A semicolon (;)
semicolon :: (Stream s m Char) => ParsecT s u m ()
semicolon = expect "semicolon" . void $ char ';'
-- |A period (.)
dot :: (Stream s m Char) => ParsecT s u m ()
dot = expect "dot" . void $ char '.'
-- |A comma (,)
comma :: (Stream s m Char) => ParsecT s u m ()
comma = expect "comma" . void $ char ','
-- |Two dots (..)
ellipsis2 :: (Stream s m Char) => ParsecT s u m ()
ellipsis2 = void $ string ".."
-- |Three dots (...)
ellipsis3 :: (Stream s m Char) => ParsecT s u m ()
ellipsis3 = void $ string "..."

-- |A backslash-escape: backslash followed by a single character
--  satisfying the predicate.
bsEsc :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
bsEsc p = try $ char '\\' *> satisfy p

-- |Parse @\"yes\"@, @\"y\"@, or @\"1\"@, case-insensitive. Return True.
yes :: (Stream s m Char) => ParsecT s u m Bool
yes = True <$ choice [
      void $ stringI "yes"
    , void $ charI 'y'
    , void $ char '1'
    ]

-- |Parse @\"no\"@, @\"n\"@, or @\"0\"@, case-insensitive. Return False.
no :: (Stream s m Char) => ParsecT s u m Bool
no = False <$ choice [
      void $ stringI "no"
    , void $ charI 'n'
    , void $ char '0'
    ]

-- |Parse 'yes' or 'no' and return whether the answer was yes-ful.
yesno :: (Stream s m Char) => ParsecT s u m Bool
yesno = yes <|> no


-- |A carriage return + line feed sequence.
crlf :: (Stream s m Char) => ParsecT s u m ()
crlf = expect "CRLF" $ void "\r\n"

-- |Short for \"linear whitespace\": one or more spaces or tabs.
--  Similar to rule `LWS` from RFC2616 §2.2, but without line folding.
lws :: (Stream s m Char) => ParsecT s u m String
lws = expect "linear whitespace" . many1 $ oneOf " \t"

-- |Parse a single line feed or carriage return.
--  Does not succeed at end of file.
newline :: (Stream s m Char) => ParsecT s u m ()
newline = expect "newline" . void $ oneOf "\n\r"

-- |Recognize when the parser is at a line break (LF, CR, or end of input)
--  If the break is due to a CR or LF, consume it.
lineBreak :: (Stream s m Char) => ParsecT s u m ()
lineBreak = expect "line break" $ newline <|> P.eof

-- |Parse a backslash followed by a 'newline'
bsnl :: (Stream s m Char) => ParsecT s u m ()
bsnl = void $ char '\\' *> newline
-- |Parse a backslash followed by a 'newline',
--  then linear whitespace ('lws') and finally another backslash.
bsnlwsbs :: (Stream s m Char) => ParsecT s u m ()
bsnlwsbs = void $ between2 (char '\\') $ newline *> lws


-- |Determine how the depth of indentation is calculated.
data IndentPolicy = DontMix [Char]
                    -- ^Any of the passed Chars can be used, but
                    --  allow only one kind of character in a line.
                    --  Depth is number of those characters.
                  | Convert [(Char, Int)]
                    -- ^Allow any mix of of the passed Chars.
                    --  Calculate depth by assigning a number
                    --  to each of character by kind and summing.

{-| Parse a 'lineBreak' followed by whitespace characters.
    Return the depth of indentation.
    
    The acceptable whitespace characters and the method by whch to
    caclulate depth is determined by an 'IntentPolicy'.

    WARNING: Do not use this combinator with the @Lex@ module, it
    will break because Parsec is sissy.
-}
dentation :: (Stream s m Char) => IndentPolicy -> ParsecT s u m Int
dentation = _dentation lineBreak

{-| Version of 'dentation' suitable for us in lexing parsers built on @Lex@

    This parser will not detect dedents at the end of input. Therefore,
    when you wish to recognize a dedent in your parser, recognize 'endOfInput'
    as well as your dedent tokens.
-}
lexDentation :: (Stream s m Char) => IndentPolicy -> ParsecT s u m Int
lexDentation = _dentation newline

_dentation :: (Stream s m Char) => ParsecT s u m newline -> IndentPolicy -> ParsecT s u m Int
_dentation nl (DontMix cs) = try $ do
    nl
    ws <- P.many $ oneOf cs
    when (length (nub ws) > 1) $ unexpected "mixed indentation"
    return $ length ws
_dentation nl (Convert table) = try $ do
    nl
    ws <- P.many $ oneOf (fst <$> table)
    return $ sum [fromJust $ lookup c table | c <- ws]


{-| Parse one or more characters that satisfy a predicate, but with additional
    restrictions on the first character parsed.

    This is especially useful for identifiers (such as @\/[a-zA-Z_][a-zA-Z_0-9]*\/@)
    and certain kinds of numbers (such as @\/[1-9][0-9]*\/@).

@
identifier = 'charClass' \"a-zA-Z0-9_\" \`many1Not\` 'charClass' \"0-9\"
naturalLiteral = 'stringToInteger' 10 \<$\> 'charClass' \"0-9\" \`many1Not\` (==\'0\')
@
-}
many1Not :: (Stream s m Char)
           => (Char -> Bool) -- ^Whether a character is allowed in the identifier
           -> (Char -> Bool) -- ^Whether the character is /disallowed/ as the first character of the identifier
           -> ParsecT s u m String
many1Not allowed notAtFront = try $ do
    first <- satisfy $ \c -> allowed c && (not . notAtFront) c
    rest <- many $ satisfy allowed
    return (first:rest)

{-| Parse a sigil character immediately followed by an identifier.

@
data Sigil = Scalar | Array
name = sigilized (zip \"$\@\" [Scalar, Array]) $ 'many1Not' (charClass' \"_a-zA-Z0-9\") ('charClass' \"0-9\")
@
-}
sigilized :: (Stream s m Char)
           => [(Char, sigil)] -- ^The sigils and their corresponding semantics
           -> ParsecT s u m a -- ^An identifier parser
           -> ParsecT s u m (sigil, a)
sigilized sigils ident = try $ do
    let wrap (c, s) = (char c, pure s)
    dispatch (wrap <$> sigils) <$$> (,) <*> ident

-- |Parse between open and close parenthesis.
inParens :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
inParens = between (char '(') (char ')')
-- |Parse between open and close square brackets (@[...]@).
inBrackets :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
inBrackets = between (char '[') (char ']')
-- |Parse between open and close curly braces (@{...}@).
inBraces :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
inBraces = between (char '{') (char '}')
-- |Parse between open and close angles (@\<...\>@).
inAngles :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
inAngles = between (char '<') (char '>')


{-| Parse a minus or plus sign and return the appropriate multiplier. -}
numSign :: (Num n, Stream s m Char) => ParsecT s u m n
numSign = dispatch $ zip (char <$> "-+") (pure <$> [-1, 1])

{-| Parse \"0x\", \"0o\", or \"0b\" case-insensitive and return the appropriate base.
    If none of these parse, return base 10.
-}
numBase :: (Stream s m Char) => ParsecT s u m Int
numBase = P.option 10 . dispatch $ zip
    (stringI <$> ["0x", "0o", "0b"])
    (pure <$> [16, 8, 2])

{-| Parse many digits in the passed base and return the corresponding integer. -}
numNatural :: (Integral n, Stream s m Char) => Int -> ParsecT s u m n
numNatural base = stringToInteger base <$> xDigits base

{-| Parse many digits in the passed base and return the appropriate rational. -}
numAfterPoint :: (Fractional n, Stream s m Char) => Int -> ParsecT s u m n
numAfterPoint base = stringToMantissa base <$> xDigits base

{-| Parse a natural in the passed base and return its reciprocal. -}
numDenominator :: (Fractional n, Stream s m Char) => Int -> ParsecT s u m n
numDenominator base = try $ do
    denom <- numNatural base
    if denom == 0 then P.parserZero else return (fromRational $ 1%denom)

-- |Optional sign as 'numSign', defaults to positive.
numOptSign :: (Num n, Stream s m Char) => ParsecT s u m n
numOptSign = P.option 1 numSign

{-| Parse an optional sign (as 'numOptSign'), then a natural number
    of the specified base (as in 'numNatural').
-}
numInteger :: (Integral n, Stream s m Char) => Int -> ParsecT s u m n
numInteger base = numOptSign <$$> (*) <*> numNatural base


{-| Parse an integer: optional sign, then a number of digits.
    Bases 10, 16, 8 and 2 are supported with appropriate
    prefixes as in 'numBase' before the digits.
-}
integer :: (Integral n, Stream s m Char) => ParsecT s u m n
integer = try $ numOptSign <$$> (*) <*> (numNatural =<< numBase)

{-| Parse a rational number: an optional sign, then two sequences
    of digits separated by a slash. Return the ratio of the appropriate
    sign between the two numbers. Bases 10, 16, 8 and 2 are supported.
-}
rational :: (Fractional n, Stream s m Char) => ParsecT s u m n
rational = try $ do
    sign <- numOptSign
    base <- numBase
    numer <- fromRational . (%1) <$> numNatural base
    char '/'
    denom <- numDenominator base
    return $ sign * numer * denom

{-| Parse a number in scientific notation: an optional sign, then
    a radix mark, two sequences of digits separated by a 'dot', and
    finally an optional exponent, which is an exponent letter, an
    optional sign and finally one or more digits in the same base.

    In base ten, the exponenet letter is @e@ (or @E@), and means
    @*10^@. In bases two, eight, and 16, the exponenet letter is
    @p@ (or @P@), and means @*2^@.

    Note that digits are required on both sides of the decimal
    point, so neither @0.@ nor @.14@ are recognized.
-}
scientific :: (Fractional n, Stream s m Char) => ParsecT s u m n
scientific = try $ do
    sign <- numOptSign
    base <- numBase
    whole <- (%1) <$> numNatural base
    dot
    mantissa <- numAfterPoint base
    exponent <- option (1%1) $ case base of
        10 -> do
            exp <- charI 'e' *> numInteger base
            pure $ (10%1) ^^ exp
        _ -> do
            exp <- charI 'p' *> numInteger base
            pure $ (2%1) ^^ exp
    -- let timesExp = (fromIntegral base % 1) ^^ exponent
    return . fromRational $ sign * (whole + mantissa) * exponent

-- |Parse a two-digit hexadacimal number.
hexOctet :: (Stream s m Char, Integral n) => ParsecT s u m n
hexOctet = fromIntegral . stringToInteger 16 <$> count 2 P.hexDigit

-- |Parse a backslash and another character; use the passed table to
--  determine the returned character.
letterEsc :: (Stream s m Char) => [(Char, Char)] -> ParsecT s u m Char
letterEsc table = fromJust . flip lookup table <$> bsEsc (`elem` map fst table)

{-| Common characetr escapes in programming language string literals:

    >\0 -> ASCII 00 (nul)
    >\a -> ASCII 07 (alarm/bell)
    >\b -> ASCII 08 (backspace)
    >\e -> ASCII 1B (escape)
    >\f -> ASCII 0C (form feed)
    >\n -> ASCII 0A (line feed)
    >\r -> ASCII 0D (carriage return)
    >\t -> ASCII 09 (horizontal tab)
    >\v -> ASCII 0B (vertical tab)
    >\' -> single-quote
    >\" -> double-quote
    >\\ -> backslash
-}
cEscapes :: [(Char, Char)]
cEscapes = zip "\\0abefnrtc\'\"\\" "\\\0\a\b\27\f\n\r\t\v\'\""

-- |Escape sequences for any unicode code point.
--  Represented by a backslash + one or more decimal digits.
--  Fails when the code point is not representable in unicode.
decimalEsc :: (Stream s m Char) => ParsecT s u m Char
decimalEsc = try $ do
    char '\\'
    n <- stringToInteger 10 <$> P.many1 digit
    when (n > 0x10FFFF) $ unexpected "code point above U+10FFFF"
    return $ chr n

-- |Escape sequences for bytes (including ASCII).
--  Represented by a backslash + lowercase \'x\' followed by two hexdigits.
asciiEsc :: (Stream s m Char) => ParsecT s u m Char
asciiEsc = try $ stringI "\\x" *> (chr <$> hexOctet)

-- |Escape sequences in the Unicode Basic Multilingual Plane (BMP). C.f. 'hiUniEsc'.
--  Represented by a backslash+lowercase \'u\' followed by four hexdigits.
loUniEsc :: (Stream s m Char) => ParsecT s u m Char
loUniEsc = try $ do
    P.string "\\u"
    chr . stringToInteger 16 <$> P.count 4 P.hexDigit

-- |Escape sequences outside the Unicode BMP. C.f. 'loUniEsc'.
--  Represented by a backslash+uppercase \'U\' followed by five or six
--  hexdigits totalling at most 0x10FFFF
hiUniEsc :: (Stream s m Char) => ParsecT s u m Char
hiUniEsc = try $ do
    P.string "\\U"
    chr . stringToInteger 16 <$> (six <||> five)
    where
    five = optional_ (char '0') *> P.count 5 P.hexDigit
    six = string "10" <$$> (++) <*> P.count 4 P.hexDigit

-- |A unicode escape, either as 'loUniEsc' or 'hiUniEsc'.
uniEsc :: (Stream s m Char) => ParsecT s u m Char
uniEsc = loUniEsc <|> hiUniEsc


{-| Parse a single-quoted string with no escape sequences, except that
    a single-quote in the string is encoded as two single-quote characters.

    This is as single-quoted strings in SQL and Rc (the shell in Plan9).
    It is an excellent encoding for strings because they are so easy to
    validate from untrusted input and escape for rendering, whether it
    is done by human or machine.
-}
sqString :: (Stream s m Char) => ParsecT s u m String
sqString = between2 (char '\'') (P.many $ normal <|> escape)
    where
    normal = satisfy (/='\'')
    escape = '\'' <$ "''"

{-| Parse a double-quoted string with common backslash escape sequences.
    
    * We use 'letterEsc' with the passed table of contents. There is no default table.
    
    * Also, 'decimalEsc', 'asciiEsc' and 'uniEsc' are allowed.
    
    * Further, the @\\&@ stands for no character: it literally adds nothing
        to the string in which it appears, but it can be useful as in
        @\"\\127\\&0\"@.
    
    * Finally, lines can be folded with a backslash-newline-backslash, ignoring
        any 'lws' between the newline and the second backslash. This is preferred
        over a simple backslash-newline, as it reduces any need to remember how
        leading whitespace is treated after a line-fold.

    The escapes parsed by 'letterEsc' are preferred to the other escape
    sequences. Note that there are no escape sequences for backslash or
    double-quote by default (aside from a numerical escape), so you'll
    need to include them in the table for 'letterEsc'.
-}
dqString :: (Stream s m Char) => [(Char, Char)] -> ParsecT s u m String
dqString table = between2 (char '\"') (catMaybes <$> P.many (normal <|> escape <|> empty))
    where
    normal = (Just <$>) . satisfy $ uniPrintMinus (charClass "\\\"")
    escape = (Just <$>) $ letterEsc table <|> decimalEsc <|> asciiEsc <|> uniEsc
    empty = (Nothing <$) $ void "\\&" <|> bsnlwsbs


-- |Parse a comment beginning with the passed string and ending at
--  (but not including) a 'lineBreak'.
lineComment :: (Stream s m Char) => String -> ParsecT s u m String
lineComment start = do
    string start
    P.anyChar `manyTill` lineBreak

{-| Parse a non-nesting comment beginning at the first passed string
    and ending with (and including) the second passed string.

    C.f 'nestingComment'.
-}
blockComment :: (Stream s m Char)
             => String -- ^Start the block comment
             -> String -- ^End the block comment
             -> ParsecT s u m String
blockComment start end = do
    string start
    P.anyChar `manyThru` string end

{-| Parse a nesting block comment.

    C.f. 'blockComment'.
-}
nestingComment :: (Stream s m Char)
               => String -- ^Start a comment
               -> String -- ^End a comment
               -> ParsecT s u m String
nestingComment start end = do
    string start
    concat <$> (inner <|> text) `manyThru` string end
    where
    inner = nestingComment start end >>= \body -> return (start ++ body ++ end)
    text = P.anyChar `manyTill` (string start <|> string end)



{-| Match any character in a set.
    
    >vowel = charClass "aeiou"
    
    Range notation is supported.
    
    >halfAlphabet = charClass "a-nA-N"
    
    To add a literal @\'-\'@ to a set, place it at the beginning or end
    of the string.

    You may also invert the set by placing a caret at the beginning of
    the string.

    >nonVowel = "^aeiou"

    To add a literal @\'^\'@ to a set, place it somewhere other than at
    the beginning of the string.
-}
    

charClass :: String -> (Char -> Bool)
charClass str = case str of
    ('^':str') -> not . go [] [] str'
    _ -> go [] [] str
    where
    go singles ranges [] = \c -> inRange c `any` ranges || c `elem` nub singles
    go singles ranges (lo:'-':hi:rest) = go singles ((lo, hi):ranges) rest
    go singles ranges (c:rest) = go (c:singles) ranges rest
    inRange c (lo, hi) = lo <= c && c <= hi

{-| The class of printable unicode characters, including linear whitesapce.

    * Accepts: Letter, Number, Symbol, Space, Punctuation/Quote, Mark, Format, PrivateUse

    * Does not Accept: LineSeparator, ParagraphSeparator, Control, Surrogate, NotAssigned
-}
uniPrint :: Char -> Bool
uniPrint c = case generalCategory c of
    LineSeparator -> False
    ParagraphSeparator -> False
    Control -> False
    Surrogate -> False
    NotAssigned -> False
    _ -> True -- Letter, Number, Symbol, Space, Punctuation/Quote, Mark, Format, PrivateUse

-- |Accepts characters from 'uniPrint', except those which 
--  satisfy the passed predicate.
uniPrintMinus :: (Char -> Bool) -> (Char -> Bool)
uniPrintMinus p c = uniPrint c && not (p c)

{-| Accepts a wide variety of unicode characters. This is the largest class
    of characters which might be used in a programming language that allows
    unicode identifiers, and probably include a little too much.

    * Accepts: Letter, Mark, Number, Punctuation/Quote, Symbol

    * Does not Accept: Space, LineSeparator, ParagraphSeparator, Control,
        Format, Surrogate, PrivateUse, NotAssigned
-}
uniId :: Char -> Bool
uniId c = case generalCategory c of
    Space -> False
    LineSeparator -> False
    ParagraphSeparator -> False
    Control -> False
    Format -> False
    Surrogate -> False
    PrivateUse -> False
    NotAssigned -> False
    _ -> True --Letter, Mark, Number, Punctuation/Quote, Symbol

-- |Accepts characters from 'uniId', except those which 
--  satisfy the passed predicate.
uniIdMinus :: (Char -> Bool) -> (Char -> Bool)
uniIdMinus p c = uniId c && not (p c)


instance (IsString a, Stream s m Char) => IsString (ParsecT s u m a) where
    fromString x = fromString <$> string x


_upAlpha c = 'A' <= c && c <= 'Z'
_loAlpha c = 'a' <= c && c <= 'z'
_alpha c = _upAlpha c || _loAlpha c
_digit c = '0' <= c && c <= '9'
_alphaNum c = _alpha c || _digit c
_asciiControl c = c <= '\31' || c == '\127'

{-| Parse a digit in the passed base: 2, 8, 10 or 16. -}
xDigit :: (Stream s m Char) => Int -> ParsecT s u m Char
xDigit base = case base of
    2  -> binDigit
    8  -> P.octDigit
    10 -> digit
    16 -> P.hexDigit
    _ -> error "unrecognized base in Text.Luthor.Syntax.xDigit (accepts only 2, 8, 10, or 16)"

xDigits :: (Stream s m Char) => Int -> ParsecT s u m String
xDigits = many1 . xDigit

{-| Interpret a string as an integer in the passed base. -}
stringToInteger :: (Integral n) => Int -> String -> n
stringToInteger base = fromIntegral . foldl impl 0
    where impl acc x = acc * fromIntegral base + (fromIntegral . digitToInt) x

{-| Interpret a string as a mantissa in the passed base. -}
stringToMantissa :: (Fractional n) => Int -> String -> n
stringToMantissa ((%1) . fromIntegral -> base) = fromRational . foldr impl 0
    where
    impl x acc = acc / base + digitToRatio x
    digitToRatio = (/base) . fromIntegral . digitToInt
