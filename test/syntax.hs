{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Exit

import Text.Luthor
import Text.Luthor.Syntax
import qualified Text.Parsec as P
import Data.Ratio

main = do
    results <- sequence [ 
        -- * Basic Characters and Strings
          (string "aaa" <|> string "aa") `matches` "aa"
        , ("aaa" <|> "aa") `matches` "aaa"
        , (stringI "aaa" <|> stringI "aa") `parses` "AAA" $ "AAA"
        , (stringI "aaa" <|> stringI "aa") `parses` "aaa" $ "aaa"
        , (stringI "AAA" <|> stringI "AA") `parses` "aA" $ "aA"
        -- ** Common Special Literals
        , bsEsc (=='a') `matches` "\\a"
        , ((:[]) <$> bsEsc (=='a') <|> "\\\\") `parses` "\\\\" $ "\\\\"
        , yesno `parses` "y" $ True
        , yesno `parses` "YeS" $ True
        , yesno `parses` "1" $ True
        , yesno `parses` "n" $ False
        , yesno `parses` "nO" $ False
        , yesno `parses` "0" $ False
        -- ** Whitespace
        , count 4 lineBreak `matches` "\n\n\r"
        -- ** Identifiers
        , (charClass "a-z0-9" `many1Not` charClass "0-9") `matches` "id10t"
        , (charClass "a-z0-9" `many1Not` charClass "0-9") `matches` "f"
        , (charClass "a-z0-9" `many1Not` charClass "0-9") `fails` "1f"
        , (charClass "a-z0-9" `many1Not` charClass "0-9") `fails` ""
        , sigilized [('$', "Scalar"), ('@', "Array")] "foo" `parses` "$foo" $ ("Scalar", "foo")
        , sigilized [('$', "Scalar"), ('@', "Array")] "foo" `parses` "@foo" $ ("Array", "foo")
        , sigilized [('$', "Scalar"), ('@', "Array")] "foo" `fails` "foo"
        -- ** Punctuation
        , inParens "foo" `matches` "(foo)"
        , inBrackets "foo" `matches` "[foo]"
        , inBraces "foo" `matches` "{foo}"
        , inAngles "foo" `matches` "<foo>"
        -- ** Number Parts
        , numSign `parses` "+" $ 1
        , numSign `parses` "-" $ (-1)
        , numBase `parses` "0b" $ 2
        , numBase `parses` "0o" $ 8
        , numBase `parses` "0x" $ 16
        , numBase `parses` "0X" $ 16
        , numBase `parses` "" $ 10
        , numNatural 10 `parses` "1253" $ 1253
        , numNatural 16 `parses` "f1" $ 241
        , numNatural 8 `parses` "702" $ 2+7*64
        , numNatural 2 `parses` "01001" $ 9
        , numAfterPoint 10 `parses` "123" $ 123%1000
        , numDenominator 16 `parses` "1f4D" $ 1%0x1f4d
        , numOptSign `parses` "+" $ 1
        , numOptSign `parses` "-" $ (-1)
        , numOptSign `parses` "" $ 1
        , numInteger 10 `parses` "+12" $ 12
        , numInteger 10 `parses` "-12" $ (-12)
        , numInteger 10 `parses` "12" $ 12
        -- ** Numbers
        , integer `parses` "124" $ 124
        , integer `parses` "-0x1a4" $ (-0x1a4)
        , integer `parses` "+0O702" $ 2+7*64
        , integer `parses` "0b0100" $ 4
        , rational `parses` "42/137" $ 42%137
        , rational `matches` "0x30/ff"
        , rational `matches` "0o30/71"
        , rational `matches` "0B111010001001/10101"
        , scientific `fails` "0."
        , scientific `fails` ".0"
        , scientific `parses` "1.4" $ 14%10
        , scientific `parses` "1.4e2" $ 140
        , scientific `parses` "-1.4e+20" $ (-1.4e20)
        , scientific `parses` "1.4e-3" $ 0.0014
        , scientific `matches` "0x1fda.f00d"
        , scientific `parses` "0x0.f" $ 15%16
        , scientific `matches` "-0x1fda.f00dh7a"
        , scientific `matches` "-0x1fda.0h-7a"
        -- ** Character Escapes
        , many (letterEsc $ zip "ane" "\a\n\27") `parses` "\\a\\n\\e" $ "\a\n\27"
        , many decimalEsc `parses` "\\51\\87\\0" $ "\51\87\0"
        , many asciiEsc `parses` "\\x42\\Xaf\\x1D" $ "\66\175\29"
        , many loUniEsc `parses` "\\u03bB" $ "\955"
        , many hiUniEsc `parses` "\\U0D00Ba\\U10feed" $ "\852154\1113837"
        -- ** String Literals
        , sqString `parses` "'it''s\\\n me'" $ "it's\\\n me"
        , dqString cEscapes `matches` "\"sdag\\n\\e\\235\\xed\\u1010\\U101010\""
        , dqString cEscapes `parses` "\"\\127\\&0\"" $ "\127\&0"
        , dqString cEscapes `parses` "\"a\\\n   \\b\"" $ "ab"
        , dqString cEscapes `fails` "\"a\\   b\""
        -- ** Comments
        , lineComment "--" `matches` "--foabsf"
        , (lineComment "--" *> char '\n') `matches` "--foabsf\n"
        , lineComment "--" `fails` "--foabsf\nfad"
        , blockComment "/*" "*/" `matches` "/*as**d\ngn\r;*/"
        , blockComment "/*" "*/" `matches` "/**/"
        , blockComment "/*" "*/" `fails` "/*"
        , blockComment "/*" "*/" `matches` "/*/**/"
        , nestingComment "{-" "-}" `matches` "{-as--d\ngn\r;-}"
        , nestingComment "{-" "-}" `matches` "{--}"
        , nestingComment "{-" "-}" `fails` "{-"
        , nestingComment "{-" "-}" `fails` "{-{--}"
        , nestingComment "{-" "-}" `matches` "{-asf{-saf-}fa-}"
        , nestingComment "{-" "-}" `matches` "{-{-{-{--}-}-}-}"
        -- * Character Classes
        , many (aChar (charClass "aeiou")) `matches` "eaiou"
        , many (aChar (charClass "-aeiou")) `matches` "ao-eiu"
        , many (aChar (charClass "aeiou-")) `matches` "ao-eiu"
        , many (aChar (charClass "-")) `matches` "---"
        , many (aChar (charClass "^asf")) `fails` "db32tq\955dfbh"
        , many (aChar (charClass "^-asf")) `fails` "-"
        , many (aChar (charClass "^asf-")) `fails` "-"
        , many (aChar (charClass "^-")) `fails` "-"
        ]
    if id `all` results then exitSuccess else exitFailure


type Parser = Parsec String ()
run p = runP p () ""

test :: Bool -> Parser a -> String -> IO Bool
test expect p input = do
    case (expect, run (p <* P.eof) input) of
        (True, Right _) -> return True
        (False, Left _) -> return True
        (True, Left err) -> do
            putErrLn $ "false negative on " ++ show input
            putErrLn $ show err
            return False
        (False, Right _) -> do
            putErrLn $ "false positive on " ++ show input
            return False

parses :: (Show a, Eq a) => Parser a -> String -> a -> IO Bool
parses p input expect = do
    case run (p <* P.eof) input of
        Right val | val == expect -> return True
                  | otherwise -> do
                        putErrLn $ "incorrect parse on " ++ show input
                        putErrLn $ "found " ++ show val ++ " expecting " ++ show expect
                        return False
        Left err -> do
            putErrLn $ "false negative on " ++ show input
            putErrLn $ show err
            return False

matches = test True
fails = test False

putErrLn = hPutStrLn stderr