{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Exit

import Text.Luthor
import Text.Luthor.Syntax
import Text.Luthor.Indent
import qualified Text.Parsec as P


data Lisp = Atom String | List [Lisp]
    deriving (Eq, Show)

file :: Parser [Lisp]
file = between2 (optional $ plusBlankline wss) (expr `sepBy` nextline)

expr :: Parser Lisp
expr = ident <||> parenExpr <||> indentExpr

ident :: Parser Lisp
ident = Atom <$> charClass "a-zA-Z0-9_-" `many1Not` charClass "0-9"

parenExpr :: Parser Lisp
parenExpr = inParens bareExpr

indentExpr :: Parser Lisp
indentExpr = List <$> between indent dedent (bareExpr `sepBy1` nextline)

wss :: [Parser ()]
wss = [void lws, void $ lineComment ";"]

ws :: Parser ()
ws = many1_ $ choice wss

bareExpr :: Parser Lisp
bareExpr = List <$> expr `sepAroundBy1` optional ws


main = do
    results <- sequence [
          expr `matches` "(a (b c ) d)"
        , expr `matches` "(a \n  b c \nd)"
        , expr `matches` "(a \n  b c \n;; foo\n x y\nd)"
        , expr `matches` "(a \n  b c \n  \t \n\nd)"
        , file `matches` "(a \n  b c \nd)\n  "
        , file `matches` "e"
        , file `fails` "(a \n  b c \nd)\n  e"
        , file `parses` ";;this is an empty file\n  ;;nothing to see here\n\n\n" $ []
        , file `matches` ("(a b c)" ++ concat (replicate 80 "\n;;comments!"))
        ]
    if id `all` results then exitSuccess else exitFailure


type Parser = ParsecI String ()
run p = runPI p (DontMix " ") wss () ""

test :: Bool -> Parser a -> String -> IO Bool
test expect p input = do
    case (expect, run (p <* endIndent <* P.eof) input) of
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
    case run (p <* endIndent <* P.eof) input of
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