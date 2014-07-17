import System.IO
import System.Exit

import Text.Parsec.Prim hiding (lookAhead)
import qualified Text.Parsec as P
import Text.Parsec.Char (string, char, anyChar)
import Text.Luthor.Combinator

main = do
    results <- sequence [ 
        --check my examples
        --TODO
        -- * Choices
          (string "foo" <||> string "fly") `matches` "foo"
        , (string "foo" <||> string "fly") `matches` "fly"
        , choice [string "foo", string "bar", string "fly"] `matches` "fly"
        , dispatch [(string "foo", string "bar"), (string "fly", string "wheel")] `matches` "foobar"
        , dispatch [(string "foo", string "bar"), (string "fly", string "wheel")] `matches` "flywheel"
        , let isLong x = if x == "lets" then pure () else fail "found the short one"
          in (isLong =<< longestOf [string "let", string "lets"]) `matches` "lets"
        , longestOf [string "let", string "lets"] `matches` "let"
        -- ** Zero or One
        , (optional_ (string "foo") *> string "fly") `matches` "foofly"
        , (optional_ (string "foo") *> string "fly") `matches` "fly"
        -- * Many
        , atLeast 3 (char 'a') `fails` "aa"
        , atLeast 3 (char 'a') `matches` "aaa"
        , atLeast 3 (char 'a') `matches` "aaaa"
        , atMost 3 (char 'a') `matches` ""
        , atMost 3 (char 'a') `matches` "aaa"
        , atMost 3 (char 'a') `fails` "aaaa"
        , manyNM 2 4 (char 'a') `fails` "a"
        , manyNM 2 4 (char 'a') `matches` "aa"
        , manyNM 2 4 (char 'a') `matches` "aaaa"
        , manyNM 2 4 (char 'a') `fails` "aaaaa"
        -- * Common Structures
        -- ** Terminate
        , ((anyChar `manyTill` (string "\n\r")) *> string "\n\r") `matches` "sdga;on\n\r"
        , ((anyChar `manyTill` (string "\n\r"))) `fails` "sdga;on\n\r"
        , ((anyChar `manyThru` (string "\n\r")) *> string "\n\r") `fails` "sdga;on\n\r"
        , ((anyChar `manyThru` (string "\n\r"))) `matches` "sdga;on\n\r"
        , (string "foo" `chomp` char '\n') `matches` "foo\n"
        , (string "foo" `chomp` char '\n') `matches` "foo"
        -- ** Surround
        , (between2 (char '\'') anyChar) `matches` "'a'"
        , (between2 (char '\'') anyChar) `matches` "'''"
        -- ** Intercalate
        , (char 'a' `sepBy` char ' ') `matches` "a a a"
        , ((char 'a' `sepBy` char ' ') *> char ' ') `matches` "a "
        , ((char 'a' `sepBy` char ' ')) `matches` ""
        , ((char 'a' `sepBy` char ' ') *> char ' ') `matches` " "
        , (char 'a' `sepEndBy` char ' ') `matches` "a a a"
        , ((char 'a' `sepEndBy` char ' ')) `matches` "a "
        , ((char 'a' `sepEndBy` char ' ')) `matches` ""
        , ((char 'a' `sepEndBy` char ' ')) `fails` " "
        , (char 'a' `endBy` char ' ') `fails` "a a a"
        , (char 'a' `endBy` char ' ') `matches` "a a a "
        , ((char 'a' `endBy` char ' ')) `matches` "a "
        , ((char 'a' `endBy` char ' ')) `matches` ""
        , ((char 'a' `endBy` char ' ')) `fails` " "
        , (char 'a' `sepAroundBy` char ' ') `matches` "a a a"
        , (char 'a' `sepAroundBy` char ' ') `matches` "a a a "
        , (char 'a' `sepAroundBy` char ' ') `matches` " a a a "
        , (char 'a' `sepAroundBy` char ' ') `matches` " a a a"
        , ((char 'a' `sepAroundBy` char ' ')) `matches` ""
        , ((char 'a' `sepAroundBy` char ' ')) `fails` " "
        -- ** Chaining
        , (char 'a' `chainl1` (const <$ char ' ')) `matches` "a a a"
        , ((char 'a' `chainl1` (const <$ char ' ')) *> char ' ') `matches` "a "
        , (char 'a' `chainr1` (const <$ char ' ')) `matches` "a a a"
        , ((char 'a' `chainr1` (const <$ char ' ')) *> char ' ') `matches` "a "
        -- * Lookahead
        , (lookAhead (string "foo") <|> string "fly") `matches` "fly"
        , (lookAhead (string "foo") <|> string "ly") `fails` "fly"
        , (lookAhead (string "foo") *> string "foo") `matches` "foo"
        , ((char 'a' `notFollowedBy` string "foo") *> string ("fly")) `matches` "afly"
        , (char 'a' `notFollowedBy` string "foo") `fails` "afoo"
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

matches = test True
fails = test False

putErrLn = hPutStrLn stderr