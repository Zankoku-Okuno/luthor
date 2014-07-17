{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Exit

import Text.Luthor
import Text.Luthor.Syntax
import Text.Luthor.Indent
import qualified Text.Parsec as P

main = do
    results <- sequence [
    	False <$ putErrLn "TODO: lots"
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