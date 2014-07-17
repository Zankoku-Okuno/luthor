import Text.Luthor
import Text.Luthor.Syntax
import Text.Luthor.Indent
import Text.Luthor.Lex

data Lisp = Atom Atom | List [Lisp]
data Atom = ANumber Rational
          | AChar Char
          | AString String
          | ASymbol String
          | AFunction [String] Lisp

main = putStrLn "Goodbyte, cruel world!"