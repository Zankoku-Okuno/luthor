{-| Convienience module for users of Luthor.

    We export less than @Text.Parsec@, since many of the modules there
    can interfere when using other Luthor modules. So, you pay one
    extra import line in small cases to save compelx imports in
    larger cases.

    One note: Haddock is failing to mention that the 'lookAhead' exported
    by this module is from @Text.Luthor.Combinators@, not @Text.Parsec.Prim@.
-}
module Text.Luthor (
      module Text.Parsec.Prim
    , module Text.Luthor.Combinator
    , ParseError, errorPos
    , SourcePos
    , SourceName, Line, Column
    , sourceName, sourceLine, sourceColumn
    ) where

import Text.Parsec.Prim hiding (lookAhead)
import Text.Luthor.Combinator

import Text.Parsec.Error (ParseError, errorPos)
import Text.Parsec.Pos ( SourcePos
                       , SourceName, Line, Column
                       , sourceName, sourceLine, sourceColumn
                       )