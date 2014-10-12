Change Log
==========

TODO date tbd
-------------
 * `Text.Luthor.Syntax.scientific`: Hexadecimal notation now uses `p` instead of `h`.
   Hopefully this is easier to read.
   Decimal notation can also use `p`.
 * `Text.Luthor.Syntax.scientific`: Now accepts bases 2, 8, 10 and 16, just like the other parsers.
 * Fixed failure-to-backtrack bug in `Text.Luthor.Combinators.many` etal.
 * Added `Text.Luthor.Syntax.ellipsis2` and `.ellipsis3`.
 * Added `Text.Luthor.Syntax.manyChar` and `.many1Char`.