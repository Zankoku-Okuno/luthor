/^--haskell/ {print "```haskell"; next}
/^--end/     {print "```"       ; next}
/^-- /  {print substr($0, 4)    ; next}
/^--/   {print substr($0, 3)    ; next}
/^\s*$/ {print ""               ; next}
        {print}
