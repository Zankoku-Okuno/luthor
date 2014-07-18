/^-- /  {print substr($0, 4); next}
/^--/   {print substr($0, 3); next}
/^\s*$/ {print ""           ; next}
        {print "    ", $0   }
