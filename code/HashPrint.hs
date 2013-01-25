module HashPrint where

import Print.Print
import HashToken
import HashParse

printToken :: [Token]->String
printToken ts = show ts

compile :: String->String
compile = printToken.eval
