module Print.Print where

line :: String->String->String
line a b = a++"\n"++b

indentN :: Int->String->String
indentN n a = concatMap (\s->(replicate n ' ')++s++"\n") (lines a)
indent :: String->String
indent a = indentN 4 a
