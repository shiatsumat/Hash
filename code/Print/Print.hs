module Print.Print where

line,space :: String->String->String
line a b = a++"\n"++b
space a b = a++" "++b

sep :: [String]->String->String
sep [] _ = ""
sep as b = foldr1 (\a1 a2->a1++b++a2) as

indentN :: Int->String->String
indentN n a = concatMap (\s->(replicate n ' ')++s++"\n") (lines a)

