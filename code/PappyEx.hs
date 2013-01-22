module PappyEx where
import Data.Char
import Pos
import Parse

type LargeParseFunc d = Pos -> String -> d
type ParseFunc d v = d -> Result d v

autochar prs pos s = case s of
                    (c:s') -> Parsed c (prs (nextPos pos c) s') (nullError d)
                    [] -> NoParse (eofError d)
                where d = prs pos s

notChar c = noneOf [c]

noneOfStr :: Derivs d => [String]->Parser d String
noneOfStr ss = many ((noneOf $ map head ss) </> (choice $ map prs ss))
        where prs (c:s) = do{char c; notFollowedBy $ string s; return c}
notStr :: Derivs d => String->Parser d String
notStr s = noneOfStr [s]

commentOut :: String->String
commentOut s = concatMap (\s->"//"++s++"\n") (lines s)

parser y = let Parser x = y in x

indentOf :: String -> Int
indentOf (c:cs) = case c of
                     ' ' -> 1+indentOf cs
                     '\t' -> 4+indentOf cs
                     _ -> 0
indentOf [] = 0

indentsOf :: String -> [Int]
indentsOf s = map indentOf (lines s)

getIndent :: Derivs d => String -> Parser d Int
getIndent s = getPosLine >>= \n->return (indentsOf s !! n)

getPosLine :: Derivs d => Parser d Int
getPosLine = getPos >>= \p->return (posLine p-1)
