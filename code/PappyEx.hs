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

