module Pappy.PappyEx where
import Data.Char
import Pappy.Pos
import Pappy.Parse
import Control.Applicative

type LargeParseFunc d = Pos -> String -> d
type ParseFunc d v = d -> Result d v

autochar prs pos s = case s of
                    (c:s') -> Parsed c (prs (nextPos pos c) s') (nullError d)
                    [] -> NoParse (eofError d)
                where d = prs pos s

notChar c = noneOf [c]

noneOfStr :: Derivs d => [String]->Parser d String
noneOfStr ss = Pappy.Parse.many ((noneOf $ map head ss) </> (choice $ map prs ss))
        where prs (c:s) = do{char c; notFollowedBy $ string s; return c}
oneOfStr ss = choice $ fmap string ss
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

infixl 3 <&>,<&&>
(<&>),(<&&>) :: Derivs d => Parser d String -> Parser d String -> Parser d String
p1 <&> p2 = do{s1<-p1;s2<-p2;return $ s1++s2}
p1 <&&> p2 = do{s1<-p1;s2<-p2;return $ s1++"\n"++s2}

optional' :: Derivs d => Parser d [v] -> Parser d [v]
optional' p = Pappy.Parse.optional p >>= \x->return $ case x of{Just s->s;Nothing->[]}

simply :: Derivs d => Parser d String -> Parser d String
simply p = p >> return ""

many1', many' :: Derivs d => Parser d String -> Parser d [String]
many1' p = (do { v <- p; vs <- many1' p; return $ v:"\n":vs } )
many' p = (do { v <- p; vs <- many' p; return $ v:"\n":vs } ) </> return []

single :: Derivs d => Parser d t -> Parser d [t]
single p = p >>= (\x->return [x])

instance (Derivs d ) => Functor (Parser d) where
    fmap f p = p>>=(\x->return $ f x)
