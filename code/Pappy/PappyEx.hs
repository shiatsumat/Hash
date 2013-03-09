module Pappy.PappyEx where
import Data.Char
import Pappy.Pos
import Pappy.Parse
import Control.Applicative hiding (many)

type LargeParseFunc d = Pos -> String -> d
type ParseFunc d v = d -> Result d v

instance (Derivs d) => Functor (Parser d) where
    fmap f p = p>>=(\x->return $ f x)
instance (Derivs d) => Applicative (Parser d) where
    pure = return
    p1 <*> p2 = p1 >>= (\f->f<$>p2)

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

infixl 3 <++>
(<++>) :: Derivs d => Parser d String -> Parser d String -> Parser d String
p1 <++> p2 = do{s1<-p1;s2<-p2;return$s1++s2}

infixl 3 <|~|>
(<|~|>) :: Derivs d => Parser d a -> Parser d b -> Parser d (a,b)
p1 <|~|> p2 = do{x1<-p1;x2<-p2;return(x1,x2)}

check :: Derivs d => Parser d t -> Parser d Bool
check p = (p >> return True) </> (return False)

simply :: Derivs d => Parser d String -> Parser d String
simply p = p >> return ""

many1', many' :: Derivs d => Parser d String -> Parser d [String]
many1' p = do { v <- p; vs <- many1' p; return $ v:"\n":vs }
many' p = do { v <- p; vs <- many' p; return $ v:"\n":vs } </> return []

single :: Derivs d => Parser d t -> Parser d [t]
single p = p >>= (\x->return [x])

sandwich :: Derivs d => Parser d a -> Parser d b -> Parser d t -> Parser d t
sandwich x y p = do {x;r<-p;y;return r}
sandwichSep, sandwichSep1 :: Derivs d => Parser d a -> Parser d b -> Parser d c -> Parser d t -> Parser d [t]
sandwichSep x y s p = do {x;r<-(p `sepBy` s);y;return r}
sandwichSep1 x y s p = do {x;r<-(p `sepBy1` s);y;return r}

parseOperators :: Derivs d => Parser d r -> [[Parser d (r->r->r)]] -> [[Parser d (r->r->r)]] -> [[Parser d (r->r)]] -> [[Parser d (r->r)]] -> Parser d r
parseOperators min [] [] [] [] = min
parseOperators min infixls infixrs prefixs suffixs = Parser pLTerm
    where
        head' [] = []
        head' l = head l
        tail' [] = []
        tail' l = tail l
        next = parseOperators min (tail' infixls) (tail' infixrs) (tail' prefixs) (tail' suffixs)
        pLTerm = parser $ case head' infixls of
            [] -> Parser pRTerm
            l -> (Parser pRTerm) `chainl1` (choice l)
        pRTerm = parser $ case head' infixrs of
            [] -> Parser pPTerm
            l -> (Parser pPTerm) `chainr1` (choice l)
        pPTerm = parser $ case head' prefixs of
            [] -> Parser pSTerm
            l -> do ss <- many (choice l)
                    t <- Parser pSTerm
                    return $ foldr ($) t ss
        pSTerm = parser $ case head' suffixs of
            [] -> next
            l -> do t <- next
                    ss <- many (choice l)
                    return $ foldl (flip ($)) t ss


