import HashPrint
import Control.Applicative

main = compile <$> (readFile "in") >>= \s->writeFile "out" s
