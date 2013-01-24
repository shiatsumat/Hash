import HashParse
import Control.Applicative

main = eval <$> (readFile "input.txt") >>= \s->writeFile "output.txt" s
