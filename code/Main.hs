import HashPrint
import Control.Applicative

main = compile <$> getContents >>= putStr
