import System.Environment
import System.Process
import HashPrint
import Control.Applicative

main = do as <- getArgs
          let filename = head as
          code <- readFile "in.txt"
          let cppcode = compile code
          let cppfilename = filename++".cpp"
          writeFile cppfilename cppcode
          let c = concatMap (++" ") (tail as)
          system ("g++ "++cppfilename++c++" -std=c++0x")

