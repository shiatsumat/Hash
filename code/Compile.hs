import System.Environment
import System.Process
import HashPreprocess
import HashPrint
import Control.Applicative

main = do as <- getArgs
          let filename = as!!0
          code <- readFile filename
          let cppcode = compile code
          let cppfilename = filename++".cpp"
          writeFile cppfilename cppcode
          let c = concatMap (" "++) (tail as)
          putStrLn "hash: interpretation succeeded."
          system ("g++ "++cppfilename++c++" -std=c++0x")

