import Pappy
import Token

---- data ----

cppDefinableSymbols :: [Name]
cppDefinableSymbols = [
    "+","-","*","/","%",
    "++","--",
    "+=","-=","*=","/=","%=",
    "<<",">>",
    "<<=",">>=",
    "&","|","^","~",
    "&=","|=","^=",
    "!","&&","||",
    "==","!=","<",">",">=","<=",
    "&","->","->*"]

cppUndefinableSymbols :: [Name]
cppUndefinableSymbols = [
    "?",":","::",".",".*"]

symbolName :: Symbol -> Name
symbolName s = "operator"++ translate s
    where name c = case c of {
              '!'->"Excl"; '?'->"Quest";
              '<'->"Lt"; '>'->"Gt";
              '/'->"Slash"; '\\'->"Backslash";
              '='->"Equal";
              '+'->"Plus"; '-'->"Minus"; '*'->"Times";
              '@'->"At"; '$'->"Dollar"; '%'->"Percent";
              '&'->"And"; '|'->"Or"; '^'->"Xor"; '~'->"Not";
              '#'->"Hash"; '.'->"Dot"}
          translate s = if elem s cppDefinableSymbols
                        then concatMap name s
                        else s

reservedWords =
  [ "main",
    "if","then","else","case","switch",
    "while","until","for","do",
    "struct","class","data",
    "public","private","protected",
    "this","thisdata",
    "constructor","destructor","operator",
    "bool","char","short","int","long","float","double",
    "typeid","template","typename",
    "const_cast","dynamic_cast","static_cast","reinterpret_cast",
    "new","delete",
    "throw","try","catch"]


---- parsing data ----

data CodeDerivs = CodeDerivs {
        cdvCode::Result CodeDerivs String,
        cdvComment::Result CodeDerivs String,
        cdvChar::Result CodeDerivs Char,
        cdvPos::Pos
    }
instance Derivs CodeDerivs where
    dvChar = cdvChar
    dvPos = cdvPos

parse :: LargeParseFunc CodeDerivs

eval s = case cdvCode (parse (Pos "<input>" 1 1) s) of
              Parsed v _ _ -> v
              NoParse e -> error $ show e
parse pos s = parse' s pos s
parse' a pos s = d where
    d = CodeDerivs (cpCode d) (cpComment d) (autochar (parse' a) pos s) pos

    cpCode = parser
        (do n <- getIndent a
            l <- getPosLine
            notStr "{-"
            x <- (do string "{-"
                     x <- Parser cdvComment
                     y <- Parser cdvCode
                     return $ x++y)
                 </> return ""
            return $ "indent:"++show n++" line:"++show l++"\n"++x)
    
    cpComment = parser
       (do x <- noneOfStr ["-}","{-"]
           y <- (do string "{-"
                    x <- Parser cdvComment
                    y <- Parser cdvComment
                    return $ commentOut x ++ y)
                </> (string "-}">>return "")
           return $ commentOut x ++ y )

main = putStr $ eval "  a{-aba\n   {-foo-}ab-}a{-arb\n f-}a{-a-}q"
