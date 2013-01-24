module HashParse where

import Pappy.Pappy
import Print.Print
import HashToken

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
                        then s
                        else concatMap name s

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

alphabetChar = ['a'..'z']++['A'..'Z']
digitChar = ['0'..'9']
nameStartChar = alphabetChar++"_"
nameChar = nameStartChar++digitChar++"'"
nameName s = concatMap translate s
    where translate c = case c of {'\''->"Prime"; _ -> [c]}

---- parsing data ----

data CodeDerivs = CodeDerivs {
        cdvCode::Result CodeDerivs String,
        cdvName,cdvNumberLiteral,cdvStringLiteral,
        cdvComment,cdvNComment,
        cdvCppCompilerDirective,cdvHashCompilerDirective
            ::Result CodeDerivs String,
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
    d = CodeDerivs
        (cpCode d)
        (cpName d) (cpNumberLiteral d) (cpStringLiteral d)
        (cpComment d)(cpNComment d)
        (cpCppCompilerDirective d) (cpHashCompilerDirective d)
        (autochar (parse' a) pos s) pos

    cpCode = parser
        (do n <- getIndent a
            l <- getPosLine
            s <- (many $ 
                Parser cdvComment </>
                Parser cdvNComment </>
                (anyChar >> return ""))
            return $ "indent:"++show n++" line:"++show l++"\n"++concat s)
    
    cpName = parser
        (do c <- oneOf nameStartChar
            cs <- many $ oneOf nameChar
            return $ c:cs)

    cpNumberLiteral = parser
        (do s <- many1 $ oneOf digitChar
            return s)

    cpStringLiteral = parser
        (do return "")

    ---- Comment ----
    cpComment = parser
        (do string "//"
            s <- many $ notChar '\n'
            char '\n'
            return $ commentOut s)

    cpNComment = parser
        (do string "{-"
            x <- noneOfStr ["-}","{-"]
            y <- ('\n':) <$> concat <$> (many' $ Parser cdvNComment <&> noneOfStr ["-}","{-"])
                <&> (simply $ string "-}")
            return $ commentOut $ x++y)

    ---- Compiler Directive ----
    cpCppCompilerDirective = parser (do return "")

    cpHashCompilerDirective = parser (do return "")

