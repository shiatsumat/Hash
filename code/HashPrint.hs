module HashPrint where

import Print.Print
import HashToken
import HashParse

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

---- convenience ----

indent a = indentN 4 a
paren s = "("++s++")"
parenSep ss = paren $ ss `sep` ", "
bracket s = "["++s++"]"
bracketSep ss = bracket $ ss `sep` ", "
brace s = "{"++s++"}"
braceSep ss = brace $ ss `sep` "\n"
angle s = "<"++s++">"
angleSep ss = angle $ ss `sep` ", "

---- printer ----

instance Show TemplateApplication where
    show (TAType t) = show t
    show (TAExp e) = show e

instance Show Type where
    show (TypName n) = n
    show (TypApplication t as) = show t ++ angleSep $ map show as
    show (TypConst t) = "const" ++ show t
    show (TypPointer t) = show t ++ "*"
    show (TypReference t) = show t ++ "&"
    show (TypRvalueReference t) = show t ++ "&&"
    show (TypFunction t1 t2) = show t1 ++ "->" ++ show t2
    show (TypArray t (-1)) = show t ++ "[]"
    show (TypArray t n) = show t ++ "[" ++ show n ++ "]"
    show (TypTuple ts) = "Hash::Tuple" ++ angleSep $ map show ts
    show (TypList t) = "Hash::List" ++ angle $ show t

instance Show Statement where
    show (SttSingle e) = (show e)++";"
    show (SttBlock ss) = braceSep $ map show ss
    show (SttControlFlow "until" e s) = "while" ++ paren ("!" ++ paren $ show e) ++ show s
    show (SttControlFlow "dowhile" e s) = "do" ++ show s ++ "while" ++ paren $ show e ++ ";"
    show (SttControlFlow "dountil" e s) = "do" ++ show s ++ "while" ++ paren ("!" ++ paren $ show e) ++ ";"
    show (SttControlFlow n e s) = n ++ paren $ show e ++ show s
    show (SttFor e1 e2 e3 s) = "for" ++ parenSep [show e1,show e2,show e3] ++ show s
    show (SttReturn e) = "return" ++ show e ++ ";"
    show (SttGoto n) = "goto" ++ n ++ ";"
    show SttContinue = "continue;"
    show SttBreak = "break;"
    show (SttIf e s1 s2) = "if" ++ paren $ show e ++ show s1 ++ show s2

instance Show Expression where
    show (ExpNumberLiteral s) = s
    show (ExpStringLiteral s) = "\""++s++"\""
    show (ExpCharLiteral s) = "'"++s++"'"
    show (ExpBinarySymbol s e1 e2)
        | s=="."||s=="->" = (paren $ show e1) ++ s ++ (show e2)
        | otherwise = (paren $ show e1) ++ s ++ (paren $ show e2)
    show (ExpSuffixUnarySymbol s e) = (paren $ show e) ++ s

showArgumentList as = map showArgument 
    where

instance Show Token where
    show (TokComment s) = s
    show (TokCppCompilerDirective s) = s
    show (TokFDec (FDec n (TypFunction t r))) =
        (show r)`space`n++"("++(printInputType t)++")"++";\n"
    show (TokFDef (FDef n ps s)) =
        n`space`(map printPattern ps`sep`" ")++" =\n"++(show s)

compile :: String->String
compile = concatMap show eval
