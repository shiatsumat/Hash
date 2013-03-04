module HashPrint where

import Print.Print
import HashToken
import HashParse

---- data ----

cppDefinableSymbols :: [String]
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

cppUndefinableSymbols :: [String]
cppUndefinableSymbols = [
    "?",":","::",".",".*"]

symbolName :: Symbol -> String
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
justsep ss = ss `sep` ", "
paren s = "("++s++")"
parenSep ss = paren $ ss `sep` ", "
bracket s = "["++s++"]"
bracketSep ss = bracket $ ss `sep` ", "
brace s = "{"`line`indent(s)++"}"
braceSep ss = brace $ ss `sep` "\n"
angle s = "<"++s++">"
angleSep ss = angle $ ss `sep` ", "

---- printer ----

class Interpret a where
    interpret :: a -> String

instance Interpret Int where
    interpret = show

instance Interpret Name where
    interpret (Name n) = n

instance Interpret TemplateApplication where
    interpret (TAType t) = interpret t
    interpret (TAExp e) = interpret e

instance Interpret Type where
    interpret (TypName n) = interpret n
    interpret (TypApplication t as) = interpret t ++ angleSep (map interpret as)
    interpret (TypConst t) = "const"`space`interpret t
    interpret (TypPointer t) = interpret t ++ "*"
    interpret (TypReference t) = interpret t ++ "&"
    interpret (TypRvalueReference t) = interpret t ++ "&&"
    interpret (TypFunction (TypTuple ts) t2) = "Hash::function"++angleSep[justsep $ map interpret ts,interpret t2]
    interpret (TypFunction t1 t2) = "Hash::function"++angleSep [interpret t1,interpret t2]
    interpret (TypArray t (-1)) = interpret t ++ "[]"
    interpret (TypArray t n) = interpret t ++ "[" ++ interpret n ++ "]"
    interpret (TypTuple ts) = "Hash::Tuple" ++ angleSep (map interpret ts)
    interpret (TypList t) = "Hash::List" ++ angle (interpret t)

instance Interpret Statement where
    interpret (SttSingle e) = (interpret e)++";"
    interpret (SttBlock ss) = braceSep $ map interpret ss
    {-interpret (SttControlFlow "until" e s) = "while" ++ paren ("!" ++ paren (interpret e)) ++ interpret s
    interpret (SttControlFlow "dowhile" e s) = "do" ++ interpret s ++ "while" ++ paren $ interpret e ++ ";"
    interpret (SttControlFlow "dountil" e s) = "do" ++ interpret s ++ "while" ++ paren ("!" ++ paren $ interpret e) ++ ";"-}
    interpret (SttControlFlow n e s) = interpret n ++ paren (interpret e) ++ interpret s
    interpret (SttFor e1 e2 e3 s) = "for" ++ parenSep [interpret e1,interpret e2,interpret e3] ++ interpret s
    interpret (SttReturn e) = "return"`space`interpret e ++ ";"
    interpret (SttGoto n) = "goto"`space`interpret n ++ ";"
    interpret SttContinue = "continue;"
    interpret SttBreak = "break;"
    interpret (SttIf e s1 s2) = "if" ++ paren (interpret e) ++ interpret s1 ++ interpret s2

instance Interpret Expression where
    interpret (ExpName n) = interpret n
    interpret (ExpNumberLiteral s) = s
    interpret (ExpStringLiteral s) = "\""++s++"\""
    interpret (ExpCharLiteral s) = "'"++s++"'"
    interpret (ExpBinarySymbol s e1 e2)
        | s=="."||s=="->" = (paren $ interpret e1) ++ s ++ (interpret e2)
        | otherwise = (paren $ interpret e1) ++ s ++ (paren $ interpret e2)
    interpret (ExpSuffixUnarySymbol s e) = (paren $ interpret e) ++ s

instance Interpret FunctionDeclaration where
    interpret (FDec _ rt n al) = interpret rt`space`interpret n++parenSep(map arg al)
        where arg (t,Nothing) = interpret t
              arg (t,Just(n,Nothing)) = interpret t++" "++interpret n
              arg (t,Just(n,Just e)) = interpret t++" "++interpret n++" = "++interpret e

instance Interpret VariantDeclaration where
    interpret (VDec t n) = "extern "++interpret t`space`interpret n++";\n"
instance Interpret VariantDefinition where
    interpret (VDef t n Nothing) = interpret t`space`interpret n++";\n"
    interpret (VDef t n (Just e)) = interpret t`space`interpret n++" = "++interpret e++";\n"

instance Interpret Token where
    interpret (TokComment s) = s
    interpret (TokCppCompilerDirective s) = s
    interpret (TokFDec fd) = interpret fd ++ ";\n"
    interpret (TokFDef (FDef fd s)) = interpret fd ++ interpret s ++ "\n"
    interpret (TokVDec vd) = interpret vd
    interpret (TokVDef vd) = interpret vd
    interpret (Tok s) = s

compile :: String->String
compile = concatMap interpret . eval
