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
braceSep ss = brace $ ss `sep` ": "

---- printer ----

printType (TypName n) = n
printType (TypTuple t) = parenSep (map printType t)
printType (TypList t) = bracketSep (map printType t)
printType (TypFunction t1 t2) = (printType t1)++"->"++(printType t2)
printType t = show t
printInputType (TypTuple t) = (map printType t) `sep` ", "
printInputType t = printType t

printPattern (PatTuple p) = parenSep (map printPattern p)
printPattern p = show p

printStatement (SttSingle e) = (printExpression e)++";"
printStatement (SttBlock ss) = braceSep (map printStatement ss)

printExpression (ExpNumberLiteral s) = s
printExpression (ExpStringLiteral s) = "\""++s++"\""
printExpression (ExpCharLiteral s) = "'"++s++"'"
printExpression (ExpBinarySymbol s e1 e2) =
    (paren $ printExpression e1) ++ s ++ (paren $ printExpression e2)
printExpression (ExpPrefixUnarySymbol s e) =
    s ++ (paren $ printExpression e)
printExpression (ExpSuffixUnarySymbol s e) =
    (paren $ printExpression e) ++ s
printExpression e = show e

printToken (TokComment s) = s
printToken (TokCppCompilerDirective s) = s
printToken (TokFunctionDeclaration (FDec n (TypFunction t r))) =
    (printType r)`space`n++"("++(printInputType t)++")"++";\n"
printToken (TokFunctionDefinition (FDef n ps s)) =
    n`space`(map printPattern ps`sep`" ")++" =\n"++(printStatement s)
printToken t = show t

printTokens :: [Token]->String
printTokens ts = concatMap printToken ts

compile :: String->String
compile = printTokens.eval
