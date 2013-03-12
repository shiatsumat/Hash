module HashPrint where

import Print.Print
import Pappy.Pos
import HashToken
import HashParse
import Data.List

---- data ----

cppReservedWords = [
    "alignas","alignof","and","and_eq","asm","auto",
    "bitand","bitor","bool","break",
    "case","catch","char","char16_t","char32_t","class","compl","const","constexpr","const_cast","continue",
    "decltype","default","delete","do","double","dynamic_cast",
    "else","enum","explicit","export","extern",
    "false","float","for","friend",
    "goto",
    "if","inline","int",
    "long",
    "mutable",
    "namespace","new","noexcept","not","not_eq","nullptr",
    "operator","or","or_eq",
    "private","protected","public",
    "register","reinterpret_cast","return",
    "short","signed","sizeof","static","static_assert","static_cast","struct","switch",
    "template","this","thread_local","throw","true","try","typedef","typeid","typename",
    "union","unsigned","using","virtual","void","volatile","wchar_t","while","xor","xor_eq"]

cppReservedTypes = [
    "bool","char","char16_t","char32_t","double","float","int","long","signed","unsigned","short","void"]
cppReservedValues = ["false","true"]

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
justSep ss = ss `sep` ", "
paren s = "("++s++")"
parenSep ss = paren $ ss `sep` ", "
bracket s = "["++s++"]"
bracketSep ss = bracket $ ss `sep` ", "
brace s = "{"`line`indent(s)++"}"
angle s = "<"++s++">"
angleSep ss = angle $ ss `sep` ", "

---- printer ----

class Interpret a where
    interpret :: a -> String

class Declare a where
    declare :: a -> String

interpretanyway (Left a) = interpret a
interpretanyway (Right a) = interpret a

instance Interpret Int where
    interpret = show

fixname s | s`elem`((cppReservedWords\\cppReservedTypes)\\cppReservedValues)= s++"NotReservedWord"
fixname s = s
instance Interpret Name where
    interpret (Name s) = fixname s
    interpret (NameInType ts s) = concatMap ((++"::").interpret) ts ++ fixname s
    interpret ThisType = "thistype"
    interpret TildaThisType = "~thistype"

instance Interpret Type where
    interpret (TypName n) = interpret n
    interpret (TypSigned n) = "signed "++interpret n
    interpret (TypUnsigned n) = "unsigned "++interpret n
    interpret (TypLong n) = "long "++interpret n
    interpret (TypApplication t as) = interpret t ++ interpret as
    interpret (TypConst t) = "const"`space`interpret t
    interpret (TypMutable t) = "mutable"`space`interpret t
    interpret (TypStatic t) = "static"`space`interpret t
    interpret (TypPointer t) = interpret t ++ "*"
    interpret (TypReference t) = interpret t ++ "&"
    interpret (TypRvalueReference t) = interpret t ++ "&&"
    interpret (TypFunction (TypTuple ts) t2) = "hash::function"++angleSep[justSep $ map interpret ts,interpret t2]
    interpret (TypFunction t1 t2) = "hash::function"++angleSep [interpret t1,interpret t2]
    interpret (TypArray t (-1)) = interpret t ++ "[]"
    interpret (TypArray t n) = interpret t ++ "[" ++ interpret n ++ "]"
    interpret (TypTuple ts) = "hash::tuple" ++ angleSep (map interpret ts)
    interpret (TypList t) = "hash::list" ++ angle (interpret t)
    interpret (TypTypename t) = "typename "++interpret t
    interpret (TypTypeType t1 t2) =  interpret t1++"::"++interpret t2
    interpret TypAuto = "auto"
    interpret (TypDecltype e) = "decltype"++paren(interpret e)
    interpret TypNothing = ""

patternif x (PatName n) = "true"
patternif x (PatLiteral l) = paren(x)++"=="++l
patternif x (PatEqual l) = paren(x)++"=="++interpret l
patternif x PatWildcard = "true"
patternif x (PatAs n p) = patternif x p
patternif x (PatTuple ps) = "hash::static_tuple_size("++x++")=="++show(length ps)
    ++concatMap each [0..length ps-1]
    where each n = "&&"++patternif ("hash::get<"++show n++">("++x++")") (ps!!n)
patternif x (PatList ps) = (if only
    then paren(x)++".size()=="++show(length ps) else "true")
    ++concatMap each [0..length ps-1-if only then 0 else 1]
    where each n = "&&"++patternif (paren(x)++".at("++show n++")") (ps!!n)
          only = last ps/=PatWildcardOthers
patternif x (PatDataConstructor n ps) = paren(x)++".is_"++interpret n++"()&&"
    ++patternif (x++"."++interpret n++"()") (PatTuple ps)

patternget x (PatName n) = "auto& "++interpret n++"="++paren x++";\n"
patternget _ (PatLiteral _) = ""
patternget _ (PatEqual _) = ""
patternget _ PatWildcard = ""
patternget x (PatAs n p) = patternget x (PatName n)++patternget x p
patternget x (PatTuple ps) = concatMap each [0..length ps-1]
    where each n = patternget ("hash::get<"++show n++">("++x++")") (ps!!n)
patternget x (PatList ps)
    = concatMap each [0..length ps-1-if only then 0 else 1]
    where each n = patternget (paren(x)++".at("++show n++")") (ps!!n)
          only = last ps/=PatWildcardOthers
patternget x (PatDataConstructor n ps) = patternget (x++"."++interpret n++"()") (PatTuple ps)

instance Interpret Expression where
    interpret (ExpName n) = interpret n
    interpret (ExpNumberLiteral s) = s
    interpret (ExpStringLiteral s) = "\""++s++"\""
    interpret (ExpCharLiteral s) = "'"++s++"'"
    interpret (ExpApplication e a) = interpret e ++ parenSep (map interpret a)
    interpret (ExpTuple l) = "hash::make_tuple" ++ parenSep (map interpret l)
    interpret ExpUnit = "hash::unit"
    interpret (ExpList l) = "hash::make_list" ++ parenSep (map interpret l)
    interpret (ExpBinarySymbol s e1 e2)
        | s=="."||s=="->" = paren(interpret e1) ++ s ++ interpret e2
        | s==">>" = paren(interpret e1) ++ "," ++ paren(interpret e2)
        | s=="|>" = paren(interpret e2) ++ paren(interpret e1)
        | s=="<|" = paren(interpret e1) ++ paren(interpret e2)
        | otherwise = paren(interpret e1) ++ s ++ paren(interpret e2)
    interpret (ExpPrefixUnarySymbol s e) = s ++ paren (interpret e)
    interpret (ExpSuffixUnarySymbol s e) = paren (interpret e) ++ s
    interpret (ExpIf e1 e2 e3) = paren (interpret e1) ++ "?" ++ interpret e2++ ":" ++ paren (interpret e3)
    interpret (ExpLambda (Just t) a s) = "[&]"++interpret a++interpret t++"->"++interpretblock s
    interpret (ExpLambda Nothing a s) = "[&]"++interpret a++interpretblock s
    interpret (ExpStatement s) = "[&]()"++interpretblock s++"()"
    interpret (ExpData n es) = interpret n++"_make"++parenSep(map interpret es)
    interpret (ExpMatch e cs) = concatMap each cs++error
        where x = interpret e
              each (p,e) = "("++patternif x p++")?[&]()"++brace(patternget x p++"return "++interpret e++";")++"():"
              error = "throw \"No Match For Pattern\""

instance Interpret Statement where
    interpret SttEmpty = ";"
    interpret (SttSingle e) = (interpret e)++";"
    interpret (SttBlock ts) = brace (interpret ts)
    interpret (SttControlFlow "while" e s) = "while" ++ paren (interpretanyway e) ++ interpret s
    interpret (SttControlFlow "until" (Left e) s) = "while" ++ paren ("!"++paren (interpret e)) ++ interpret s
    interpret (SttControlFlow "dowhile" e s) = "do" ++ interpret s ++ "while" ++ paren (interpretanyway e) ++ ";"
    interpret (SttControlFlow "dountil" (Left e) s) = "do" ++ interpret s ++ "while" ++ paren ("!"++paren (interpret e)) ++ ";"
    interpret (SttFor x e1 e2 s) = "for" ++ paren(interpretanyway x++";"++interpret e1++";"++interpret e2) ++ interpret s
    interpret (SttReturn ExpNothing) = "return;"
    interpret (SttReturn e) = "return"`space`interpret e ++ ";"
    interpret (SttGoto n) = "goto"`space`interpret n ++ ";"
    interpret SttContinue = "continue;"
    interpret SttBreak = "break;"
    interpret (SttIf e s1 (Just s2)) = "if" ++ paren (interpretanyway e) ++ interpret s1 ++ "else" ++ interpret s2
    interpret (SttIf e s1 Nothing) = "if" ++ paren (interpretanyway e) ++ interpret s1
    interpret (SttMatch e cs) = (map each cs)`sep`" "++error
        where x = interpret e
              each (p,s) = "if("++patternif x p++")"++brace(patternget x p++interpret s)++"else"
              error = brace "throw \"No Match For Pattern\";"

interpretblock :: Statement -> String
interpretblock s@(SttBlock _) = interpret s
interpretblock s = brace $ interpret s

instance Interpret TemplateDefinitionList where
    interpret (TDL l) = "template"++angleSep (map arg l)
        where arg (t,Nothing) = "typename "++interpret t
              arg (t,Just n) = interpret t`space`interpret n
instance Interpret TemplateApplicationList where
    interpret (TAL l) = angleSep (map interpretanyway l)

typesuffix (TypPointer t) = (fst(typesuffix t),snd(typesuffix t)++"*")
typesuffix (TypReference t) = (fst(typesuffix t),snd(typesuffix t)++"&")
typesuffix (TypRvalueReference t) = (fst(typesuffix t),snd(typesuffix t)++"&&")
typesuffix t = (t,"")
instance Interpret VariantDeclaration where
    interpret (VDec t ns) = "extern "++interpret (fst(typesuffix t))`space`justSep (map variant ns)
        where variant n = snd(typesuffix t)++interpret n
instance Interpret VariantDefinition where
    interpret (VDef t ns) = interpret (fst(typesuffix t))`space`justSep (map variant ns)
        where variant (n,Just e) = snd(typesuffix t)++interpret n++" = "++paren (interpret e)
              variant (n,Nothing) = snd(typesuffix t)++interpret n
instance Declare VariantDefinition where
    declare (VDef t ns) = "extern "++interpret (fst(typesuffix t))`space`justSep (map variant ns)
        where variant (n,_) = snd(typesuffix t)++interpret n

instance Interpret WriteModifier where
    interpret Const = "const"
    interpret Mutable = "mutable"
    interpret NoWM = ""

instance Interpret ArgumentList where
    interpret (AList a m) = parenSep (map arg a) ++ interpret m
        where arg (t,Nothing) = interpret t
              arg (t,Just(n,Nothing)) = interpret t++" "++interpret n
              arg (t,Just(n,Just e)) = interpret t++" "++interpret n++" = "++interpret e

instance Interpret FunctionDeclaration where
    interpret (FDec (Just t) TypNothing n a) = interpret t`line`interpret n++interpret a
    interpret (FDec Nothing TypNothing n a) = interpret n++interpret a
    interpret (FDec (Just t) r n a) = interpret t`line`interpret r`space`interpret n++interpret a
    interpret (FDec Nothing r n a) = interpret r`space`interpret n++interpret a
instance Interpret FunctionDefinition where
    interpret (FDef f s) = interpret f ++ interpretblock s ++ "\n"
    interpret (FDefC f [] s) = interpret f ++ interpretblock s ++ "\n"
    interpret (FDefC f i s) = interpret f`line`indent (":"++justSep(map interpret i))++interpretblock s ++ "\n"
instance Declare FunctionDefinition where
    declare (FDef f _) = interpret f++";\n"
    declare (FDefC f _ _) = interpret f++";\n"

instance Interpret DataType where
    interpret Struct = "struct"
    interpret Class = "class"
instance Interpret AccessModifier where
    interpret Public = "public"
    interpret Private = "private"
    interpret Protected = "protected"
    interpret Default = ""
instance Interpret MemberModifier where
    interpret (MM a b) = interpret a++": "++if b then "virtual " else ""
instance Interpret DataHeader where
    interpret (DH (Just l) t n i w) = interpret l`line`interpret t`space`interpret n++superclass i
        where superclass [] = ""
              superclass x = ": "++justSep(map (\(a,t)->interpret a`space`interpret t) x)
    interpret (DH Nothing t n i w) = interpret t`space`interpret n++superclass i
        where superclass [] = ""
              superclass x = ": "++justSep(map (\(a,t)->interpret a`space`interpret t) x)
instance Interpret DataWhere where
    interpret (DWSame t1 t2) = "static_assert(hash::is_same"++
        angleSep [interpret t1,interpret t2]++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWBase t1 t2) = "static_assert(hash::is_base_of"++
        angleSep [interpret t1,interpret t2]++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWTypeIs t s) = "static_assert(hash::is_"++s++
        angle (interpret t)++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWTypeHas t s) = "static_assert(hash::has_"++s++
        angle (interpret t)++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWExpression e) = "static_assert("++interpret e++",\"DOESN'T MATCH STATIC CONDITION\");\n"

instance Interpret DataDefinition where
    interpret (DDef h@(DH _ t n _ w) m) = interpret h ++
        brace(thistype++concatMap interpret w ++ interpret' m)
        ++ ";\n"
        where interpret' (Tokens ts) = interpretnodecl $ Tokens (map modify ts)
              modify (TokMM (MM Default b))
                | t==Struct = TokMM (MM Public b)
                | t==Class = TokMM (MM Private b)
              modify (TokFDef (FDefC (FDec a b x c) d e))
                | x==ThisType = (TokFDef (FDefC (FDec a b n c) d e))
              modify (TokFDef (FDef (FDec a b x c) d))
                | x==TildaThisType = (TokFDef (FDef (FDec a b (destructor n) c) d))
              modify (TokFDec (FDec a b x c))
                | x==ThisType = (TokFDec (FDec a b n c))
              modify (TokFDec (FDec a b x c))
                | x==TildaThisType = (TokFDec (FDec a b (destructor n) c))
              modify x = x
              destructor (Name s) = Name ("~"++s)
              destructor (NameInType ts s) = Name $ interpret (NameInType ts ("~"++s))
              thistype = "private: typedef "++interpret n++" thistype;\n"
instance Declare DataDefinition where
    declare (DDef (DH (Just l) t n _ _) _) = interpret l`line`interpret t`space`interpret n++";\n"
    declare (DDef (DH Nothing t n _ _) _) = interpret t`space`interpret n++";\n"
instance Interpret DataDeclaration where
    interpret (DDec (Just l) t n) = interpret l`line`interpret t`space`interpret n++";\n"
    interpret (DDec Nothing t n) = interpret t`space`interpret n++";\n"

instance Interpret EnumDataDefinition where
    interpret (EDef False s n t m) = "enum "++(if s then"class "else"")++interpret n++basetype t++brace(justSep $ map member m)++";\n"
        where member (n,Nothing) = interpret n
              member (n,Just e) = interpret n++" = "++interpret e
              basetype Nothing = ""
              basetype (Just t) = " :"++interpret t++" "
    interpret (EDef True s n t m) = "enum "++(if s then"class "else"")++interpret n++basetype t++brace(justSep $ map member [0..(length m-1)])++";\n"
        where member x = interpret n++" = "++show(exp x)
                where (n,Nothing) = m!!x
                      exp 0 = 1
                      exp n = 2 * exp(n-1)
              basetype Nothing = ""
              basetype (Just t) = " :"++interpret t++" "
instance Interpret EnumDataDeclaration where
    interpret (EDec s n) = "enum "++(if s then"class "else"")++interpret n++";\n"
        where basetype Nothing = ""
              basetype (Just t) = " :"++interpret t++" "

instance Interpret TypeAlias where
    interpret (TA n t) = "typedef "++interpret t`space`interpret n++";\n"

instance Interpret Error where
    interpret (Error (Pos _ l c) s) = "#error "++show l++":"++show c++": "++s++"\n"

instance Interpret Token where
    interpret (TokComment s) = s
    interpret (TokCppCompilerDirective s) = s
    interpret (TokVDec v) = interpret v ++ ";\n"
    interpret (TokVDef v) = interpret v ++ ";\n"
    interpret (TokFDec f) = interpret f ++ ";\n"
    interpret (TokFDef f) = interpret f
    interpret (TokDDec d) = interpret d
    interpret (TokDDef d) = interpret d
    interpret (TokEDec d) = interpret d
    interpret (TokEDef d) = interpret d
    interpret (TokTA a) = interpret a
    interpret (TokMM a) = interpret a
    interpret (TokLabel n) = interpret n++":\n"
    interpret (TokStatement s) = interpret s++"\n"
    interpret TokEmpty = ";\n"
    interpret (TokError e) = interpret e
    interpret (TokPos (Pos  f l c)) = "#line "++show l++"\n"

instance Declare Token where
    declare (TokFDef x) = declare x
    declare (TokDDef x) = declare x
    declare _ = ""

interpretnodecl (Tokens ts) = concatMap interpret ts
instance Interpret Tokens where
    interpret (Tokens ts) = concatMap interpret (filter first ts) ++ decl ++ concatMap interpret ts'
        where first (TokCppCompilerDirective s) = True
              first _ = False
              ts' = filter (not.first) ts
              decl' = concat $ map declare ts'
              decl = if null decl' then "" else
                        "//////forward declarations begin//////\n"++
                        decl'++
                        "//////forward declarations end//////\n"
compile :: String->String
compile s = "#line 0\n#include \"hash.hpp\"\n"++interpret (eval s)

