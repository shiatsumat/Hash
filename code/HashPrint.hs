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

cppDefinableOperators = [
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

cppUndefinableOperators = [
    "?",":","::",".",".*"]

symbolName :: Operator -> String
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
          translate s = if elem s cppDefinableOperators
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
braceSep ss = brace $ ss `sep` ", "
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
    interpret (TypApplication t as) = interpret t ++ interpret as
    interpret (TypBinaryOperator "->" (TypTuple ts) t2) = "hash::function"++angleSep[justSep $ map interpret ts,interpret t2]
    interpret (TypBinaryOperator "->" t1 t2) = "hash::function"++angleSep [interpret t1,interpret t2]
    interpret (TypBinaryOperator s t1 t2) = "void"
    interpret (TypPrefixOperator s t) = s`space`interpret t
    interpret (TypSuffixOperator "@" t) = "const"`space`interpret t
    interpret (TypSuffixOperator s t) = interpret t++s
    interpret (TypArray t (-1)) = interpret t ++ "[]"
    interpret (TypArray t n) = interpret t ++ "[" ++ interpret n ++ "]"
    interpret (TypTuple ts) = "hash::tuple" ++ angleSep (map interpret ts)
    interpret (TypList t) = "hash::list" ++ angle (interpret t)
    interpret (TypTypeType t1 t2) =  interpret t1++"::"++interpret t2
    interpret TypAuto = "auto"
    interpret (TypDecltype e) = "decltype"++paren(interpret e)
    interpret TypNothing = ""

patternmany each ps = concatMap each [0..length ps-1-if only then 0 else 1]
    where only = last ps/=PatWildcardOthers
patternif x (PatName n) = "true"
patternif x (PatLiteral l) = paren(x)++"=="++l
patternif x (PatEqual l) = paren(x)++"=="++paren(interpret l)
patternif x PatWildcard = "true"
patternif x (PatType t) = paren("nullptr!=dynamic_cast"++angle(paren(interpret t)++"*")++paren(paren(x)++"*"))
patternif x (PatAnd p1 p2) = paren(patternif x p1)++"&&"++paren(patternif x p2)
patternif x (PatOr p1 p2) = paren(patternif x p1)++"||"++paren(patternif x p2)
patternif x (PatAs n p) = patternif x p
patternif x (PatTuple ps) = (if last ps/=PatWildcardOthers then "hash::static_tuple_size("++x++")=="++show(length ps) else "true")
    ++patternmany each ps
    where each n = "&&"++patternif ("hash::get<"++show n++">("++x++")") (ps!!n)
patternif x (PatList ps) = (if last ps/=PatWildcardOthers
    then paren(x)++".size()=="++show(length ps) else "true")
    ++patternmany each ps
    where each n = "&&"++patternif (paren(x)++".at("++show n++")") (ps!!n)
patternif x (PatArray ps) = patternmany each ps
    where each n = "&&"++patternif (paren(x)++"["++show n++"]") (ps!!n)
    
patternif x (PatDataConstructor n ps) = paren(x)++".is_"++interpret n++"()&&"
    ++patternif (x++"."++interpret n++"()") (PatTuple ps)
patternif x (PatRecord ps) = concatMap each ps
    where each (n,p) = patternif (paren(x)++"."++interpret n) p
patternif x (PatWhen p e) = patternif x p++"&&"++paren(interpret e)
patternif x (PatUnless p e) = patternif x p++"&&!"++paren(interpret e)

patternget x (PatName n) = "auto& "++interpret n++"="++paren x++";\n"
patternget _ (PatLiteral _) = ""
patternget _ (PatEqual _) = ""
patternget _ PatWildcard = ""
patternget _ (PatType _) = ""
patternget x (PatAnd p1 p2) = patternget x p1++patternget x p2
patternget x (PatOr p1 p2) = if g1==g2 then g1 else "static_assert(true,\"Or Pattern Is Invalid\");\n"
    where g1 = patternget x p1
          g2 = patternget x p2
patternget x (PatAs n p) = patternget x (PatName n)++patternget x p
patternget x (PatTuple ps) = patternmany each ps
    where each n = patternget ("hash::get<"++show n++">("++x++")") (ps!!n)
patternget x (PatList ps) = patternmany each ps
    where each n = patternget (paren(x)++".at("++show n++")") (ps!!n)
patternget x (PatArray ps) = patternmany each ps
    where each n = patternget (paren(x)++"["++show n++"]") (ps!!n)
patternget x (PatDataConstructor n ps) = patternget (x++"."++interpret n++"()") (PatTuple ps)
patternget x (PatRecord ps) = concatMap each ps
    where each (n,p) = patternget (paren(x)++"."++interpret n) p
patternget x (PatWhen p e) = patternget x p
patternget x (PatUnless p e) = patternget x p

instance Interpret Expression where
    interpret (ExpName n) = interpret n
    interpret (ExpNumberLiteral s) = s
    interpret (ExpStringLiteral s) = "\""++s++"\""
    interpret (ExpCharLiteral s) = "'"++s++"'"
    interpret (ExpApplication e a) = interpret e ++ parenSep (map interpret a)
    interpret (ExpTuple l) = "hash::make_tuple" ++ parenSep (map interpret l)
    interpret ExpUnit = "hash::unit"
    interpret (ExpList l) = "hash::make_list" ++ parenSep (map interpret l)
    interpret (ExpInitializerList l) = braceSep (map interpret l)
    interpret (ExpBinaryOperator s e1 e2)
        | s=="."||s=="->" = paren(interpret e1)++s++interpret e2
        | s=="<*" = "do_before"++parenSep[interpret e1,interpret e2]
        | s=="*>" = paren(paren(interpret e1)++","++paren(interpret e2))
        | s=="|>" = paren(interpret e2)++paren(interpret e1)
        | s=="<|" = paren(interpret e1)++paren(interpret e2)
        | s=="**" = "power"++parenSep[interpret e1,interpret e2]
        | head s=='`'&&last s=='`' = init(tail s)++parenSep[interpret e1,interpret e2]
        | otherwise = paren(interpret e1) ++ s ++ paren(interpret e2)
    interpret (ExpPrefixOperator s e) = s ++ paren (interpret e)
    interpret (ExpSuffixOperator s e) = paren (interpret e) ++ s
    interpret (ExpIf e1 e2 (Just e3)) = paren (interpret e1) ++ "?" ++ interpret e2++ ":" ++ paren (interpret e3)
    interpret (ExpIf e1 e2 Nothing) = paren (interpret e1) ++ "?" ++ interpret e2++ ":" ++ "throw \"No Match For Condition\""
    interpret (ExpLambda (Just t) a s) = "[&]"++interpret a++interpret t++"->"++interpretblock s
    interpret (ExpLambda Nothing a s) = "[&]"++interpret a++interpretblock s
    interpret (ExpStatement s) = "[&]()"++interpretblock s++"()"
    interpret (ExpData n es) = interpret n++"_make"++parenSep(map interpret es)
    interpret (ExpMatch e cs) = concatMap each cs++error
        where x = interpret e
              each (PatOr p1 p2, e) = each(p1,e)++each(p2,e)
              each (p,e) = "("++patternif x p++")?[&]()"++brace(patternget x p++"return "++interpret e++";")++"():"
              error = "throw \"No Match For Pattern\""
    interpret (ExpTypeOperator ":>" e t) = "static_cast"++angle(interpret t)++paren(interpret e)
    interpret (ExpTypeOperator ":?>" e t) = "dynamic_cast"++angle(interpret t)++paren(interpret e)
    interpret (ExpTypeOperator ":#>" e t) = "reinterpret_cast"++angle(interpret t)++paren(interpret e)
    interpret (ExpTypeOperator ":@>" e t) = "const_cast"++angle(interpret t)++paren(interpret e)
    interpret (ExpTypeOperator ":?" e t) = paren("nullptr!=dynamic_cast"++angle(paren(interpret t)++"*")++paren(paren(interpret e)++"*"))

instance Interpret Statement where
    interpret SttEmpty = ";"
    interpret (SttSingle e) = (interpret e)++";"
    interpret (SttBlock ts) = brace (interpret ts)
    interpret (SttControlFlow "while" e s) = "while" ++ paren (interpretanyway e) ++ interpret s
    interpret (SttControlFlow "until" (Left e) s) = "while" ++ paren ("!"++paren (interpret e)) ++ interpretblock s
    interpret (SttControlFlow "dowhile" e s) = "do" ++ interpretblock s ++ "while" ++ paren (interpretanyway e) ++ ";"
    interpret (SttControlFlow "dountil" (Left e) s) = "do" ++ interpretblock s ++ "while" ++ paren ("!"++paren (interpret e)) ++ ";"
    interpret (SttFor x e1 e2 s) = "for" ++ paren(interpretanyway x++";"++interpret e1++";"++interpret e2) ++ interpretblock s
    interpret (SttForeach t n e s) = "for"++paren(interpret t`space`interpret n++" : "++interpret e) ++ interpretblock s
    interpret (SttReturn ExpNothing) = "return;"
    interpret (SttReturn e) = "return"`space`interpret e ++ ";"
    interpret (SttGoto n) = "goto"`space`interpret n ++ ";"
    interpret SttContinue = "continue;"
    interpret SttBreak = "break;"
    interpret (SttIf e s1 (Just s2)) = "if" ++ paren (interpretanyway e) ++ interpretblock s1 ++ "else" ++ interpretblock s2
    interpret (SttIf e s1 Nothing) = "if" ++ paren (interpretanyway e) ++ interpretblock s1
    interpret (SttMatch e cs) = head++(map each cs)`sep`" "++error
        where head = interpret(TokVDef$VDef TypAuto [(Name x,InitExp e)])
              x = "variant_for_match"
              each (PatOr p1 p2, e) = each(p1,e)`space`each(p2,e)
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
    interpret (TAL l) = angleSep (map each l)
        where each (Left t) = interpret t
              each (Right e) = paren(interpret e)

typesuffix (TypSuffixOperator s t) = (fst(typesuffix t),snd(typesuffix t)++s)
typesuffix t = (t,"")
instance Interpret VariantDeclaration where
    interpret (VDec t ns) = "extern "++interpret (fst(typesuffix t))`space`justSep (map variant ns)
        where variant n = snd(typesuffix t)++interpret n
instance Interpret Initialization where
    interpret InitNo = ""
    interpret (InitExp e) = "="++interpret e
    interpret (InitArg es) = parenSep(map interpret es)
    interpret (InitList es) = braceSep(map interpret es)
instance Interpret VariantDefinition where
    interpret (VDef t ns) = interpret (fst(typesuffix t))`space`justSep (map variant ns)
        where variant (n,i) = interpret n++interpret i
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
        where interpret' (Tokens ts) = interpret $ Tokens (map modify ts)
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
    interpret (TokNamespace ns ts) = foldr each (interpret ts) ns
        where each n x = "namespace "++interpret n++"{\n"++x++"}\n"

instance Declare Token where
    declare (TokFDef x) = declare x
    declare (TokDDef x) = declare x
    declare _ = ""

instance Interpret Tokens where
    interpret (Tokens ts) = concatMap interpret ts

compile :: String->String
compile s = "#line 0\n#include \"hash.hpp\"\n"++interpret (eval s)

