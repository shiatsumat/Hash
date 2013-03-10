module HashPrint where

import Print.Print
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

instance Interpret Name where
    interpret (Name ns) = (map fix ns)`sep`"::"
        where fix s | s`elem`((cppReservedWords\\cppReservedTypes)\\cppReservedValues)= s++"NotReservedWord"
              fix s = s
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
    interpret (TypPointer t) = interpret t ++ "*"
    interpret (TypReference t) = interpret t ++ "&"
    interpret (TypRvalueReference t) = interpret t ++ "&&"
    interpret (TypFunction (TypTuple ts) t2) = "Hash::Function"++angleSep[justSep $ map interpret ts,interpret t2]
    interpret (TypFunction t1 t2) = "Hash::Function"++angleSep [interpret t1,interpret t2]
    interpret (TypArray t (-1)) = interpret t ++ "[]"
    interpret (TypArray t n) = interpret t ++ "[" ++ interpret n ++ "]"
    interpret (TypTuple ts) = "Hash::Tuple" ++ angleSep (map interpret ts)
    interpret (TypList t) = "Hash::List" ++ angle (interpret t)

instance Interpret Expression where
    interpret (ExpName n) = interpret n
    interpret (ExpNumberLiteral s) = s
    interpret (ExpStringLiteral s) = "\""++s++"\""
    interpret (ExpCharLiteral s) = "'"++s++"'"
    interpret (ExpApplication e a) = interpret e ++ parenSep (map interpret a)
    interpret (ExpTuple l) = "Hash::makeTuple" ++ parenSep (map interpret l)
    interpret ExpUnit = "Hash::unit"
    interpret (ExpList l) = "Hash::makeList" ++ parenSep (map interpret l)
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
    interpret (ExpStatement s) = "[&]()->"++interpretblock s++"()"

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
    interpret (SttWhite [] SttNothing) = ""
    interpret (SttWhite w SttNothing) = concatMap interpret w
    interpret (SttWhite [] s) = interpret s
    interpret (SttWhite w s) = concatMap interpret w ++ interpret s
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
    interpret (VDec t n) = "extern "++interpret t`space`interpret n
instance Interpret VariantDefinition where
    interpret (VDef t ns) = interpret (fst(typesuffix t))`space`justSep (map variant ns)
        where variant (n,Just e) = snd(typesuffix t)++interpret n++" = "++interpret e
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
instance Interpret DataHeader where
    interpret (DH (Just l) t n i w) = interpret l`line`interpret t`space`interpret n++superclass i
        where superclass [] = ""
              superclass x = ": "++justSep(map (\(a,t)->interpret a`space`interpret t) x)
    interpret (DH Nothing t n i w) = interpret t`space`interpret n++superclass i
        where superclass [] = ""
              superclass x = ": "++justSep(map (\(a,t)->interpret a`space`interpret t) x)
instance Interpret DataWhere where
    interpret (DWSame t1 t2) = "static_assert(Hash::is_same"++
        angleSep [interpret t1,interpret t2]++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWBase t1 t2) = "static_assert(Hash::is_base_of"++
        angleSep [interpret t1,interpret t2]++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWTypeIs t s) = "static_assert(Hash::is_"++s++
        angle (interpret t)++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWTypeHas t s) = "static_assert(Hash::has_"++s++
        angle (interpret t)++"::value,\"DOESN'T MATCH TYPE CONDITION\");\n"
    interpret (DWExpression e) = "static_assert("++interpret e++",\"DOESN'T MATCH STATIC CONDITION\");\n"

instance Interpret DataDefinition where
    interpret (DDef h@(DH _ t n _ w) m) = interpret h ++ brace(thistype++concat(
        map interpret w ++
        map (\(a,t)->modify a++member t) m)) ++ ";\n"
        where modify Default | t==Struct = "public: "
                             | t==Class = "private: "
              modify a = interpret a++": "
              member (TokFDef (FDefC (FDec a b x c) d e))
                | x==ThisType = interpret (TokFDef (FDefC (FDec a b n c) d e))
              member (TokFDef (FDef (FDec a b x c) d))
                | x==TildaThisType = interpret (TokFDef (FDef (FDec a b (destructor n) c) d))
              member (TokFDec (FDec a b x c))
                | x==ThisType = interpret (TokFDec (FDec a b n c))
              member (TokFDec (FDec a b x c))
                | x==TildaThisType = interpret (TokFDec (FDec a b (destructor n) c))
              member x = interpret x
              destructor (Name [n]) = Name ["~"++n]
              thistype = "typedef "++interpret n++" thistype;\n"
instance Declare DataDefinition where
    declare (DDef (DH (Just l) t n _ _) _) = interpret l`line`interpret t`space`interpret n++";\n"
    declare (DDef (DH Nothing t n _ _) _) = interpret t`space`interpret n++";\n"

instance Interpret EnumData where
    interpret (ED False s n t m) = "enum "++(if s then"class "else"")++interpret n++basetype t++brace(justSep $ map member m)++";\n"
        where member (n,Nothing) = interpret n
              member (n,Just e) = interpret n++" = "++interpret e
              basetype Nothing = ""
              basetype (Just t) = " :"++interpret t++" "
    interpret (ED True s n t m) = "enum "++(if s then"class "else"")++interpret n++basetype t++brace(justSep $ map member [0..(length m-1)])++";\n"
        where member x = interpret n++" = "++show(exp x)
                where (n,Nothing) = m!!x
                      exp 0 = 1
                      exp n = 2 * exp(n-1)
              basetype Nothing = ""
              basetype (Just t) = " :"++interpret t++" "

instance Interpret Token where
    interpret (TokComment s) = s
    interpret (TokCppCompilerDirective s) = s
    interpret (TokVDec v) = interpret v ++ ";\n"
    interpret (TokVDef v) = interpret v ++ ";\n"
    interpret (TokFDec f) = interpret f ++ ";\n"
    interpret (TokFDef f) = interpret f
    interpret (TokDDef d) = interpret d
    interpret (TokED d) = interpret d
    interpret (TokLabel n) = interpret n++":\n"
    interpret (TokStatement s) = interpret s++"\n"
    interpret TokEmpty = ";\n"
    interpret (TokError s) = s

instance Declare Token where
    declare (TokFDef x) = declare x
    declare (TokDDef x) = declare x
    declare _ = ""

instance Interpret Tokens where
    interpret (Tokens ts) =  concatMap interpret (filter first ts) ++ decl ++ concatMap interpret ts'
        where first (TokCppCompilerDirective s) = True
              first _ = False
              ts' = filter (not.first) ts
              decl' = concat $ map declare ts'
              decl = if null decl' then "" else
                        "//////forward declarations begin//////\n"++
                        decl'++
                        "//////forward declarations end//////\n"
compile :: String->String
compile s = "#include \"hash.hpp\"\n"++interpret (eval s)

