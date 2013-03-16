module HashParse where

import Pappy.Pappy
import HashToken

---- data ----

infixlOperators = [
    ["|>","<|","*>","<*"],
    [],
    ["||"],
    ["&&"],
    ["|"],
    ["^"],
    ["&"],
    ["==","!="],
    ["<","<=",">",">="],
    ["<<",">>"],
    
    ["+","-"],
    ["*","/","%"],
    [".*","->*"],
    [],
    [],
    []]

infixrOperators = [
    [],
    ["=","+=","-=","*=","/=","%=","<<=",">>=","&=","^=","|="],
    ["**"],
    [],
    [],
    [],
    [],
    [],
    [],
    [],

    [],
    [],
    [],
    [],
    [".","->"],
    ["::"]]

prefixOperators = [
    ["throw"],
    [],
    [],
    [],
    [],
    [],
    [],
    [],
    [],
    [],
    
    [],
    [],
    [],
    ["++","--","+","-","!","~","*","&","sizeof","new","delete"],
    [],
    []]

suffixOperators = [
    [],
    [],
    [],
    [],
    [],
    [],
    [],
    [],
    [],
    [],
    
    [],
    [],
    [],
    [],
    ["++","--"],
    []]

expressionTypeOperators =
    [":>",":?>",":?",":#>",":@>"]

infixlTypeOperators = [
    [":+:",":*:"]]
infixrTypeOperators = [
    ["->"]]
prefixTypeOperators = [
    ["constexpr","const","typename","static","mutable"]]
suffixTypeOperators = [
    ["*","&&","&","@"]]

controlStructures = ["while","until","dowhile","dountil"]

alphabetChar = ['a'..'z']++['A'..'Z']
digitChar = ['0'..'9']
nameStartChar = alphabetChar++"_"
nameChar = nameStartChar++digitChar++"'"
nameName s = concatMap translate s
    where translate c = case c of {'\''->"Prime"; _ -> [c]}
whiteChar = " \t\n\r"

---- parse data ----

data CodeDerivs = CodeDerivs {
        cdvCode :: Result CodeDerivs Tokens,
        cdvWhiteStuff,cdvWhiteStuff1 :: Result CodeDerivs [Token],
        cdvName, cdvSimpleName, cdvName' :: Result CodeDerivs Name,
        cdvComment,cdvNComment,cdvNumberLiteral,cdvCharLiteral,cdvStringLiteral :: Result CodeDerivs String,
        cdvCppCompilerDirective :: Result CodeDerivs Token,
        cdvType :: Result CodeDerivs Type,
        cdvPattern :: Result CodeDerivs Pattern,
        cdvStatement,cdvBlock,cdvBlock' :: Result CodeDerivs Statement,
        cdvExpression, cdvExpressionTemplate :: Result CodeDerivs Expression,
        cdvArgumentList :: Result CodeDerivs ArgumentList,
        cdvTemplateDefinitionList :: Result CodeDerivs TemplateDefinitionList,
        cdvTemplateApplicationList :: Result CodeDerivs TemplateApplicationList,
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
        (cpCode d) (cpWhiteStuff d) (cpWhiteStuff1 d)
        (cpName d) (cpSimpleName d) (cpName' d)
        (cpComment d) (cpNComment d) (cpNumberLiteral d) (cpCharLiteral d) (cpStringLiteral d)
        (cpCppCompilerDirective d)
        (cpType d)
        (cpPattern d)
        (cpStatement d) (cpBlock d) (cpBlock' d)
        (cpExpression d) (cpExpressionTemplate d)
        (cpArgumentList d)
        (cpTemplateDefinitionList d) (cpTemplateApplicationList d)
        (autochar (parse' a) pos s) pos
    
    ---- Basic ----
    
    cpData = parser $
             Parser cpDataDeclarationToken </>
             Parser cpDataDefinitionToken </>
             Parser cpEnumDataDeclarationToken </>
             Parser cpEnumDataDefinitionToken </>
             Parser cpTypeAliasToken
    cpFunction = parser $
                 Parser cpFunctionDeclarationToken </>
                 Parser cpFunctionDefinitionToken
    cpVariant = parser $
                Parser cpVariantDeclarationToken </>
                Parser cpVariantDefinitionToken
    cpCode = parser $ token (Parser (cpError anyChar)) $ single $
                Parser cpNamespace </>
                Parser cpData </>
                Parser cpFunction </>
                Parser cpVariant </>
                Parser cpEmptyToken
    cpCode' = parser $ token (Parser (cpError (notChar '}'))) $ single $
                Parser cpNamespace </>
                Parser cpData </>
                Parser cpFunction </>
                Parser cpVariant </>
                Parser cpEmptyToken

    cpNamespace = parser $
        do string' "namespace"
           ns <- Parser cdvSimpleName `sepBy` string' "::"
           ts <- brace (Parser cpCode')
           return $ TokNamespace ns ts
        </>
        do string' "namespace"
           ns <- Parser cdvSimpleName `sepBy` string' "::"
           string' "where"
           ts <- Parser cpCode
           return $ TokNamespace ns ts

    ---- Pos ----

    cpPos = parser $ TokPos <$> getPos

    ---- Error ----
    
    cpError x = parser $
        do p <- getPos
           many1 (x::Parser CodeDerivs Char)
           return $ TokError $ Error p "unrecoverable error."
    
    ---- Convenience ----
    
    white = Parser cdvWhiteStuff
    string' s | last s `elem` nameChar = white >> Parser cpRawName `satisfy` (==s)
    string' s = white >> string s
    oneOfStr' ss = white >> Parser cpRawName `satisfy` (`elem` ss)
    char' c = white >> char c
    paren = sandwich (char' '(') (char' ')')
    parenSep = sandwichSep (char' '(') (char' ')') (char' ',')
    justSep = (`sepBy1` (char' ','))
    bracket = sandwich (char' '[') (char' ']')
    bracketSep = sandwichSep (char' '[') (char' ']') (char' ',')
    brace = sandwich (char' '{') (char' '}')
    braceSep = sandwichSep (char' '{') (char' '}') (char' ',')
    angle = sandwich (char' '<') (char' '>')
    angleSep = sandwichSep (char' '<') (char' '>') (char' ',')
    oxford = sandwich (string' "[|") (string' "|]")
    oxfordSep = sandwichSep (string' "[|") (string' "|]") (char' ',')
    token e p = Tokens <$> concat <$> (many (
        (single (Parser cpPos) <++> ((Parser cdvWhiteStuff1 `satisfy` (not.null)) </> p)) </>
        Parser cdvWhiteStuff1 </> (single (Parser cpPos) <++> single e)))
    operatorList m l = map (map (\x->m<$>string' x)) l

    ---- White Stuff ----
    
    cpWhiteStuff1' = parser $
            (Parser cdvComment >>= \s->return [TokComment s]) </>
            (Parser cdvNComment >>= \s->return [TokComment s]) </>
            (oneOf whiteChar >> return [])
    cpComment = parser $
        do string "//"
           s <- many $ notChar '\n'
           char '\n'
           return $ commentOut (s++"\n")
    cpNComment = parser $
        do string "/*"
           x <- noneOfStr ["*/","/*"]
           y <- ('\n':) <$> concat <$>
               (many' $ (Parser cdvNComment <++> noneOfStr ["*/","/*"]))
           string "*/"
           return $ commentOut (x++y)
        </>
        do string "/*/"
           return $ commentOut ""
    cpLabel = parser $
        do string "label"
           many1 $ Parser cpWhiteStuff1
           c <- oneOf nameStartChar
           cs <- many $ oneOf nameChar
           char ':'
           return $ TokLabel $ Name (c:cs)
    cpCppCompilerDirective = parser $
        do char '#'
           x <- noneOfStr ["\n","\\\n"]
           y <- concat<$>many(
               (string "\\\n")>>
               ("\\\n"++)<$>(noneOfStr ["\n","\\\n"]))
           string "\n"
           return $ TokCppCompilerDirective("#"++x++y++"\n")

    cpWhiteStuff1 = parser $
        concat <$> many1 (
            single (Parser cpCppCompilerDirective) </>
            single (Parser cpLabel) </>
            Parser cpWhiteStuff1')
    cpWhiteStuff = parser $
        Parser cpWhiteStuff1 </> return []

    cpEmptyToken = parser $ char' ';' >> return TokEmpty

    ---- Name ----

    cpRawName = parser $
        do c <- oneOf nameStartChar
           cs <- many $ oneOf nameChar
           return $ (c:cs)
    cpRawName' = parser $
        do char' '~'
           c <- oneOf nameStartChar
           cs <- many $ oneOf nameChar
           return $ ('~':c:cs)
        </> Parser cpRawName
    cpSimpleName = parser $
        white >> optional (char' '@') >> Name <$> (Parser cpRawName)
    cpSimpleName' = parser $
        white >> Name <$> (Parser cpRawName')
    dename (Name s) = s
    cpName = parser $
        (white >> (string' "thistype">>return ThisType)) </>
        do x <- (string' "::">>return [TypNothing])</>return []
           ts <- many ((Parser cpTypeSmall) <* (string' "::"))
           n <- dename <$> Parser cdvSimpleName
           return $ NameInType (x++ts) n
    cpName' = parser $
        (white >> (Parser cpRawName'`satisfy`(=="~thistype")>>return TildaThisType)) </>
        do x <- (string' "::">>return [TypNothing])</>return []
           ts <- many ((Parser cpTypeSmall) <* (string' "::"))
           n <- dename <$> Parser cpSimpleName'
           return $ NameInType (x++ts) n

    ---- Literal ----

    cpNumberLiteral = parser
        (white >> (many1 $ oneOf digitChar))

    cpCharLiteral = parser
        (sandwich (char' '\'') (char' '\'') $
            do{x <- char '\\';y <- anyChar;return [x,y]}
            </> do{c<-anyChar;return [c]})
    
    cpStringLiteral = parser
        (sandwich (char' '"') (char' '"') $
            concat <$> many (do{x <- char '\\';y <- anyChar;return [x,y]}
            </> do{c<-notChar '\"';return [c]}))

    ---- Pattern ----

    cpWildcardOthers = parser $ string' "_*" >> return PatWildcardOthers
    cpManyThings = parser $ justSep (Parser cpWildcardOthers</>Parser cdvPattern)
                   `satisfy` (\x->x==[]||PatWildcardOthers`notElem`init x)
    cpPatternMinMin = parser $
        PatTuple <$> (paren(Parser cpManyThings) `satisfy` (\x->length x/=1))</>
        PatType <$> (string' "?>" >> Parser cdvType) </>
        paren (Parser cdvPattern) </>
        (char' '_'>>return PatWildcard) </>
        PatDataConstructor <$> Parser cdvName <*> braceSep (Parser cdvPattern) </>
        PatAs <$> Parser cdvName <*> (string' "@">>Parser cdvPattern) </>
        PatName <$> Parser cdvName </>
        PatLiteral <$> Parser cpNumberLiteral </>
        PatList <$> (bracket(Parser cpManyThings) </> brace(Parser cpManyThings)) </>
        PatArray <$> oxford(Parser cpManyThings) </>
        PatEqual <$> (char' '='>>paren(Parser cdvExpression)) </>
        PatRecord <$> braceSep(Parser cdvName<|~|>Parser cdvPattern)
    cpPatternMin = parser $ parseOperators (Parser cpPatternMinMin)
        [[char' '|'>>return PatOr],[char' '&'>>return PatAnd]] []
        [[char' '!'>>return PatNot]] []
    cpPattern = parser $
        do{p<-Parser cpPatternMin;string' "as";n<-Parser cdvName;return$PatAs n p} </>
        PatWhen <$> Parser cpPatternMin <*> (string' "when">>Parser cdvExpression) </>
        PatUnless <$> Parser cpPatternMin <*> (string' "unless">>Parser cdvExpression) </>
        Parser cpPatternMin

    ---- Expression ----
    
    cpExpression = parser $ parseOperators (Parser cpFunctionOperator)
        (operatorList ExpBinaryOperator infixlOperators)
        (operatorList ExpBinaryOperator infixrOperators)
        (operatorList ExpPrefixOperator prefixOperators)
        (operatorList ExpSuffixOperator suffixOperators)

    cpExpressionTemplate = parser $ parseOperators (Parser cpFunctionOperator)
        (operatorList ExpBinaryOperator (good infixlOperators))
        (operatorList ExpBinaryOperator (good infixrOperators))
        (operatorList ExpPrefixOperator (good prefixOperators))
        (operatorList ExpSuffixOperator (good suffixOperators))
        where good = map $ filter $ notElem '>'

    cpFunctionOperator = parser $
        do let operator = do char' '`'
                             notFollowedBy(Parser cdvWhiteStuff1)
                             n <- Parser cpRawName
                             char' '`'
                             notFollowedBy(Parser cdvWhiteStuff1)
                             return $ ExpBinaryOperator ("`"++n++"`")
           chainl1 (Parser cpExpressionTypeOperator) operator
        </>
        Parser cpExpressionTypeOperator

    cpExpressionTypeOperator = parser $
        do e <- Parser cpExpressionMin
           s <- choice $ map string' [":>",":?>",":?",":#>",":@>"]
           t <- Parser cdvType
           return $ ExpTypeOperator s e t
        </>
        Parser cpExpressionMin

    cpLambda = parser $
        do char' '\\'
           a <- Parser cdvArgumentList
           t <- optional $ Parser cdvType <* string' "->"
           s <- Parser cpBlock'
           return $ ExpLambda t a s

    cpMatch = parser $
        do string' "match"
           e <- Parser cdvExpression
           string' "with"
           cs <- concat <$> many1 element
           return $ ExpStatement $ SttMatch e cs
           where element = do c <- many1 ((string' "case">>Parser cdvPattern)</>(string' "default">>return PatWildcard))
                              s <- string' "->">>SttReturn<$>Parser cdvExpression
                              return $ map (\x->(x,s)) c

    cpExpressionMin = parser $
        do string' "if"
           e1 <- paren $ Parser cdvExpression
           e2 <- Parser cdvExpression
           e3 <- optional (string' "else">>Parser cdvExpression)
           return $ ExpIf e1 e2 e3
        </>
        Parser cpLambda </>
        ExpData <$> (Parser cdvName) <*> (braceSep $ Parser cdvExpression) </>
        Parser cpMatch </>
        ExpApplication <$> (Parser cpExpressionMinMin) <*> (parenSep $ Parser cdvExpression) </>
        Parser cpExpressionMinMin

    cpExpressionMinMin = parser $
        ExpName <$> Parser cdvName </>
        ExpNumberLiteral <$> Parser cdvNumberLiteral </>
        ExpStringLiteral <$> Parser cdvStringLiteral </>
        ExpCharLiteral <$> Parser cdvCharLiteral </>
        paren (Parser cdvExpression) </>
        ExpTuple <$> (parenSep $ Parser cpExpression) </>
        paren (return ExpUnit) </>
        ExpList <$> (bracketSep $ Parser cpExpression) </>
        ExpInitializerList <$> (braceSep $ Parser cpExpression) </>
        ExpStatement <$> Parser cdvBlock'

    ---- Statement ----

    cpBlock = parser $
        SttBlock <$> (brace $ token (Parser (cpError$notChar '}')) $ single $
                    TokStatement <$> Parser cpStatementSpecial </>
                    Parser cdvCppCompilerDirective </>
                    Parser cpData </>
                    Parser cpFunction </>
                    Parser cpVariant </>
                    TokStatement <$> Parser cpStatementSingle)
    cpBlock' = parser $
        do Tokens ss <- brace $ token (Parser (cpError$notChar '}')) $ single $
                    TokStatement <$> Parser cpStatementSpecialNoReturn </>
                    Parser cdvCppCompilerDirective </>
                    Parser cpData </>
                    Parser cpFunction </>
                    Parser cpVariant </>
                    TokStatement <$> Parser cpStatementSingle
           return $ SttBlock $ Tokens $ ss++[TokStatement $ SttReturn ExpUnit]

    cpStatementSpecialNoReturn = parser $
        Parser cdvBlock </>
        sandwich (string' "goto") (char' ';') (SttGoto <$> Parser cdvSimpleName) </>
        (string' "continue" >> char' ';' >> return SttContinue) </>
        (string' "break" >> char' ';' >> return SttBreak) </>
        (char' ';' >> return SttEmpty) </>
        do n <- oneOfStr' controlStructures
           x <- paren ((Right <$> Parser cpRawVariantDefinition) </> (Left <$> Parser cdvExpression))
           s <- Parser cpStatement
           return $ SttControlFlow n x s
        </>
        do string' "if"
           x <- paren ((Right <$> Parser cpRawVariantDefinition) </> (Left <$> Parser cdvExpression))
           s1 <- Parser cdvStatement
           s2 <- optional (string' "else" >> Parser cdvStatement)
           return $ SttIf x s1 s2
        </>
        do string' "for"
           char' '('
           x <- (Right <$> Parser cpRawVariantDefinition) </> (Left <$> Parser cdvExpression)
           char' ';'
           e1 <- Parser cdvExpression
           char' ';'
           e2 <- Parser cdvExpression
           char' ')'
           s <- Parser cdvStatement
           return $ SttFor x e1 e2 s
        </>
        do string' "foreach"
           char' '('
           t <- Parser cdvType
           n <- Parser cdvSimpleName
           string' "in"
           e <- Parser cdvExpression
           char' ')'
           s <- Parser cdvStatement
           return $ SttForeach t n e s
        </>
        do string' "match"
           e <- Parser cdvExpression
           string' "with"
           cs <- concat <$> many1 element
           return $ SttMatch e cs
           where element = do c <- many1 ((string' "case">>Parser cdvPattern)</>(string' "default">>return PatWildcard))
                              s <- string' "->">>Parser cdvStatement
                              return $ map (\x->(x,s)) c
    cpReturn = parser $
        sandwich (string' "return") (char' ';') (SttReturn <$> Parser cdvExpression) </>
        ((string' "return") >> (char' ';') >> return (SttReturn ExpNothing))
    cpStatementSpecial = parser $
        Parser cpStatementSpecialNoReturn </> Parser cpReturn
    cpStatementSingle = parser $
        do e <- Parser cdvExpression
           char' ';'
           return $ SttSingle e
    cpStatement = parser $
        Parser cpStatementSpecial </>
        Parser cpStatementSingle

    ---- Template ----
    
    cpTemplateDefinition = parser $
        (Parser cdvType <|~|> optional (Parser cdvName))
    cpTemplateApplication = parser $
        (Left <$> Parser cdvType) </> (Right <$> Parser cdvExpressionTemplate)
    cpTemplateDefinitionList = parser $
        TDL <$> angleSep (Parser cpTemplateDefinition)
    cpTemplateApplicationList = parser $
        TAL <$> angleSep (Parser cpTemplateApplication)

    ---- Type ----
    
    cpType = parser $
        (string' "::" >> TypTypeType TypNothing <$> Parser cpType) </>
        chainl1 (Parser cpTypeSmall) (string' "::">>return TypTypeType)
    cpTypeSmall = parser $
        parseOperators (Parser cpTypeMin)
            (operatorList TypBinaryOperator infixlTypeOperators)
            (operatorList TypBinaryOperator infixrTypeOperators)
            (operatorList TypPrefixOperator prefixTypeOperators)
            (operatorList TypSuffixOperator suffixTypeOperators)
    cpTypeMin = parser $
        (TypApplication <$> Parser cpTypeMinMin <*> Parser cdvTemplateApplicationList) </>
        (string' "var" >> return TypAuto) </>
        (string' "val" >> return (TypPrefixOperator "const" TypAuto)) </>
        (string' "ref" >> return (TypSuffixOperator "&" TypAuto)) </>
        (string' "rref" >> return (TypSuffixOperator "&&" TypAuto)) </>
        (string' "decltype" >> TypDecltype <$> paren (Parser cdvExpression) ) </>
        Parser cpTypeMinMin
    cpTypeMinMin = parser $
        (paren (Parser cdvType)) </>
        (TypName <$> (Name<$>(oneOfStr' names) </> Parser cdvSimpleName)) </>
        (TypTuple <$> parenSep (Parser cdvType)) </>
        (TypList <$> bracket (Parser cdvType))
        where names = 
                [x++y | x<-["signed ","unsinged "],y<-["char","short","int","long","long long","long long int"]] ++
                ["long double","long long","long long int"]

    ---- Variant ----

    cpVariantDeclaration = parser
        (do string' "extern"
            t <- Parser cdvType
            ns <- justSep $ Parser cdvName
            char' ';'
            return $ VDec t ns)
    cpVariantDeclarationToken = parser $ TokVDec <$> Parser cpVariantDeclaration
    cpRawVariantDefinition = parser
        (do t <- Parser cdvType
            ns <- justSep $ Parser cdvName <|~|> (
                (string' "=" >> InitExp <$> Parser cdvExpression) </>
                InitArg <$> parenSep (Parser cdvExpression) </>
                InitList <$> braceSep (Parser cdvExpression) </>
                return InitNo)
            return $ VDef t ns)
    cpVariantDefinition = parser $ Parser cpRawVariantDefinition <*  char' ';'
    cpVariantDefinitionToken = parser $ TokVDef <$> Parser cpVariantDefinition

    ---- Function ----
    
    cpArgumentList = parser $
        AList <$> parenSep ((Parser cdvType)<|~|>(optional $ Parser cdvName <|~|> (optional $ char' '=' >> Parser cdvExpression))) <*> ((string' "const">>return Const) </> (string' "mutable">>return Mutable) </> (return NoWM))
    
    cpFunctionHeader = parser $
        do r <- Parser cdvType
           n <- Parser cdvName
           t <- optional $ Parser cdvTemplateDefinitionList
           a <- Parser cdvArgumentList
           return $ FDec t r n a
    cpFunctionHeader'' = parser $
        do r <- Parser cdvType
           n <- Parser cdvName
           a <- Parser cdvArgumentList
           return $ FDec Nothing r n a
    cpConstructorHeader = parser $
        do n <- Parser cdvName
           t <- optional $ Parser cdvTemplateDefinitionList
           a <- Parser cdvArgumentList
           return $ FDec t TypNothing n a
    cpDestructorHeader = parser $
        do n <- Parser cdvName'
           a <- paren $ return $ AList [] NoWM
           return $ FDec Nothing TypNothing n a
    cpFunctionDeclaration = parser $ choice [Parser cpFunctionHeader, Parser cpConstructorHeader, Parser cpDestructorHeader] <* char' ';'
    cpFunctionDeclarationToken = parser $ TokFDec <$> Parser cpFunctionDeclaration

    cpFunctionBody = parser $
        Parser cdvBlock </>
        do char' '='
           e <- Parser cdvExpression
           char' ';'
           return $ SttBlock $ Tokens [TokStatement(SttReturn e)]
    cpFunctionDefinition = parser $
        FDef <$> Parser cpFunctionHeader <*> Parser cpFunctionBody
    cpInitializationList = parser $
        do char' ':'
           justSep (Parser cdvExpression)
    cpConstructorDefinition = parser $
        do f <- Parser $ cpConstructorHeader
           i <- Parser cpInitializationList </> return []
           s <- Parser cpFunctionBody
           return $ FDefC f i s
    cpDestructorDefinition = parser $
        do f <- Parser $ cpDestructorHeader
           s <- Parser cpFunctionBody
           return $ FDef f s
    cpFunctionDefinitionToken = parser $ TokFDef <$>
        choice [Parser cpFunctionDefinition, Parser cpConstructorDefinition, Parser cpDestructorDefinition]

    ---- Data ----
    
    cpAccessModifier = parser $
        choice (map (\(x,y)->string' x>>return y) [("private",Private),("public",Public),("protected",Protected)])
        </>
        return Default
    cpMemberModifierToken = parser $
        TokMM <$> (MM <$> Parser cpAccessModifier <*> check (string' "virtual"))

    cpSuperData = parser $
        (char' ':' >> justSep (Parser cpAccessModifier <|~|> Parser cdvType)) </>
        return []

    cpDataWhereElement = parser $
        paren (Parser cpDataWhereElement) </>
        do t <- Parser cdvType
           string' ":is:"
           DWTypeIs t <$> oneOfStr'
            ["abstract","arithmetic","array",
             "class","compound","const","convertible",
             "empty","enum",
             "floating_point","function",
             "member_function_pointer",
             "member_object_pointer",
             "object",
             "pod","pointer","polymorphic",
             "reference",
             "scalar","signed",
             "union","unsigned",
             "void","volatile"]
        </>
        do t <- Parser cdvType
           string' ":has:"
           DWTypeHas t <$> oneOfStr'
            ["nothrow_assign","nothrow_constructor","nothrow_copy",
             "trivial_assign","trivial_constructor","trivial_copy"]
        </>
        do t <- Parser cdvType
           string' ":=:"
           DWSame t <$> Parser cdvType
        </>
        do t <- Parser cdvType
           string' ":>:"
           DWBase t <$> Parser cdvType
        </>
        do t <- Parser cdvType
           string' ":<:"
           DWBase <$> Parser cdvType <*> return t
        </>
        DWExpression <$> Parser cdvExpression

    cpDataWhere = parser $
        ((string' "where") >> (justSep $ Parser cpDataWhereElement))
        </> return []

    cpDataHeader = parser $
        do optional $ (string' "abstract") </> (string' "sealed")
           d <- (string' "struct" >> return Struct) </> (string' "class" >> return Class) </> (string' "interface" >> return Struct)
           n <- Parser cdvName
           t <- optional (Parser cdvTemplateDefinitionList)
           s <- Parser cpSuperData
           w <- Parser cpDataWhere
           return $ DH t d n s w
    
    cpDataDefinition = parser $
        do h <- Parser cpDataHeader
           char' '{'
           m <- token (Parser$cpError$notChar '}') $
                single (Parser cpMemberModifierToken) <++> single (
                Parser cpFunctionDeclarationToken </>
                Parser cpFunctionDefinitionToken </>
                Parser cpVariantDefinitionToken)
                </>
                single (Parser cpEmptyToken)
           char' '}'
           return $ DDef h m

    cpDataDefinitionToken = parser $ TokDDef <$> Parser cpDataDefinition

    cpDataDeclaration = parser $
        do optional $ (string' "abstract") </> (string' "sealed")
           d <- (string' "struct" >> return Struct) </> (string' "class" >> return Class) </> (string' "interface" >> return Struct)
           n <- Parser cdvName
           t <- optional (Parser cdvTemplateDefinitionList)
           char' ';'
           return $ DDec t d n
    cpDataDeclarationToken = parser $ TokDDec <$> Parser cpDataDeclaration

    ---- Enum ----

    cpEnumDataDefinition = parser $
        do string' "enum"
           s <- check (string' "class")
           n <- Parser cdvName
           t <- optional $ (char' ':' >> Parser cdvType)
           m <- braceSep $ (Parser cdvSimpleName <|~|> optional (char' '=' >> Parser cdvExpression))
           return $ EDef False s n t m
        </>
        do string' "bitenum"
           s <- check (string' "class")
           n <- Parser cdvName
           t <- optional $ (char' ':' >> Parser cdvType)
           m <- braceSep $ (Parser cdvSimpleName <|~|> return Nothing)
           return $ EDef True s n t m
    cpEnumDataDefinitionToken = parser $ TokEDef <$> Parser cpEnumDataDefinition

    cpEnumDataDeclaration = parser $
        do string' "enum" </> string' "bitenum"
           s <- check (string' "class")
           n <- Parser cdvName
           char' ';'
           return $ EDec s n
    cpEnumDataDeclarationToken = parser $ TokEDec <$> Parser cpEnumDataDeclaration

    ---- Type Alias ----
    
    cpTypeAlias = parser $
        do string' "type"
           TA <$> (Parser cdvSimpleName) <*> (char' '=' >> Parser cdvType)
    cpTypeAliasToken = parser $
        TokTA <$> Parser cpTypeAlias

