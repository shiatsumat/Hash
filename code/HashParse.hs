module HashParse where

import Pappy.Pappy
import Print.Print
import HashToken

---- data ----

infixlSymbols = [
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

infixrSymbols = [
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

prefixSymbols = [
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

suffixSymbols = [
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
        cdvStatement,cdvBlock :: Result CodeDerivs Statement,
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
        (cpStatement d) (cpBlock d)
        (cpExpression d) (cpExpressionTemplate d)
        (cpArgumentList d)
        (cpTemplateDefinitionList d) (cpTemplateApplicationList d)
        (autochar (parse' a) pos s) pos
    
    ---- Basic ----

    cpCode = parser $ Tokens <$> concat <$> (many (
                (Parser cdvWhiteStuff1) </>
                single (Parser cdvCppCompilerDirective) </>
                single (Parser cpVariantDeclarationToken) </>
                single (Parser cpVariantDefinitionToken) </>
                single (Parser cpFunctionDeclarationToken) </>
                single (Parser cpFunctionDefinitionToken) </>
                single (Parser cpDataDefinitionToken) </>
                single (Parser cpEnumDataToken) </>
                single (Parser cpEmptyToken) </>
                single (Parser cpError)
                ))

    ---- Error ----
    
    cpError = parser $
        do p <- getPos
           many1 (anyChar::Parser CodeDerivs Char)
           return $ TokError $ "PARSE ERROR at "++show p++".\n"
    
    ---- Convenience ----
    
    white = Parser cdvWhiteStuff
    string' s | last s `elem` nameChar = Parser cpRawName `satisfy` (==s)
    string' s = white >> string s
    oneOfStr' ss = Parser cpRawName `satisfy` (`elem` ss)
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

    ---- White Stuff ----
    
    cpWhiteStuff1' = parser $
            (Parser cdvComment >>= \s->return [TokComment s]) </>
            (Parser cdvNComment >>= \s->return [TokComment s]) </>
            (oneOf whiteChar >> return [])
    cpLabel = parser $
        do string "label"
           many1 $ Parser cpWhiteStuff1
           c <- oneOf nameStartChar
           cs <- many $ oneOf nameChar
           char ':'
           return $ TokLabel $ Name [c:cs]
    cpWhiteStuff1 = parser $
        concat <$> many1 (Parser cpWhiteStuff1' </> single (Parser cpLabel))

    cpWhiteStuff = parser $
        Parser cpWhiteStuff1 </>
        return []

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

    cpEmptyToken = parser $ char' ';' >> return TokEmpty

    ---- Name ----

    cpRawName = parser $
        do white
           c <- oneOf nameStartChar
           cs <- many $ oneOf nameChar
           return $ (c:cs)
    cpRawName' = parser $
        do white
           char' '~'
           c <- oneOf nameStartChar
           cs <- many $ oneOf nameChar
           return $ ('~':c:cs)
        </> Parser cpRawName
    cpSimpleName = parser $
        Name <$> single (optional (char' '@') >> Parser cpRawName)
    cpSimpleName' = parser $
        Name <$> single (optional (char' '@') >> Parser cpRawName')
    cpName = parser $
        do ns <- many ((dename <$> Parser cdvSimpleName) >> (string' "::"))
           n <- dename <$> Parser cdvSimpleName
           return $ Name $ ns++[n]
            where dename (Name [s]) = s
    cpName' = parser $
        do ns <- many ((dename <$> Parser cdvSimpleName) >> (string' "::"))
           n <- dename <$> Parser cpSimpleName'
           return $ Name $ ns++[n]
            where dename (Name [s]) = s

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

    ---- Compiler Directive ----

    cpCppCompilerDirective = parser
        (do char '#'
            x <- noneOfStr ["\n","\\\n"]
            y <- concat<$>many(
                (string "\\\n")>>
                ("\\\n"++)<$>(noneOfStr ["\n","\\\n"]))
            string "\n"
            return $ TokCppCompilerDirective("#"++x++y++"\n"))

    ---- Expression ----
    
    cpExpression = parser $ parseOperators (Parser cpExpressionMin)
        (map (map (\x->ExpBinarySymbol<$>string' x)) infixlSymbols)
        (map (map (\x->ExpBinarySymbol<$>string' x)) infixrSymbols)
        (map (map (\x->ExpPrefixUnarySymbol<$>string' x)) prefixSymbols)
        (map (map (\x->ExpSuffixUnarySymbol<$>string' x)) suffixSymbols)

    cpExpressionTemplate = parser $ parseOperators (Parser cpExpressionMin)
        (map (map (\x->ExpBinarySymbol<$>string' x)) (good infixlSymbols))
        (map (map (\x->ExpBinarySymbol<$>string' x)) (good infixrSymbols))
        (map (map (\x->ExpPrefixUnarySymbol<$>string' x)) (good prefixSymbols))
        (map (map (\x->ExpSuffixUnarySymbol<$>string' x)) (good suffixSymbols))
        where good = map $ filter $ notElem '>'

    cpLambda = parser $
        do char' '\\'
           a <- Parser cdvArgumentList
           t <- optional $ Parser cdvType <* string' "->"
           s <- Parser cpBlock'
           return $ ExpLambda t a s

    cpExpressionMin = parser $
        do string' "if"
           e1 <- paren $ Parser cdvExpression
           e2 <- Parser cdvExpression
           string' "else"
           e3 <- Parser cdvExpression
           return $ ExpIf e1 e2 e3
        </>
        (ExpApplication <$> (Parser cpExpressionMinMin) <*> (parenSep $ Parser cpExpression)) </>
        (Parser cpLambda) </>
        (Parser cpExpressionMinMin)
        

    cpExpressionMinMin = parser $
        (ExpName <$> Parser cdvName) </>
        (ExpNumberLiteral <$> Parser cdvNumberLiteral) </>
        (ExpStringLiteral <$> Parser cdvStringLiteral) </>
        (ExpCharLiteral <$> Parser cdvCharLiteral) </>
        (paren (Parser cdvExpression)) </>
        (ExpTuple <$> (parenSep $ Parser cpExpression)) </>
        (paren (return ExpUnit)) </>
        (ExpList <$> (bracketSep $ Parser cpExpression)) </>
        (ExpStatement <$> Parser cdvBlock)

    ---- Statement ----

    cpBlock = parser $
        do char' '{'
           ss <- Tokens <$> concat <$> (many $
                    (Parser cdvWhiteStuff1) </>
                    single (Parser cpVariantDeclarationToken) </>
                    single (Parser cpVariantDefinitionToken) </>
                    single (Parser cpFunctionDeclarationToken) </>
                    single (Parser cpFunctionDefinitionToken) </>
                    single (Parser cpStatementToken))
           char' '}'
           return $ SttBlock ss

    cpBlock' = parser $
        do char' '{'
           ss <- Tokens <$> (++[TokStatement $ SttReturn ExpUnit]) <$>
                    concat <$> (many $
                    (Parser cdvWhiteStuff1) </>
                    single (Parser cpVariantDeclarationToken) </>
                    single (Parser cpVariantDefinitionToken) </>
                    single (Parser cpFunctionDeclarationToken) </>
                    single (Parser cpFunctionDefinitionToken) </>
                    single (Parser cpStatementToken))
           char' '}'
           return $ SttBlock ss
        </>
        Parser cdvBlock
    {-
    cpCase = parser $
        do many1 (char' '|' >> Parser cdvExpression)
           string' "->"

    cpSwitch =  parser $
        do string' "switch"
           e <- paren (Parser Expression)
           return SttSwitch e
    -}

    cpStatementNoReturn = parser $
        Parser cdvBlock </>
        sandwich (string' "goto") (char' ';') (SttGoto <$> Parser cdvSimpleName) </>
        (string' "continue" >> char' ';' >> return SttContinue) </>
        (string' "break" >> char' ';' >> return SttBreak) </>
        (char' ';' >> return SttEmpty) </>
        do n <- oneOfStr' ["while","until","dowhile","dountil"]
           x <- paren ((Right <$> Parser cpRawVariantDefinition) </> (Left <$> Parser cdvExpression))
           s <- Parser cpStatement
           return $ SttControlFlow n x s
        </>
        do string' "if"
           x <- paren ((Left <$> Parser cdvExpression) </> (Right <$> Parser cpRawVariantDefinition))
           s1 <- Parser cdvStatement
           s2 <- optional (string' "else" >> Parser cdvStatement)
           return $ SttIf x s1 s2
        </>
        do string' "for"
           char' '('
           x <- (Left <$> Parser cdvExpression) </> (Right <$> Parser cpRawVariantDefinition)
           char' ';'
           e1 <- Parser cdvExpression
           char' ';'
           e2 <- Parser cdvExpression
           char' ')'
           s <- Parser cdvStatement
           return $ SttFor x e1 e2 s
        </>
        do e <- Parser cdvExpression
           char' ';'
           return $ SttSingle e
    cpStatement = parser $
        sandwich (string' "return") (char' ';') (SttReturn <$> Parser cdvExpression) </>
        ((string' "return") >> (char' ';') >> return (SttReturn ExpNothing)) </>
        Parser cpStatementNoReturn
    cpStatementToken = parser $
        TokStatement <$> Parser cdvStatement

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
    
    cpTypeMin = parser $
        (TypApplication <$> Parser cpTypeMinMin <*> Parser cdvTemplateApplicationList) </>
        (string' "var" >> return TypAuto) </>
        (string' "val" >> return (TypConst TypAuto)) </>
        (string' "ref" >> return (TypReference TypAuto)) </>
        (string' "rref" >> return (TypRvalueReference TypAuto)) </>
        (string' "decltype" >> TypDecltype <$> paren (Parser cdvExpression) ) </>
        Parser cpTypeMinMin

    cpTypeMinMin = parser $
        (paren (Parser cdvType)) </>
        (string' "signed" >> TypSigned <$> Parser cdvName) </>
        (string' "unsigned" >> TypUnsigned <$> Parser cdvName) </>
        (string' "long" >> TypLong <$> Parser cdvType) </>
        (TypName <$> Parser cdvName) </>
        (TypTuple <$> parenSep (Parser cdvType)) </>
        (TypList <$> bracket (Parser cdvType))

    cpType = parser $ parseOperators (Parser cpTypeMin)
        [] infixrs prefixs suffixs
        where
            infixrs = [[string' "->">>return TypFunction]]
            prefixs = [[string' "const">>return TypConst,
                        string' "mutable">>return TypMutable]]
            suffixs = [[char' '!'>>return TypConst,char' '*'>>return TypPointer,string' "&&">>return TypRvalueReference,char' '&'>>return TypReference]]

    ---- Variant ----

    cpVariantDeclaration = parser
        (do string' "extern"
            t <- Parser cdvType
            n <- Parser cdvName
            char' ';'
            return $ VDec t n)
    cpVariantDeclarationToken = parser $ TokVDec <$> Parser cpVariantDeclaration
    cpRawVariantDefinition = parser
        (do t <- Parser cdvType
            ns <- justSep $ Parser cdvName <|~|> optional (string' "=" >> Parser cdvExpression)
            return $ VDef t ns)
    cpVariantDefinition = parser $ Parser cpRawVariantDefinition <*  char' ';'
    cpVariantDefinitionToken = parser $ TokVDef <$> Parser cpVariantDefinition

    ---- Pattern ----
    
    cpPattern = parser $
        paren (Parser cdvPattern) </>
        (char' '_'>>return PatWildcard) </>
        PatDataConstructor <$> Parser cdvName <*> bracketSep (Parser cdvPattern) </>
        PatAs <$> Parser cdvName <*> (string' "as">>Parser cdvPattern) </>
        PatName <$> Parser cdvName </>
        PatTuple <$> parenSep (Parser cdvPattern) </>
        PatList <$> bracketSep (Parser cdvPattern)

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
           m <- many (Parser cpAccessModifier <|~|> (
                Parser cpVariantDefinitionToken </>
                Parser cpFunctionDeclarationToken </>
                Parser cpFunctionDefinitionToken))
           char' '}'
           return $ DDef h m

    cpDataDefinitionToken = parser $ TokDDef <$> Parser cpDataDefinition

    ---- Enum ----

    cpEnumData = parser $
        do string' "enum"
           s <- check (string' "class")
           n <- Parser cdvName
           t <- optional $ (char' ':' >> Parser cdvType)
           m <- braceSep $ (Parser cdvSimpleName <|~|> optional (char' '=' >> Parser cdvExpression))
           return $ ED False s n t m
        </>
        do string' "bitenum"
           s <- check (string' "class")
           n <- Parser cdvName
           t <- optional $ (char' ':' >> Parser cdvType)
           m <- braceSep $ (Parser cdvSimpleName <|~|> return Nothing)
           return $ ED True s n t m

    cpEnumDataToken = parser $ TokED <$> Parser cpEnumData

    ---- Type Alias ----
    
    cpTypeAlias = 1

