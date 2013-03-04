module HashParse where

import Pappy.Pappy
import Print.Print
import HashToken

---- data ----

infixlSymbols = [
    [","],
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
whiteChar = " \t\n\r"

---- parse data ----

data CodeDerivs = CodeDerivs {
        cdvCode,cdvWhiteStuff :: Result CodeDerivs [Token],
        cdvName,cdvSimpleSafeName :: Result CodeDerivs Name,
        cdvComment,cdvNComment,cdvNumberLiteral,cdvCharLiteral,cdvStringLiteral :: Result CodeDerivs String,
        cdvCppCompilerDirective :: Result CodeDerivs Token,
        cdvType :: Result CodeDerivs Type,
        cdvPattern :: Result CodeDerivs Pattern,
        cdvStatement :: Result CodeDerivs Statement,
        cdvExpression :: Result CodeDerivs Expression,
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
        (cpCode d) (cpWhiteStuff d)
        (cpName d) (cpSimpleSafeName d)
        (cpComment d) (cpNComment d) (cpNumberLiteral d) (cpCharLiteral d) (cpStringLiteral d)
        (cpCppCompilerDirective d)
        (cpType d) (cpPattern d)
        (cpStatement d)
        (cpExpression d)
        (autochar (parse' a) pos s) pos
    
    ---- Basic ----

    cpCode = parser $ concat <$> (many (
                (Parser cdvWhiteStuff) `satisfy` (not.null) </>
                single (Parser cdvCppCompilerDirective) </>
                single (Parser cpFunctionDeclaration) </>
                single (Parser cpFunctionDefinition) </>
                single (Parser cpVariantDeclaration) </>
                single (Parser cpVariantDefinition)
                ))

    ---- White Stuff ----
    
    cpWhiteStuff = parser
        (do concat <$> many(
               (Parser cdvComment >>= \s->return [TokComment s]) </>
               (Parser cdvNComment >>= \s->return [TokComment s]) </>
               (oneOf whiteChar >> return [])))

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

    white = Parser cdvWhiteStuff
    string' s = white>>string s
    char' c = white>>char c

    ---- Name ----
    
    cpRawName = parser $
        do Parser cdvWhiteStuff
           c <- oneOf nameStartChar
           cs <- many $ oneOf nameChar
           return $ (c:cs)
    cpSimpleName = parser $
        Name <$> (optional (char' '@') >> Parser cpRawName)
    cpSimpleSafeName = parser $
        ((Parser cpSimpleName) `satisfy` ((`notElem` reservedWords).name)) </>
        (char' '@' >> Name <$> Parser cpRawName)
    cpName = parser $
        do ns <- concat <$> many ((name<$>Parser cpSimpleSafeName) <++> (string' "::"))
           n <- name <$> Parser cpSimpleName
           return $ Name $ ns++n

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

    cpExpressionMin = parser $
        (ExpName <$> Parser cdvName) </>
        (ExpNumberLiteral <$> Parser cdvNumberLiteral) </>
        (ExpStringLiteral <$> Parser cdvStringLiteral) </>
        (ExpCharLiteral <$> Parser cdvCharLiteral) </>
        (sandwich (char' '(') (char' ')') (Parser cdvExpression))

    ---- Statement ----

    cpStatement = parser $
        (do e <- Parser cdvExpression
            char' ';'
            return $ SttSingle e) </>
        (do char' '{'
            ss <- many $ Parser cpStatement
            char' '}'
            return $ SttBlock ss) </>
        (do string' "return"
            SttReturn <$> Parser cdvExpression) </>
        (do string' "goto"
            SttGoto <$> Parser cdvName) </>
        (string' "continue" >> return SttContinue) </>
        (string' "break" >> return SttBreak)

    ---- Template ----
    
    cpTemplateDefinition = parser $
        (TDType <$> Parser cdvType) </>
        (TDVar <$> Parser cdvType <*> Parser cdvName)
    cpTemplateApplication = parser $
        (TAType <$> Parser cdvType) </>
        (TAExp <$> Parser cdvExpression)
    cpTemplateDefinitionList = parser
        (sandwichSep (string' "#(") (string' ")#") (char' ',')
            (Parser cpTemplateDefinition))
    cpTemplateApplicationList = parser
        (sandwichSep (string' "#(") (string' ")#") (char' ',')
            (Parser cpTemplateApplication))

    ---- Type ----
    
    cpTypeMin = parser $
        (string' "var" >> return TypAuto) </>
        (TypName <$> Parser cdvName) </>
        (sandwich (char' '(') (char' ')') (Parser cdvType)) </>
        (TypTuple <$> sandwichSep (char' '(') (char' ')') (char' ',') (Parser cdvType)) </>
        (TypList <$> sandwich (char' '[') (char' ']') (Parser cdvType))
    cpType = parser $ parseOperators (Parser cpTypeMin)
        [] infixrs prefixs suffixs
        where
            infixrs = [[string' "->">>return TypFunction]]
            prefixs = [[string' "const">>return TypConst]]
            suffixs = [[char' '*'>>return TypPointer],[char' '&'>>return TypReference],[string' "&&">>return TypRvalueReference]]

    ---- Pattern ----
    
    cpPattern = parser $
        (PatName <$> Parser cdvName) </>
        (sandwich (char' '(') (char' ')') (Parser cdvPattern)) </>
        (PatTuple <$> sandwichSep (char' '(') (char' ')') (char' ',') (Parser cdvPattern))

    ---- Function ----
    
    cpArgumentList = parser $
        do sandwichSep (char' '(') (char' ')') (char' ',')
            ((Parser cdvType)<|~|>(optional $ Parser cdvName <|~|> (optional $ char' '=' >> Parser cdvExpression)))
    cpFunctionHeader = parser $
        do {-tl <- return Nothing --optional $ Parser cpTemplateDefinitionList-}
           rt <- Parser cdvType
           n <- Parser cdvSimpleSafeName
           al <- Parser cpArgumentList
           return $ FDec Nothing rt n al
    cpFunctionDeclaration = parser $
        do fh <- Parser cpFunctionHeader
           char' ';'
           return $ TokFDec fh
    cpFunctionDefinition = parser $
        do fh <- Parser cpFunctionHeader
           s <- Parser cdvStatement
           return $ TokFDef $ FDef fh s
        </>
        do fh <- Parser cpFunctionHeader
           char' '='
           e <- Parser cdvExpression
           char' ';'
           return $ TokFDef $ FDef fh $ SttBlock [SttReturn e]

    ---- Variant ----

    cpVariantDeclaration = parser
        (do string' "extern"
            t <- Parser cdvType
            n <- Parser cdvName
            char' ';'
            return $ TokVDec $ VDec t n)
    cpVariantDefinition = parser
        (do t <- Parser cdvType
            n <- Parser cdvName
            e <- optional $ string' "=" >> Parser cdvExpression
            char' ';'
            return $ TokVDef $ VDef t n e)

    ---- Data ----
    
    cpDataDeclaration = 1

    cpDataDefinition = 1
    
    ---- Type Alias ----
    
    cpTypeAlias = 1

    ---- Label ----
    
    cpLabel = 1
