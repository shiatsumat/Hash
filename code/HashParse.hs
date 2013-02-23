module HashParse where

import Pappy.Pappy
import Print.Print
import HashToken

---- data ----

infixlSymbols = [
    [],
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
    [],
    [],
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
    ["::"],
    [],
    [],
    [],
    []]

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
    [],
    [],
    [],
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
    [],
    [],
    [],
    [],
    []]

infixrTypeSymbols = [
    ["->"]]

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
        cdvCode :: Result CodeDerivs [Token],
        cdvWhiteStuff :: Result CodeDerivs [Token],
        cdvComment,cdvNComment,cdvName,cpDefinableName,cdvNumberLiteral,cdvCharLiteral,cdvStringLiteral :: Result CodeDerivs String,
        cdvCppCompilerDirective :: Result CodeDerivs Token,
        cdvType :: Result CodeDerivs Type,
        cdvPattern :: Result CodeDerivs Pattern,
        cdvStatement :: Result CodeDerivs Statement,
        cdvExpression,cdvTerm0,cdvTerm1,cdvTerm2,cdvTerm3,cdvTerm4,cdvTerm5, cdvTerm6,cdvTerm7,cdvTerm8,cdvTerm9,cdvTerm10,cdvTerm11,cdvTerm12,cdvTerm13,cdvTerm14,cdvTerm15,cdvTerm16,cdvTerm17,cdvTerm18,cdvTerm19,cdvTerm20 :: Result CodeDerivs Expression,
        cdvChar::Result CodeDerivs Char,
        cdvPos::Pos
    }
cdvTerm 0 = cdvTerm0
cdvTerm 1 = cdvTerm1
cdvTerm 2 = cdvTerm2
cdvTerm 3 = cdvTerm3
cdvTerm 4 = cdvTerm4
cdvTerm 5 = cdvTerm5
cdvTerm 6 = cdvTerm6
cdvTerm 7 = cdvTerm7
cdvTerm 8 = cdvTerm8
cdvTerm 9 = cdvTerm9
cdvTerm 10 = cdvTerm10
cdvTerm 11 = cdvTerm11
cdvTerm 12 = cdvTerm12
cdvTerm 13 = cdvTerm13
cdvTerm 14 = cdvTerm14
cdvTerm 15 = cdvTerm15
cdvTerm 16 = cdvTerm16
cdvTerm 17 = cdvTerm17
cdvTerm 18 = cdvTerm18
cdvTerm 19 = cdvTerm19
cdvTerm 20 = cdvTerm20

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
        (cpWhiteStuff d) (cpComment d) (cpNComment d)
        (cpName d) (cpDefinableName d)
        (cpNumberLiteral d) (cpCharLiteral d) (cpStringLiteral d)
        (cpCppCompilerDirective d)
        (cpType d) (cpPattern d)
        (cpStatement d)
        (cpExpression d) (cpTerm 0 d) (cpTerm 1 d) (cpTerm 2 d) (cpTerm 3 d) (cpTerm 4 d) (cpTerm 5 d) (cpTerm 6 d) (cpTerm 7 d) (cpTerm 8 d) (cpTerm 9 d) (cpTerm 10 d) (cpTerm 11 d) (cpTerm 12 d) (cpTerm 13 d) (cpTerm 14 d) (cpTerm 15 d) (cpTerm 16 d) (cpTerm 17 d) (cpTerm 18 d) (cpTerm 19 d) (cpTerm 20 d)
        (autochar (parse' a) pos s) pos
    
    ---- Basic ----

    cpCode = parser
        (concat<$>many (
                (single $ Parser cdvCppCompilerDirective) </>
                ((Parser cdvWhiteStuff) `satisfy` (not.null)) </>
                (single $ Parser cpVariantDeclaration) </>
                (single $ Parser cpVariantDefinition) </>
                (single $ Parser cpFunctionDeclaration) </>
                (single $ Parser cpFunctionDefinition) </>
                (anyChar >> return [])))

    ---- White Stuff ----
    
    cpWhiteStuff = parser
        (do concat <$> many(
               (Parser cdvComment >>= \s->return [TokComment s]) </>
               (Parser cdvNComment >>= \s->return [TokComment s]) </>
               (oneOf whiteChar >> return [])))

    cpComment = parser
        (do string "//"
            s <- many $ notChar '\n'
            char '\n'
            return $ commentOut (s++"\n"))

    cpNComment = parser
        (do string "{-"
            x <- noneOfStr ["-}","{-"]
            y <- ('\n':) <$> concat <$>
                (many' $ (Parser cdvNComment <++> noneOfStr ["-}","{-"]))
            string "-}"
            return $ commentOut (x++y) )

    white = Parser cdvWhiteStuff
    string' s = white>>string s
    oneOfStr
    char' c = white>>char c

    ---- Name ----

    cpName = parser
        (do Parser cdvWhiteStuff
            c <- oneOf nameStartChar
            cs <- many $ oneOf nameChar
            return $ c:cs)
    
    cpDefinableName = parser
        ((Parser cdvName) `satisfy` (`notElem` reservedWords))

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
    
    cpExpression = parser
        (Parser cdvTerm0)

    cpTerm :: Int -> ParseFunc CodeDerivs Expression
    cpTerm 20 = parser $
        (ExpName <$> Parser cdvName) </>
        (ExpNumberLiteral <$> Parser cdvNumberLiteral) </>
        (ExpStringLiteral <$> Parser cdvStringLiteral) </>
        (ExpCharLiteral <$> Parser cdvCharLiteral) </>
        (sandwich (char' '(') (char' ')') (Parser cdvExpression))

    cpTerm n = parser $ case l of
        [] -> Parser cpRTerm
        _  -> (Parser cpRTerm) `chainl1` (ExpBinarySymbol <$> (choice $ map string' l))
        where
            l = infixlSymbols!!n
            cpRTerm = parser $ case l of
                [] -> Parser cpPTerm
                _  -> (Parser cpPTerm) `chainr1` (ExpBinarySymbol <$> (choice $ map string' l))
                where l = prefixSymbols!!n
            cpPTerm = parser $ case l of
                [] -> Parser cpSTerm
                _  -> do ss <- many (choice $ map string' l)
                         t <- Parser $ cpSTerm
                         return $ foldr ExpPrefixUnarySymbol t ss
                where l = prefixSymbols!!n
            cpSTerm = parser $ case l of
                [] -> p
                _  -> do t <- p
                         ss <- many (choice $ map string' l)
                         return $ foldl (\t s->ExpSuffixUnarySymbol s t) t ss
                where l = suffixSymbols!!n
                      p = Parser $ cdvTerm $ n+1

    ---- Statement ----

    cpStatement = parser $
        (do e <- Parser cdvExpression
            char' ';'
            return $ SttSingle e) </>
        (do char' '{'
            ss <- many $ Parser cpStatement
            char' '}'
            return $ SttBlock ss) </>
        (do 
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
    
    cpTypeTerm = parser $
        (TypName <$> (optional $ Parser cdvName *> string' "::") <*> (Parser cdvName)) </>
        (sandwich (char' '(') (char' ')') (Parser cdvType)) </>
        (TypTuple <$> sandwichSep (char' '(') (char' ')') (char' ',') (Parser cdvType)) </>
        (TypList <$> sandwich (char' '[') (char' ']') (Parser cdvType)) </>
        (TypApplication <$> (Parser cdvType) <*> (many $ Parser cpTemplateApplication))
    cpType = parser $
        (Parser cpTypeTerm) `chainr1` (string' "->" >> return (\x y->TypFunction x y))

    ---- Pattern ----
    
    cpPattern = parser $
        (PatName <$> Parser cdvName) </>
        (sandwich (char' '(') (char' ')') (Parser cdvPattern)) </>
        (PatTuple <$> sandwichSep (char' '(') (char' ')') (char' ',') (Parser cdvPattern))

    ---- Function ----
    
    cpArgumentList = parser
        (do sandwichSep (char' '(') (char' ')') (char' ',')
                ((Parser cdvType)<|~|>(optional $ Parser cdvName <|~|> (optional $ Parser cdvExpression))))
    cpFunctionHeader = parser
        (do tl <- optional $ Parser cpTemplateDefinitionList
            rt <- Parser cdvType
            n <- Parser cdvName
            als <- many1 $ Parser cpArgumentList
            return $ FDec tl rt n als)
    cpFunctionDeclaration = parser
        (do fh <- Parser cpFunctionHeader
            char' ';'
            return $ TokFDec fh)
    cpFunctionDefinition = parser
        (do fh <- Parser cpFunctionHeader
            optional $ char' '='
            s <- Parser cdvStatement
            return $ TokFDef $ FDef fh s)

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
