module HashParse where

import Pappy.Pappy
import Print.Print
import HashToken

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

infixlSymbols = [
    ["+","-"],
    ["*","/","%"]]
infixrSymbols = [
    [],
    ["^"]]

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
        cdvExpression,cdvTerm0,cdvTerm1,cdvTerm2,cdvTerm3,cdvTerm4,cdvTerm5, cdvTerm6,cdvTerm7,cdvTerm8,cdvTerm9,cdvTerm10,cdvTerm11,cdvTerm12,cdvTerm13,cdvTerm14,cdvTerm15,cdvTerm16,cdvTerm17,cdvTerm18,cdvTerm19,cdvTerm20,cdvTerm21 :: Result CodeDerivs Expression,
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
cdvTerm 21 = cdvTerm21

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
        (cpExpression d) (cpTerm 0 d) (cpTerm 1 d) (cpTerm 2 d) (cpTerm 3 d) (cpTerm 4 d) (cpTerm 5 d) (cpTerm 6 d) (cpTerm 7 d) (cpTerm 8 d) (cpTerm 9 d) (cpTerm 10 d) (cpTerm 11 d) (cpTerm 12 d) (cpTerm 13 d) (cpTerm 14 d) (cpTerm 15 d) (cpTerm 16 d) (cpTerm 17 d) (cpTerm 18 d) (cpTerm 19 d) (cpTerm 20 d) (cpTerm 21 d)
        (autochar (parse' a) pos s) pos
    
    ---- Basic ----

    cpCode = parser
        (do n <- getIndent a
            l <- getPosLine
            s <- concat<$>many (
                    (single $ Parser cdvCppCompilerDirective) </>
                    ((Parser cdvWhiteStuff) `satisfy` (not.null)) </>
                    (single $ TokFunctionDeclaration <$> Parser cpFunctionDeclaration) </>
                    (anyChar >> return []))
            return s)

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
                (many' $ (Parser cdvNComment <&> noneOfStr ["-}","{-"]))
            string "-}"
            return $ commentOut (x++y) )

    ---- Name ----

    cpName = parser
        (do Parser cdvWhiteStuff
            c <- oneOf nameStartChar
            cs <- many $ oneOf nameChar
            return $ c:cs)
    
    cpDefinableName = parser
        ((Parser cdvName) `satisfy` (`notElem` reservedWords))

    ---- Symbol ----

    cpSymbol s = parser
        (do Parser cdvWhiteStuff
            string s
            return s)
    cpKeyword = cpSymbol

    ---- Literal ----

    cpNumberLiteral = parser
        (do many1 $ oneOf digitChar)

    cpCharLiteral = parser
        (do char '\''
            s <- do{x <- char '\\';y <- anyChar;return [x,y]}
             </> do{c<-anyChar;return [c]}
            char '\''
            return s)
    
    cpStringLiteral = parser
        (do char '"'
            s <- concat <$>
                 many (do{x <- char '\\';y <- anyChar;return [x,y]}
                 </> do{c<-anyChar;return [c]})
            char '"'
            return s)

    ---- Compiler Directive ----

    cpCppCompilerDirective = parser
        (do char '#'
            x <- noneOfStr ["\n","\\\n"]
            y <- concat<$>many(
                (string "\\\n")>>
                ('\n':)<$>(noneOfStr ["\n","\\\n"]))
            string "\n"
            return $ TokCppCompilerDirective("#"++x++y++"\n"))

    ---- Expression ----
    
    cpExpression = parser
        (Parser cdvTerm0)

    cpTerm :: Int -> ParseFunc CodeDerivs Expression
    cpTerm 21 = parser
        (do s <- Parser cdvNumberLiteral
            return (ExpLiteral s))
    cpTerm n = parser
        (do (Parser $ cdvTerm $ n+1) `chainl1`
                (do s <- Parser $ cpSymbol (infixlSymbols!!0!!0)
                    return (\x y->ExpApplication (ExpName $ symbolName s) x y)))

    ---- Statement ----

    cpStatement = parser (
        (do e <- Parser cdvExpression
            Parser $ cpSymbol ";"
            return $ SttSingle e) </>
        (do Parser $ cpSymbol "{"
            ss <- many $ Parser cpStatement
            Parser $ cpSymbol "}"
            return $ SttBlock ss))

    ---- Type ----
    
    cpTypeTerm = parser (
        (do TypName <$> Parser cdvName) </>
        (do Parser $ cpSymbol "("
            t <- Parser cpType
            Parser $ cpSymbol ")"
            return t) </>
        (do Parser $ cpSymbol "("
            ts <- (Parser cpType) `sepBy` (Parser $ cpSymbol ",")
            Parser $ cpSymbol ")"
            return $ TypTuple ts) </>
        (do Parser $ cpSymbol "["
            ts <- (Parser cpType) `sepBy` (Parser $ cpSymbol ",")
            Parser $ cpSymbol "]"
            return $ TypList ts))

    cpType = parser
        (do (Parser cpTypeTerm) `chainr1`
                (do Parser $ cpSymbol "->"
                    return (\x y->TypFunction x y)))

    ---- Template ----

    cpTemplateList = parser
        (do string "!("
            ts<-(Parser cdvType)`sepBy1`(char '\'')
            string ")"
            return ts)

    ---- Pattern ----

    cpPattern = parser
        (do return $ PatName "")

    ---- Function ----

    cpFunctionDeclaration = parser
        (do Parser $ cpKeyword "function"
            n <- Parser cdvName
            Parser $ cpSymbol "::"
            t <- Parser cdvType
            return $ FDec n t)

    cpFunctionDefinition = parser
        (do n <- Parser cdvName
            p <- Parser cdvPattern
            return [])

    ---- Data ----
    
    cpDataDeclaration = 1

    cpDataDefinition = 1
    
    ---- Type Alias ----
    
    cpTypeAlias = 1


