import Pappy

---- token data ----
type Name = String
type Literal = String
type Symbol = String

data Type = TypName Name
          | TypConst Type
          | TypPointer Type
          | TypReference Type
          | TypRvalueReference Type
          | TypBinaryOperator Symbol Type Type
          | TypArray Type
          | TypFunction Type Type
          | TypStatic Type
          | TypTemplate Type [Type]
          | TypAuto
    deriving (Eq, Show, Read)
data Template = TempType Type
              | TempTypeFixed Type
              | TempTypeKind Type Int
              | TempTypeFixedKind Type Int
              | TempVar Name Type
              | TempVarFixed Literal
    deriving (Eq, Show, Read)

data Statement = Stt [Expression] deriving (Eq, Show, Read)
data ControlFlow = While | Until | DoWhile deriving (Eq, Show, Read)
data Expression = ExpName Name
                | ExpLiteral Literal
                | ExpSubstitution Name Expression
                | ExpApplication Expression [Expression]
                | ExpIf Expression Expression Expression
                | ExpCase Expression [(Pattern, Expression)]
                | ExpLambda [Pattern] Statement
                | ExpFor Expression Expression Expression Statement
                | ExpControlFlow ControlFlow Expression Statement
                | ExpContinue
                | ExpBreak
                | ExpReturn Expression
                | ExpGoto Name
                | ExpLabel Name
    deriving (Eq, Show, Read)

data Pattern = PatName Name
             | PatDataConstructor Name [Pattern]
             | PatWildcard
             | PatLiteral Literal
             | PatTuple [Pattern]
             | PatList [Pattern]
             | PatAs Name Pattern
    deriving (Eq, Show, Read)

data FunctionDeclaration = FDec Name Type deriving (Eq, Show, Read)
data FunctionDefinition = FDef Name [Pattern] Statement deriving (Eq, Show, Read)

data VariantDeclaration = VDec Name Type deriving (Eq, Show, Read)
data VariantDefinition = VDef Name Expression deriving (Eq, Show, Read)

data DataType = Struct | Class deriving (Eq, Show, Read)
data AccessModifier = Private | Public | Protected deriving (Eq, Show, Read)
data DataHeader = DHdr DataType Name [Template] [(AccessModifier, Type)] deriving (Eq, Show, Read)
data DataDeclaration = DDec
    DataHeader
    [(AccessModifier, VariantDeclaration)]
    [(AccessModifier, FunctionDeclaration)]
    [(AccessModifier, TypeDeclaration)]
    (Maybe AlgebraicData)
    deriving (Eq, Show, Read)
data AlgebraicData = Alg [(Name, [(Type, Name)])] deriving (Eq, Show, Read)

data TypeDeclaration = TDecl Name [Template] Type deriving (Eq, Show, Read)

data Token = TokVariantDeclaration VariantDeclaration
           | TokVariantDefinition VariantDefinition
           | TokFunctionDeclaration FunctionDeclaration
           | TokFunctionDefinition FunctionDefinition
           | TokDataDeclaration DataDeclaration
           | TokTypeDeclaration TypeDeclaration
    deriving (Eq, Show, Read)


---- data ----
symbolName :: Symbol -> Name
symbolName s = "operator"++ (concatMap translate s)
    where translate c = case c of {
              '!'->"Excl"; '?'->"Quest";
              '<'->"Lt"; '>'->"Gt";
              '/'->"Slash"; '\\'->"Backslash";
              '='->"Equal";
              '&'->"And"; '|'->"Or"; '^'->"Xor"; '~'->"Not";
              '+'->"Plus"; '-'->"Minus"; '*'->"Times";
              '@'->"At"; '$'->"Dollar"; '%'->"Percent";
              '#'->"Hash"; '.'->"Dot"}

reservedWords =
  [ "main",
    "if","then","else","case",
    "while","until","for","do",
    "struct","class","data",
    "public","private","protected",
    "this","thisdata",
    "constructor","destructor",
    "bool","char","short","int","long","float","double"]


---- parsing data ----
data CodeDerivs = CodeDerivs {
        cdvCode::Result CodeDerivs String,
        cdvComment::Result CodeDerivs String,
        cdvChar::Result CodeDerivs Char,
        cdvPos::Pos
    }
instance Derivs CodeDerivs where
    dvChar = cdvChar
    dvPos = cdvPos

parse :: LargeParseFunc CodeDerivs
cpCode, cpComment :: ParseFunc CodeDerivs String

eval s = case cdvCode (parse (Pos "<input>" 1 1) s) of
              Parsed v _ _ -> v
              NoParse e -> error $ show e
parse pos s = d where
    d = CodeDerivs (cpCode d) (cpComment d) (autochar parse pos s) pos


---- parsers ----
Parser cpCode =
    (do notStr "{-"
        x <- (do string "{-"
                 x <- Parser cdvComment
                 y <- Parser cdvCode
                 return $ x++y)
          </> return ""
        return x )

Parser cpComment =
    (do x <- noneOfStr ["-}","{-"]
        y <- (do string "{-"
                 x <- Parser cdvComment
                 y <- Parser cdvComment
                 return $ commentOut x ++ y)
          </> (string "-}">>return "")
        return $ commentOut x ++ y )

main = putStr $ eval "1+3*(4+5){-aba\n{-foo-}ab-}a{-ab\na-}a"
