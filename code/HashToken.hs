module HashToken where

type Namespace = String
type Name = String
type Literal = String
type Symbol = String

data TemplateDefinition = TDType Type
                        | TDVar Type Name
    deriving (Eq, Show, Read)
type TemplateDefinitionList = [TemplateDefinition]
data TemplateApplication = TAType Type
                        | TAExp Expression
    deriving (Eq, Show, Read)
type TemplateApplicationList = [TemplateApplication]

data Type = TypName (Maybe Namespace) Name
          | TypApplication Type TemplateApplicationList
          | TypConst Type
          | TypPointer Type
          | TypReference Type
          | TypRvalueReference Type
          | TypFunction Type Type
          | TypArray Type Int
          | TypTuple [Type]
          | TypList Type
          | TypAuto
    deriving (Eq, Show, Read)

data Statement = SttSingle Expression
               | SttBlock [Statement]
               | SttControlFlow Name Expression Statement
               | SttFor Expression Expression Expression Statement
               | SttReturn Expression
               | SttGoto Name
               | SttContinue
               | SttBreak
               | SttIf Expression Statement Statement
    deriving (Eq, Show, Read)
data Expression = ExpName Name
                | ExpSymbol Symbol
                | ExpNumberLiteral Literal
                | ExpStringLiteral Literal
                | ExpCharLiteral Literal
                | ExpApplication Expression [Expression]
                | ExpBinarySymbol Symbol Expression Expression
                | ExpPrefixUnarySymbol Symbol Expression
                | ExpSuffixUnarySymbol Symbol Expression
                | ExpIf Expression Expression Expression -- a?b:c
                | ExpLambda [ArgumentList] Statement
                | ExpStatement
    deriving (Eq, Show, Read)

data Pattern = PatName Name
             | PatDataConstructor Name [Pattern]
             | PatWildcard
             | PatLiteral Literal
             | PatTuple [Pattern]
             | PatList [Pattern]
             | PatAs Name Pattern
    deriving (Eq, Show, Read)

type ArgumentList = [(Type,Maybe (Name,Maybe Expression))]
data FunctionDeclaration = FDec (Maybe TemplateDefinitionList) Type Name [ArgumentList] deriving (Eq, Show, Read)
data FunctionDefinition = FDef FunctionDeclaration Statement deriving (Eq, Show, Read)

data VariantDeclaration = VDec Type Name deriving (Eq, Show, Read)
data VariantDefinition = VDef Type Name (Maybe Expression) deriving (Eq, Show, Read)

{-
data DataType = Struct | Class deriving (Eq, Show, Read)
data AccessModifier = Private | Public | Protected deriving (Eq, Show, Read)
data DataHeader = DHdr DataType Name [Template] [(AccessModifier, Type)] deriving (Eq, Show, Read)
data DataDeclaration = DDec
    DataHeader
    [(AccessModifier, VariantDeclaration)]
    [(AccessModifier, FunctionDeclaration)]
    [(AccessModifier, TypeAliasDeclaration)]
    deriving (Eq, Show, Read)

data AlgebraicData = AD [(Name, [(Type, Name)])] deriving (Eq, Show, Read)

data TypeAlias = TA Name [Template] Type deriving (Eq, Show, Read)
-}

data Token = TokVDec VariantDeclaration
           | TokVDef VariantDefinition
           | TokFDec FunctionDeclaration
           | TokFDef FunctionDefinition
           {-
           | TokDataDeclaration DataDeclaration
           | TokTypeAliasDeclaration TypeAliasDeclaration
           -}
           | TokComment String
           | TokCppCompilerDirective String
           | TokHashCompilerDirective String
           | Tok String
    deriving (Eq, Show, Read)

