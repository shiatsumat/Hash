module HashToken where

type Name = String
type Literal = String
type Symbol = String

data Type = TypName Name
          | TypConst Type
          | TypPointer Type
          | TypReference Type
          | TypRvalueReference Type
          | TypApplication Type [Either Type Expression]
          | TypArray Type
          | TypFunction Type Type
          | TypStatic Type
          | TypTemplate Type [Type]
          | TypAuto
    deriving (Eq, Show, Read)
data Template = TempType Type
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
    deriving (Eq, Show, Read)
data AlgebraicData = AHdr [(Name, [(Type, Name)])] deriving (Eq, Show, Read)

data TypeDeclaration = TDecl Name [Template] Type deriving (Eq, Show, Read)

data Token = TokVariantDeclaration VariantDeclaration
           | TokVariantDefinition VariantDefinition
           | TokFunctionDeclaration FunctionDeclaration
           | TokFunctionDefinition FunctionDefinition
           | TokDataDeclaration DataDeclaration
           | TokTypeDeclaration TypeDeclaration
           | TokCppCompilerDirective String
           | TokHashCompilerDirective String
    deriving (Eq, Show, Read)

