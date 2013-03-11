module HashToken where
import Pappy.Pos

data Name = Name String | NameInType [Type] String | ThisType | TildaThisType deriving (Eq, Show, Read)
type Literal = String
type Symbol = String
data WriteModifier = Const | Mutable | NoWM deriving (Eq, Show, Read)

data TemplateDefinitionList = TDL [(Type,Maybe Name)] deriving (Eq, Show, Read)
data TemplateApplicationList = TAL [Either Type Expression] deriving (Eq, Show, Read)

data Type = TypName Name
          | TypSigned Name
          | TypUnsigned Name
          | TypLong Type
          | TypApplication Type TemplateApplicationList
          | TypConst Type
          | TypMutable Type
          | TypPointer Type
          | TypReference Type
          | TypRvalueReference Type
          | TypFunction Type Type
          | TypArray Type Int
          | TypTuple [Type]
          | TypList Type
          | TypAuto
          | TypDecltype Expression
          | TypTypeType Type Type
          | TypTypename Type
          | TypNothing
    deriving (Eq, Show, Read)

data Expression = ExpName Name
                | ExpSymbol Symbol
                | ExpNumberLiteral Literal
                | ExpStringLiteral Literal
                | ExpCharLiteral Literal
                | ExpApplication Expression [Expression]
                | ExpTuple [Expression]
                | ExpUnit
                | ExpList [Expression]
                | ExpBinarySymbol Symbol Expression Expression
                | ExpPrefixUnarySymbol Symbol Expression
                | ExpSuffixUnarySymbol Symbol Expression
                | ExpIf Expression Expression Expression
                | ExpLambda (Maybe Type) ArgumentList Statement
                | ExpStatement Statement
                | ExpNothing
    deriving (Eq, Show, Read)


data Statement = SttEmpty
               | SttSingle Expression
               | SttBlock Tokens
               | SttControlFlow String (Either Expression VariantDefinition) Statement
               | SttFor (Either Expression VariantDefinition) Expression Expression Statement
               | SttSwitch Expression ([Expression],Statement)
               | SttReturn Expression
               | SttGoto Name
               | SttContinue
               | SttBreak
               | SttIf (Either Expression VariantDefinition) Statement (Maybe Statement)
               | SttWhite [Token] Statement
               | SttNothing
    deriving (Eq, Show, Read)

data Pattern = PatName Name
             | PatDataConstructor Name [Pattern]
             | PatWildcard
             | PatLiteral Literal
             | PatTuple [Pattern]
             | PatList [Pattern]
             | PatAs Name Pattern
    deriving (Eq, Show, Read)

data VariantDeclaration = VDec Type Name deriving (Eq, Show, Read)
data VariantDefinition = VDef Type [(Name,Maybe Expression)] deriving (Eq, Show, Read)

data ArgumentList = AList [(Type,Maybe (Name,Maybe Expression))] WriteModifier deriving (Eq, Show, Read)
data FunctionDeclaration = FDec (Maybe TemplateDefinitionList) Type Name ArgumentList
    deriving (Eq, Show, Read)
data FunctionDefinition = FDef FunctionDeclaration Statement
                        | FDefC FunctionDeclaration [Expression] Statement
    deriving (Eq, Show, Read)

data DataType = Struct | Class deriving (Eq, Show, Read)
data AccessModifier = Private | Public | Protected | Default deriving (Eq, Show, Read)
data DataWhere = DWSame Type Type
               | DWBase Type Type
               | DWTypeIs Type String
               | DWTypeHas Type String
               | DWExpression Expression
    deriving (Eq, Show, Read)
data DataDeclaration = DDec (Maybe TemplateDefinitionList) DataType Name deriving (Eq, Show, Read)
data DataHeader = DH (Maybe TemplateDefinitionList) DataType Name [(AccessModifier, Type)] [DataWhere] deriving (Eq, Show, Read)
data DataDefinition = DDef
    DataHeader
    [(AccessModifier, Token)]
    deriving (Eq, Show, Read)

type EnumBit = Bool
type EnumStrong = Bool
data EnumData = ED EnumBit EnumStrong Name (Maybe Type) [(Name,Maybe Expression)] deriving (Eq, Show, Read)

data AlgebraicData = AD [(Name, [(Type, Name)])] deriving (Eq, Show, Read)

data TypeAlias = TA Name Type deriving (Eq, Show, Read)

instance Read Pos where
    readsPrec _ s = [(Pos "" 0 0,s)]
data Error = Error Pos String deriving (Eq, Show, Read)

data Token = TokVDec VariantDeclaration
           | TokVDef VariantDefinition
           | TokFDec FunctionDeclaration
           | TokFDef FunctionDefinition
           | TokDDec DataDeclaration
           | TokDDef DataDefinition
           | TokED EnumData
           | TokComment String
           | TokCppCompilerDirective String
           | TokHashCompilerDirective String
           | TokLabel Name
           | TokStatement Statement
           | TokEmpty
           | TokError Error
           | TokPos Pos
    deriving (Eq, Show, Read)

data Tokens = Tokens [Token] deriving (Eq, Show, Read)

