module HashToken where
import Pappy.Pos

data Name = Name String | NameInType [Type] String | ThisType | TildaThisType deriving (Eq, Show, Read)
type Literal = String
type Operator = String
data WriteModifier = Const | Mutable | NoWM deriving (Eq, Show, Read)

data TemplateDefinitionList = TDL [(Type,Maybe Name)] deriving (Eq, Show, Read)
data TemplateApplicationList = TAL [Either Type Expression] deriving (Eq, Show, Read)

data Type = TypName Name
          | TypApplication Type TemplateApplicationList
          | TypBinaryOperator Operator Type Type
          | TypPrefixOperator Operator Type
          | TypSuffixOperator Operator Type
          | TypArray Type Int
          | TypTuple [Type]
          | TypList Type
          | TypAuto
          | TypDecltype Expression
          | TypTypeType Type Type
          | TypNothing
    deriving (Eq, Show, Read)

data Expression = ExpName Name
                | ExpOperator Operator
                | ExpNumberLiteral Literal
                | ExpStringLiteral Literal
                | ExpCharLiteral Literal
                | ExpApplication Expression [Expression]
                | ExpTuple [Expression]
                | ExpUnit
                | ExpList [Expression]
                | ExpInitializerList [Expression]
                | ExpBinaryOperator Operator Expression Expression
                | ExpPrefixOperator Operator Expression
                | ExpSuffixOperator Operator Expression
                | ExpIf Expression Expression (Maybe Expression)
                | ExpLambda (Maybe Type) ArgumentList Statement
                | ExpStatement Statement
                | ExpData Name [Expression]
                | ExpMatch Expression [(Pattern,Expression)]
                | ExpTypeOperator Operator Expression Type
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
               | SttMatch Expression [(Pattern,Statement)]
               | SttNothing
    deriving (Eq, Show, Read)

type OnlyThem = Bool
data Pattern = PatName Name
             | PatDataConstructor Name [Pattern]
             | PatWildcard
             | PatWildcardOthers
             | PatLiteral Literal
             | PatTuple [Pattern]
             | PatList [Pattern]
             | PatAs Name Pattern
             | PatNot Pattern
             | PatOr Pattern Pattern
             | PatAnd Pattern Pattern
             | PatRecord [(Name,Pattern)]
             | PatEqual Expression
             | PatType Type
             | PatWhen Pattern Expression
             | PatUnless Pattern Expression
    deriving (Eq, Show, Read)

data Initialization = InitNo | InitExp Expression | InitArg [Expression] | InitList [Expression] deriving (Eq, Show, Read)
data VariantDeclaration = VDec Type [Name] deriving (Eq, Show, Read)
data VariantDefinition = VDef Type [(Name,Initialization)] deriving (Eq, Show, Read)

data ArgumentList = AList [(Type,Maybe (Name,Maybe Expression))] WriteModifier deriving (Eq, Show, Read)
data FunctionDeclaration = FDec (Maybe TemplateDefinitionList) Type Name ArgumentList
    deriving (Eq, Show, Read)
data FunctionDefinition = FDef FunctionDeclaration Statement
                        | FDefC FunctionDeclaration [Expression] Statement
    deriving (Eq, Show, Read)

data DataType = Struct | Class deriving (Eq, Show, Read)
type Virtual = Bool
data MemberModifier = MM AccessModifier Virtual deriving (Eq, Show, Read)
data AccessModifier = Private | Public | Protected | Default deriving (Eq, Show, Read)
data DataWhere = DWSame Type Type
               | DWBase Type Type
               | DWTypeIs Type String
               | DWTypeHas Type String
               | DWExpression Expression
    deriving (Eq, Show, Read)
data DataHeader = DH (Maybe TemplateDefinitionList) DataType Name [(AccessModifier, Type)] [DataWhere] deriving (Eq, Show, Read)
data DataDefinition = DDef
    DataHeader
    Tokens
    deriving (Eq, Show, Read)
data DataDeclaration = DDec (Maybe TemplateDefinitionList) DataType Name deriving (Eq, Show, Read)

type EnumBit = Bool
type EnumStrong = Bool
data EnumDataDefinition = EDef EnumBit EnumStrong Name (Maybe Type) [(Name,Maybe Expression)] deriving (Eq, Show, Read)
data EnumDataDeclaration = EDec EnumStrong Name deriving (Eq, Show, Read)

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
           | TokEDec EnumDataDeclaration
           | TokEDef EnumDataDefinition
           | TokMM MemberModifier
           | TokTA TypeAlias
           | TokStatement Statement
           | TokComment String
           | TokLabel Name
           | TokCppCompilerDirective String
           | TokNamespace [Name] Tokens
           | TokEmpty
           | TokError Error
           | TokPos Pos
    deriving (Eq, Show, Read)

data Tokens = Tokens [Token] deriving (Eq, Show, Read)

