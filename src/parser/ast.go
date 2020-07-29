package parser

type FunctionType byte

const (
	OrdFunction    FunctionType = 1
	AsyncFunction  FunctionType = 2
	WorkFunction   FunctionType = 4
	InlineFunction FunctionType = 8
)

type StatementType byte

const (
	ImportStatement        StatementType = 2
	StructTypedefStatement StatementType = 3
	EnumTypedefStatement   StatementType = 4
	TupleTypedefStatement  StatementType = 5

	DeclarationStatement StatementType = 6
	AssignmentStatement  StatementType = 7
	ExpressionStatement  StatementType = 8
)

type LoopType byte

const (
	InitCondLoop LoopType = 1
	InitCond     LoopType = 2
	Cond         LoopType = 3
	NoneLoop     LoopType = 4
)

type DeclarationStruct struct {
	Identifiers []Token
	Types       []TypeStruct
	Values      []ExpressionStruct
}

type TypeType byte

const (
	IdentifierType TypeType = 1
	FuncType       TypeType = 2
	StructType     TypeType = 3
	TupleType      TypeType = 4
)

type TypeStruct struct {
	Type         TypeType
	PointerIndex byte

	Identifier Token
	FuncType   FunctionTypeStruct
	StructType StructTypeStruct
	TupleType  TupleTypeStruct
}

type TupleTypeStruct struct {
	Identifier Token
	Types      []TypeStruct
}

type StructTypeStruct struct {
	Identifier Token
	Props      []StructPropStruct
}

type StructPropStruct struct {
	Identifier Token
	Type       TypeStruct
	Value      ExpressionStruct
}

type EnumTypeStruct struct {
	Identifier  Token
	Identifiers []Token
}

type FunctionTypeStruct struct {
	Type        FunctionType
	Args        []TypeStruct
	ReturnTypes []TypeStruct
}

type FunctionExpressionStruct struct {
	Type        FunctionType
	Args        []ArgStruct
	ReturnTypes []TypeStruct
	Block       BlockStruct
}

type ArgStruct struct {
	Identifier Token
	Type       TypeStruct
}

type BlockStruct struct {
	Statements []StatementStruct
}

type StatementStruct struct {
	Type StatementType

	Expression  ExpressionStruct
	Declaration DeclarationStruct
	Funct       FunctionTypeStruct
	Strct       StructTypeStruct
	Enum        EnumTypeStruct
	Tupl        TupleTypeStruct
}

type LoopStruct struct {
	Type          LoopType
	InitStatement StatementStruct
	Condition     ExpressionStruct
	LoopStatement StatementStruct
	Block         BlockStruct
}

type ExpressionStruct struct {
	expr_left  *ExpressionStruct
	op         Token
	expr_right *ExpressionStruct
}

type IfElseBlockStruct struct {
	InitStatement StatementStruct
	Conditions    []ExpressionStruct
	Blocks        []BlockStruct
	ElseBlock     BlockStruct
}

type SwitchType byte

type SwitchStruct struct {
	InitStatement StatementStruct
	Expression    ExpressionStruct
	Cases         []ExpressionStruct
}
