package parser

type FunctionType byte

// Never change these numbers, they are very sepcific
const (
	OrdFunction    FunctionType = 1
	AsyncFunction  FunctionType = 2
	WorkFunction   FunctionType = 4
	InlineFunction FunctionType = 8
)

type LoopType byte

const (
	InitLoop LoopType = 1
	CondLoop LoopType = 2
	LoopLoop LoopType = 4
	NoneLoop LoopType = 8
)

type SwitchType byte

const (
	InitCondSwitch SwitchType = 1
	CondSwitch     SwitchType = 2
	NoneSwtch      SwitchType = 3
)

type BasicType byte

const (
	U8Type  BasicType = 1
	U16Type BasicType = 2
	U32Type BasicType = 3
	U64Type BasicType = 4
	I8Type  BasicType = 5
	I16Type BasicType = 6
	I32Type BasicType = 7
	I64Type BasicType = 8
)

var BasicTypeDebug = map[BasicType]string{
	U8Type:  "U8Type",
	U16Type: "U16Type",
	U32Type: "U32Type",
	U64Type: "U64Type",
	I8Type:  "I8Type",
	I16Type: "I16Type",
	I32Type: "I32Type",
	I64Type: "I64Type",
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
	StructType Struct
	TupleType  Tuple
}

type StructPropStruct struct {
	Identifier Token
	Type       TypeStruct
	Value      Expression
}

type FunctionTypeStruct struct {
	Type        FunctionType
	Args        []TypeStruct
	ReturnTypes []TypeStruct
}

type ArgStruct struct {
	Identifier Token
	Type       TypeStruct
}

type CaseStruct struct {
	Condition  Expression
	Statements []Statement
}

type Statement interface {
	isStatement()
}

type Expression interface {
	isExpression()
	isStatement()
}

type (
	Block struct {
		Statements []Statement
	}
	Declaration struct {
		Identifiers []Token
		Types       []TypeStruct
		Values      []Expression
	}
	Import struct {
		Paths []Token
	}
	Loop struct {
		Type          LoopType
		InitStatement Statement
		Condition     Expression
		LoopStatement Statement
		Block         Block
	}
	Switch struct {
		Type           SwitchType
		InitStatement  Statement
		Condition      Expression
		Cases          []CaseStruct
		HasDefaultCase bool
		DefaultCase    Block
	}
	IfElseBlock struct {
		HasInitStmt   bool
		InitStatement Statement
		Conditions    []Expression
		Blocks        []Block
		ElseBlock     Block
	}
	Return struct {
		Values []Expression
	}
	Assignment struct {
		Variables []Expression
		Op        Token
		Values    []Expression
	}
	Enum struct {
		Name        Token
		Identifiers []Token
		Values      []Expression
	}
	Tuple struct {
		Identifier Token
		Types      []TypeStruct
	}
	Struct struct {
		Identifier Token
		Props      []Declaration
	}
	Break         struct{}
	Continue      struct{}
	NullStatement struct{}
)

type (
	BasicLit struct {
		Value Token
	}

	BinaryExpr struct {
		Left  Expression
		Op    Token
		Right Expression
	}

	UnaryExpr struct {
		Op   Token
		Expr Expression
	}

	PostfixUnaryExpr struct {
		Op   Token
		Expr Expression
	}

	TernaryExpr struct {
		Cond  Expression
		Left  Expression
		Right Expression
	}

	ArrExpr struct {
		Expr []Expression
	}

	ParenExpr struct {
		Expr Expression
	}

	FunctionExpression struct {
		Type        FunctionType
		Args        []ArgStruct
		ReturnTypes []TypeStruct
		Block       Block
	}

	CallExpr struct {
		Function Expression
		Args     []Expression
	}

	TypeCast struct {
		Type TypeStruct
		Expr Expression
	}

	IdentExpr struct {
		Value Token
	}

	MemberExpr struct {
		Base Expression
		Expr Expression
	}

	ArrayMemberExpr struct {
		Parent Expression
		Index  Expression
	}

	CompoundLiteral struct {
		Name Expression
		Data CompoundLiteralData
	}

	CompoundLiteralData struct {
		Fields []Token
		Values []Expression
	}
)

func (Block) isStatement()         {}
func (Declaration) isStatement()   {}
func (Import) isStatement()        {}
func (Loop) isStatement()          {}
func (Switch) isStatement()        {}
func (IfElseBlock) isStatement()   {}
func (Return) isStatement()        {}
func (Assignment) isStatement()    {}
func (Enum) isStatement()          {}
func (Tuple) isStatement()         {}
func (Struct) isStatement()        {}
func (NullStatement) isStatement() {}
func (Break) isStatement()         {}
func (Continue) isStatement()      {}

func (BasicLit) isExpression()            {}
func (BinaryExpr) isExpression()          {}
func (UnaryExpr) isExpression()           {}
func (ArrExpr) isExpression()             {}
func (CallExpr) isExpression()            {}
func (ParenExpr) isExpression()           {}
func (FunctionExpression) isExpression()  {}
func (TernaryExpr) isExpression()         {}
func (PostfixUnaryExpr) isExpression()    {}
func (TypeCast) isExpression()            {}
func (IdentExpr) isExpression()           {}
func (MemberExpr) isExpression()          {}
func (ArrayMemberExpr) isExpression()     {}
func (CompoundLiteral) isExpression()     {}
func (CompoundLiteralData) isExpression() {}

func (BasicLit) isStatement()            {}
func (BinaryExpr) isStatement()          {}
func (UnaryExpr) isStatement()           {}
func (ArrExpr) isStatement()             {}
func (CallExpr) isStatement()            {}
func (ParenExpr) isStatement()           {}
func (FunctionExpression) isStatement()  {}
func (TernaryExpr) isStatement()         {}
func (PostfixUnaryExpr) isStatement()    {}
func (TypeCast) isStatement()            {}
func (IdentExpr) isStatement()           {}
func (MemberExpr) isStatement()          {}
func (ArrayMemberExpr) isStatement()     {}
func (CompoundLiteral) isStatement()     {}
func (CompoundLiteralData) isStatement() {}
