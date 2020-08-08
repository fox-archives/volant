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

type ArgStruct struct {
	Identifier Token
	Type       Type
}

type CaseStruct struct {
	Condition Expression
	Block     Block
}

type Statement interface {
	isStatement()
}

type Expression interface {
	isExpression()
	isStatement()
}

type Type interface {
	isType()
	isExpression()
	isStatement()
}

type (
	Block struct {
		Statements []Statement
	}
	Declaration struct {
		Identifiers []Token
		Types       []Type
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
		Expr           Expression
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
	EnumTypedef struct {
		Name Token
		Type EnumType
	}
	TupleTypedef struct {
		Identifier Token
		Type       TupleType
	}
	StructTypedef struct {
		Identifier Token
		Type       StructType
	}
	Defer struct {
		Stmt Statement
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

	FuncExpr struct {
		Type        FunctionType
		Args        []ArgStruct
		ReturnTypes []Type
		Block       Block
	}

	CallExpr struct {
		Function Expression
		Args     []Expression
	}

	TypeCast struct {
		Type Expression
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

	ArrayLiteral struct {
		Exprs []Expression
	}

	HeapAlloc struct {
		Type Expression
	}
)

type (
	FuncType struct {
		Type        FunctionType
		Args        []Type
		ReturnTypes []Type
	}

	StructType struct {
		Props []Declaration
	}

	TupleType struct {
		Types []Type
	}

	EnumType struct {
		Identifiers []Token
		Values      []Expression
	}

	BasicType struct {
		Expr Expression
	}

	PointerType struct {
		BaseType Type
	}

	DynamicType struct {
		BaseType Type
	}

	ConstType struct {
		BaseType Type
	}

	ImplictArrayType struct {
		BaseType Type
	}

	ArrayType struct {
		Size     Token
		BaseType Type
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
func (EnumTypedef) isStatement()   {}
func (TupleTypedef) isStatement()  {}
func (StructTypedef) isStatement() {}
func (NullStatement) isStatement() {}
func (Break) isStatement()         {}
func (Continue) isStatement()      {}
func (Defer) isStatement()         {}

func (BasicLit) isExpression()            {}
func (BinaryExpr) isExpression()          {}
func (UnaryExpr) isExpression()           {}
func (ArrExpr) isExpression()             {}
func (CallExpr) isExpression()            {}
func (ParenExpr) isExpression()           {}
func (FuncExpr) isExpression()            {}
func (TernaryExpr) isExpression()         {}
func (PostfixUnaryExpr) isExpression()    {}
func (TypeCast) isExpression()            {}
func (IdentExpr) isExpression()           {}
func (MemberExpr) isExpression()          {}
func (ArrayMemberExpr) isExpression()     {}
func (CompoundLiteral) isExpression()     {}
func (CompoundLiteralData) isExpression() {}
func (HeapAlloc) isExpression()           {}
func (ArrayLiteral) isExpression()        {}

func (BasicLit) isStatement()            {}
func (BinaryExpr) isStatement()          {}
func (UnaryExpr) isStatement()           {}
func (ArrExpr) isStatement()             {}
func (CallExpr) isStatement()            {}
func (ParenExpr) isStatement()           {}
func (FuncExpr) isStatement()            {}
func (TernaryExpr) isStatement()         {}
func (PostfixUnaryExpr) isStatement()    {}
func (TypeCast) isStatement()            {}
func (IdentExpr) isStatement()           {}
func (MemberExpr) isStatement()          {}
func (ArrayMemberExpr) isStatement()     {}
func (CompoundLiteral) isStatement()     {}
func (CompoundLiteralData) isStatement() {}
func (HeapAlloc) isStatement()           {}
func (ArrayLiteral) isStatement()        {}

func (BasicType) isType()        {}
func (StructType) isType()       {}
func (EnumType) isType()         {}
func (TupleType) isType()        {}
func (FuncType) isType()         {}
func (ConstType) isType()        {}
func (PointerType) isType()      {}
func (ArrayType) isType()        {}
func (DynamicType) isType()      {}
func (ImplictArrayType) isType() {}

func (BasicType) isExpression()        {}
func (StructType) isExpression()       {}
func (EnumType) isExpression()         {}
func (TupleType) isExpression()        {}
func (FuncType) isExpression()         {}
func (ConstType) isExpression()        {}
func (PointerType) isExpression()      {}
func (ArrayType) isExpression()        {}
func (DynamicType) isExpression()      {}
func (ImplictArrayType) isExpression() {}

func (BasicType) isStatement()        {}
func (StructType) isStatement()       {}
func (EnumType) isStatement()         {}
func (TupleType) isStatement()        {}
func (FuncType) isStatement()         {}
func (ConstType) isStatement()        {}
func (PointerType) isStatement()      {}
func (ArrayType) isStatement()        {}
func (DynamicType) isStatement()      {}
func (ImplictArrayType) isStatement() {}
