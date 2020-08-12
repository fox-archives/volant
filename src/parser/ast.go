package parser

type FunctionType byte

// Never change these numbers, they are very sepcific
const (
	OrdFunction   FunctionType = 1
	AsyncFunction FunctionType = 2
	WorkFunction  FunctionType = 3
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

type File struct {
	Path       string
	Statements []Statement
}

type (
	Block struct {
		Statements []Statement
		Line       int
		Column     int
	}
	Declaration struct {
		Identifiers []Token
		Types       []Type
		Values      []Expression
		Line        int
		Column      int
	}
	Import struct {
		Paths  []Token
		Line   int
		Column int
	}
	Loop struct {
		Type          LoopType
		InitStatement Statement
		Condition     Expression
		LoopStatement Statement
		Block         Block
		Line          int
		Column        int
	}
	Switch struct {
		Type           SwitchType
		InitStatement  Statement
		Expr           Expression
		Cases          []CaseStruct
		HasDefaultCase bool
		DefaultCase    Block
		Line           int
		Column         int
	}
	IfElseBlock struct {
		HasInitStmt   bool
		InitStatement Statement
		Conditions    []Expression
		Blocks        []Block
		ElseBlock     Block
		Line          int
		Column        int
	}
	Return struct {
		Values []Expression
		Line   int
		Column int
	}
	Assignment struct {
		Variables []Expression
		Op        Token
		Values    []Expression
		Line      int
		Column    int
	}
	Defer struct {
		Stmt   Statement
		Line   int
		Column int
	}
	Delete struct {
		Exprs  []Expression
		Line   int
		Column int
	}
	Typedef struct {
		Name        Token
		DefaultName Token
		Type        Type
		Line        int
		Column      int
	}
	Break struct {
		Line   int
		Column int
	}
	Continue struct {
		Line   int
		Column int
	}
	ExportStatement struct {
		Stmt Statement
	}
	NullStatement struct {
		Line   int
		Column int
	}
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

	FuncExpr struct {
		Type  FuncType
		Block Block
	}

	CallExpr struct {
		Function Expression
		Args     []Expression
	}

	TypeCast struct {
		Type Type
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
		Type Type
	}

	LenExpr struct {
		Expr Expression
		Type Type
	}

	SizeExpr struct {
		Expr Expression
		Type Type
	}
)

type (
	FuncType struct {
		Type        FunctionType
		ArgTypes    []Type
		ArgNames    []Token
		ReturnTypes []Type
	}

	StructType struct {
		Props            []Declaration
		SuperStructs     []Expression
		SuperStructTypes []Type
	}

	TupleType struct {
		Types []Type
	}

	EnumType struct {
		Identifiers []Token
		Values      []Expression
	}

	UnionType struct {
		Identifiers []Token
		Types       []Type
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

func (Block) isStatement()           {}
func (Declaration) isStatement()     {}
func (Import) isStatement()          {}
func (Loop) isStatement()            {}
func (Switch) isStatement()          {}
func (IfElseBlock) isStatement()     {}
func (Return) isStatement()          {}
func (Assignment) isStatement()      {}
func (NullStatement) isStatement()   {}
func (Break) isStatement()           {}
func (Continue) isStatement()        {}
func (Defer) isStatement()           {}
func (Delete) isStatement()          {}
func (ExportStatement) isStatement() {}

func (BasicLit) isExpression()            {}
func (BinaryExpr) isExpression()          {}
func (UnaryExpr) isExpression()           {}
func (CallExpr) isExpression()            {}
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
func (LenExpr) isExpression()             {}
func (SizeExpr) isExpression()            {}

func (BasicLit) isStatement()            {}
func (BinaryExpr) isStatement()          {}
func (UnaryExpr) isStatement()           {}
func (CallExpr) isStatement()            {}
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
func (LenExpr) isStatement()             {}
func (SizeExpr) isStatement()            {}

func (BasicType) isType()        {}
func (StructType) isType()       {}
func (EnumType) isType()         {}
func (TupleType) isType()        {}
func (UnionType) isType()        {}
func (FuncType) isType()         {}
func (ConstType) isType()        {}
func (PointerType) isType()      {}
func (ArrayType) isType()        {}
func (DynamicType) isType()      {}
func (ImplictArrayType) isType() {}
func (Typedef) isType()          {}

func (BasicType) isExpression()        {}
func (StructType) isExpression()       {}
func (EnumType) isExpression()         {}
func (TupleType) isExpression()        {}
func (UnionType) isExpression()        {}
func (FuncType) isExpression()         {}
func (ConstType) isExpression()        {}
func (PointerType) isExpression()      {}
func (ArrayType) isExpression()        {}
func (DynamicType) isExpression()      {}
func (ImplictArrayType) isExpression() {}
func (Typedef) isExpression()          {}

func (BasicType) isStatement()        {}
func (StructType) isStatement()       {}
func (EnumType) isStatement()         {}
func (TupleType) isStatement()        {}
func (UnionType) isStatement()        {}
func (FuncType) isStatement()         {}
func (ConstType) isStatement()        {}
func (PointerType) isStatement()      {}
func (ArrayType) isStatement()        {}
func (DynamicType) isStatement()      {}
func (ImplictArrayType) isStatement() {}
func (Typedef) isStatement()          {}

var VoidType = BasicType{
	Expr: IdentExpr{
		Value: Token{
			Buff:          []byte("void"),
			PrimaryType:   Identifier,
			SecondaryType: SecondaryNullType,
		},
	},
}
