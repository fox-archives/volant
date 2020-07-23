package parser

// PrimaryTokenType enum{Identifier, NumberLiteral, StringLiteral, Keyword, StringDelimiter, CharDelimiter, LeftParen, RigthParen, LeftBrace, RightBrace}
type PrimaryTokenType byte

// SecondaryTokenType enum{DecimalRadix, BinaryRadix, OctalRadix, HexadecimalRadix}
type SecondaryTokenType byte

// The numbers here are just to keep values different, they dont mean anything specific

// Primary token type
const (
	PrimaryNullType PrimaryTokenType = 0

	Identifier    PrimaryTokenType = 1
	NumberLiteral PrimaryTokenType = 2
	StringLitral  PrimaryTokenType = 3
	CharLiteral   PrimaryTokenType = 4
	Keyword       PrimaryTokenType = 5

	StringDelimiter PrimaryTokenType = 6
	CharDelimiter   PrimaryTokenType = 7

	LeftParen       PrimaryTokenType = 8
	RightParen      PrimaryTokenType = 9
	LeftBrace       PrimaryTokenType = 10
	RightBrace      PrimaryTokenType = 11
	LeftCurlyBrace  PrimaryTokenType = 12
	RightCurlyBrace PrimaryTokenType = 13

	// Operators
	AirthmaticOperator PrimaryTokenType = 51
	AssignmentOperator PrimaryTokenType = 52
	RelationalOperator PrimaryTokenType = 53
	LogicalOperator    PrimaryTokenType = 54
	BitwiseOperator    PrimaryTokenType = 55
	SpecialOperator    PrimaryTokenType = 56

	// Keywords
	ForKeyword      PrimaryTokenType = 101
	SwitchKeyword   PrimaryTokenType = 102
	IfKeyword       PrimaryTokenType = 103
	ElseKeyword     PrimaryTokenType = 104
	FunctionKeyword PrimaryTokenType = 105
	StructKeyword   PrimaryTokenType = 106
	EnumKeyword     PrimaryTokenType = 107
	CaseKeyword     PrimaryTokenType = 208

	// the parser stops parsing whenever it receives either of these types and shows the correct error message
	EOF        PrimaryTokenType = 254
	ErrorToken PrimaryTokenType = 255
	// ...more stuff
)

// Secondary token type
const (
	SecondaryNullType SecondaryTokenType = 0

	// Radix for number literals
	DecimalRadix     SecondaryTokenType = 1
	BinaryRadix      SecondaryTokenType = 2
	OctalRadix       SecondaryTokenType = 3
	HexadecimalRadix SecondaryTokenType = 4

	// Airthmatic Operators
	Add     SecondaryTokenType = 11
	Sub     SecondaryTokenType = 12
	Mul     SecondaryTokenType = 13
	Div     SecondaryTokenType = 14
	Modulus SecondaryTokenType = 15

	// Assignment Operators
	AddEqual     SecondaryTokenType = 21
	SubEqual     SecondaryTokenType = 22
	MulEqual     SecondaryTokenType = 23
	DivEqual     SecondaryTokenType = 24
	ModulusEqual SecondaryTokenType = 25
	Equal        SecondaryTokenType = 26
	AddAdd       SecondaryTokenType = 27
	SubSub       SecondaryTokenType = 28

	// Realtional Operators
	EqualEqual   SecondaryTokenType = 31
	NotEqual     SecondaryTokenType = 32
	Greater      SecondaryTokenType = 33
	Less         SecondaryTokenType = 34
	LessEqual    SecondaryTokenType = 35
	GreaterEqual SecondaryTokenType = 36

	// Logical Operators
	AndAnd SecondaryTokenType = 41
	OrOr   SecondaryTokenType = 42
	Not    SecondaryTokenType = 43

	// Bitwise Opeartors
	LeftShift   SecondaryTokenType = 51
	RightShift  SecondaryTokenType = 52
	Or          SecondaryTokenType = 53
	And         SecondaryTokenType = 54
	ExclusiveOr SecondaryTokenType = 55

	// Special Operators
	Colon    SecondaryTokenType = 61
	QuesMark SecondaryTokenType = 62
	Dot      SecondaryTokenType = 63

	// Used with ErrorToken
	NotFound    SecondaryTokenType = 101
	UnknownChar SecondaryTokenType = 102
)

// Keywords contains an array of all valid keywords of the lang
var Keywords = map[string]PrimaryTokenType{
	"if":     IfKeyword,
	"else":   ElseKeyword,
	"for":    ForKeyword,
	"switch": SwitchKeyword,
	"case":   CaseKeyword,
	"enum":   EnumKeyword,
	"struct": StructKeyword,
	// more stuff
}

// Token ...
type Token struct {
	primaryType   PrimaryTokenType
	secondaryType SecondaryTokenType
	buff          []byte
}
