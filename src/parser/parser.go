package parser

type Parser struct {
	Lexer    *Lexer
	tokens   []Token
	position int
	Forks    map[byte]int
}

func (parser *Parser) ReadToken() Token {
	for parser.position >= len(parser.tokens) {
		parser.tokens = append(parser.tokens, parser.Lexer.NextToken())
	}
	return parser.tokens[parser.position]
}

func (parser *Parser) eatLastToken() {
	parser.position++
}

func (parser *Parser) fork(num byte) {
	parser.Forks[num] = parser.position
}

func (parser *Parser) moveToFork(num byte) {
	parser.position = parser.Forks[num]
}

func (parser *Parser) expect(primary PrimaryTokenType, secondary SecondaryTokenType) Token {
	token := parser.ReadToken()

	if primary == PrimaryNullType {
		if token.SecondaryType != secondary {
			NewError(SyntaxError, "expected "+SecondaryTypes[secondary]+", got "+token.Serialize(), token.Line, token.Column)
		}
	} else if secondary == SecondaryNullType {
		if token.PrimaryType != primary {
			NewError(SyntaxError, "expected "+PrimaryTypes[primary]+", got "+token.Serialize(), token.Line, token.Column)
		}
	} else {
		if token.PrimaryType != primary || token.SecondaryType != secondary {
			// Error: expected {primary, secondary}, got {token}
			NewError(SyntaxError, "expected "+PrimaryTypes[primary]+" and "+SecondaryTypes[secondary]+", got "+token.Serialize(), token.Line, token.Column)
		}
	}

	return token
}

func (parser *Parser) ParseGlobalStatement() Statement {
	var statement Statement

	switch token := parser.ReadToken(); token.PrimaryType {
	case ImportKeyword:
		parser.eatLastToken()
		statement = parser.parseImport()
	case StructKeyword:
		parser.eatLastToken()
		statement = parser.parseStructTypedef()
	case EnumKeyword:
		parser.eatLastToken()
		statement = parser.parseEnumTypedef()
	case TupleKeyword:
		parser.eatLastToken()
		statement = parser.parseTupleTypedef()
	case Identifier:
		statement = parser.parseDeclaration()
	default:
		// Error: Invalid token {token}
	}

	if parser.ReadToken().PrimaryType == SemiColon {
		parser.eatLastToken()
	}

	return statement
}

func (parser *Parser) parseStructTypedef() Statement {
	return parser.parseStructType(false)
}

func (parser *Parser) parseTupleTypedef() Statement {
	return parser.parseTupleType(false)
}

func (parser *Parser) parseEnumTypedef() Statement {
	return parser.parseEnumType()
}

func (parser *Parser) parseImport() Import {
	var imprt Import

	if token := parser.ReadToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()

		for token2 := parser.ReadToken(); token2.PrimaryType == Comma; parser.eatLastToken() {
			imprt.Paths = append(imprt.Paths, parser.expect(StringLiteral, SecondaryNullType))
			parser.eatLastToken()
		}

		parser.expect(RightParen, SecondaryNullType)
		parser.eatLastToken()

	} else if token.PrimaryType == StringLiteral {
		imprt.Paths = append(imprt.Paths, token)
		parser.eatLastToken()
	} else {
		// Error: expected string literal, got {token]
	}

	return imprt
}

func (parser *Parser) parseType(allowTypeDefs bool, alllowUnnamed bool) Type {
	var pointerIndex byte = 0
	var typ Type

	for token := parser.ReadToken(); token.SecondaryType == Mul; token = parser.ReadToken() {
		pointerIndex++
		parser.eatLastToken()
	}

	if token := parser.ReadToken(); token.PrimaryType == StructKeyword {
		if allowTypeDefs {
			parser.eatLastToken()
			structType := parser.parseStructType(alllowUnnamed)
			typ.Type = StructType
			typ.StructType = structType
		} else {
			// Error: type not allowed
		}
	} else if token.PrimaryType == TupleKeyword {
		if allowTypeDefs {
			parser.eatLastToken()
			tupleType := parser.parseTupleType(alllowUnnamed)
			typ.Type = TupleType
			typ.TupleType = tupleType
		} else {
			// Error: type not allowed
		}
	} else if token.PrimaryType == FunctionKeyword {
		parser.eatLastToken()
		funcType := parser.parseFunctionType()
		typ.Type = FuncType
		typ.FuncType = funcType
	} else if token.PrimaryType == Identifier {
		parser.eatLastToken()
		typ.Identifier = token
		typ.Type = IdentifierType
	}

	typ.PointerIndex = pointerIndex
	return typ
}

func (parser *Parser) parseTypeArray() []Type {
	types := []Type{}

	if token := parser.ReadToken(); token.PrimaryType == RightParen {
		return types
	}

	types = append(types, parser.parseType(false, false))

	for token := parser.ReadToken(); token.PrimaryType == Comma; token = parser.ReadToken() {
		parser.eatLastToken()
		types = append(types, parser.parseType(false, false))
	}

	return types
}

func (parser *Parser) parseFunctionType() FunctionTypeStruct {
	function := FunctionTypeStruct{}
	function.Type = FunctionType(0)

	// check for async/work/inline keyword
	if token := parser.ReadToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	if token := parser.ReadToken(); token.PrimaryType == AsyncKeyword {
		function.Type = function.Type | AsyncFunction
		parser.eatLastToken()
	} else if token.PrimaryType == WorkKeyword {
		function.Type = function.Type | WorkFunction
		parser.eatLastToken()
	} else {
		function.Type = OrdFunction
	}

	if token := parser.ReadToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	// parse arguments
	parser.expect(LeftParen, SecondaryNullType)
	parser.eatLastToken()

	function.Args = parser.parseTypeArray()

	parser.expect(RightParen, SecondaryNullType)
	parser.eatLastToken()

	// parse return types
	if token := parser.ReadToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()

		function.ReturnTypes = parser.parseTypeArray()

		parser.expect(RightParen, SecondaryNullType)
		parser.eatLastToken()
	} else if token.PrimaryType != Comma && token.PrimaryType != SemiColon && token.SecondaryType != Equal && token.PrimaryType != RightParen {
		function.ReturnTypes = []Type{parser.parseType(false, false)}
	}

	return function
}

func (parser *Parser) parseStructType(allowUnnamed bool) Struct {
	strct := Struct{}

	if token := parser.ReadToken(); token.PrimaryType == Identifier {
		parser.eatLastToken()
		strct.Identifier = token
	} else if !allowUnnamed {
		// Error: expected identifier, got {token}
	}

	parser.expect(LeftCurlyBrace, SecondaryNullType)
	parser.eatLastToken()

	for {
		strct.Props = append(strct.Props, parser.parseDeclaration())

		if parser.ReadToken().PrimaryType == SemiColon {
			parser.eatLastToken()
		}

		if parser.ReadToken().PrimaryType == RightCurlyBrace {
			parser.eatLastToken()
			break
		}
	}

	return strct
}

func (parser *Parser) parseTupleType(allowUnnamed bool) Tuple {
	tupl := Tuple{}

	if token := parser.ReadToken(); token.PrimaryType == Identifier {
		parser.eatLastToken()
		tupl.Identifier = token
	} else if !allowUnnamed {
		// Error: expected identifier, got {token}
	}

	parser.expect(LeftCurlyBrace, SecondaryNullType)
	parser.eatLastToken()

	tupl.Types = parser.parseTypeArray()

	parser.expect(RightCurlyBrace, SecondaryNullType)
	parser.eatLastToken()

	return tupl
}

func (parser *Parser) parseEnumType() Enum {
	enum := Enum{}

	enum.Name = parser.expect(Identifier, SecondaryNullType)
	parser.eatLastToken()

	parser.expect(LeftCurlyBrace, SecondaryNullType)
	parser.eatLastToken()

	enum.Identifiers = append(enum.Identifiers, parser.expect(Identifier, SecondaryNullType))
	parser.eatLastToken()

	if parser.ReadToken().SecondaryType == Equal {
		parser.eatLastToken()
		enum.Values = append(enum.Values, parser.parseExpression())
	} else {
		enum.Values = append(enum.Values, nil)
	}

	for parser.ReadToken().PrimaryType == Comma {
		parser.eatLastToken()

		enum.Identifiers = append(enum.Identifiers, parser.expect(Identifier, SecondaryNullType))
		parser.eatLastToken()

		if parser.ReadToken().SecondaryType == Equal {
			parser.eatLastToken()
			enum.Values = append(enum.Values, parser.parseExpression())
		} else {
			enum.Values = append(enum.Values, nil)
		}
	}

	parser.expect(RightCurlyBrace, SecondaryNullType)
	parser.eatLastToken()

	return enum
}

func (parser *Parser) parseExpressionArray() []Expression {
	exprs := []Expression{}
	exprs = append(exprs, parser.parseExpression())

	for token := parser.ReadToken(); token.PrimaryType == Comma; token = parser.ReadToken() {
		parser.eatLastToken()
		exprs = append(exprs, parser.parseExpression())
	}

	return exprs
}

// var1, var2, ...varn :[type1, type2, ...typen][= val1, val2, ...valn]
func (parser *Parser) parseDeclaration() Declaration {
	declaration := Declaration{}

	declaration.Identifiers = append(declaration.Identifiers, parser.expect(Identifier, SecondaryNullType))
	parser.eatLastToken()

	for parser.ReadToken().PrimaryType == Comma {
		parser.eatLastToken()

		declaration.Identifiers = append(declaration.Identifiers, parser.expect(Identifier, SecondaryNullType))
		parser.eatLastToken()
	}

	parser.expect(PrimaryNullType, Colon)
	parser.eatLastToken()

	if next := parser.ReadToken(); next.SecondaryType != Equal {
		declaration.Types = parser.parseTypeArray()
	} else {
		declaration.Types = []Type{}
	}

	if next := parser.ReadToken(); next.SecondaryType == Equal {
		parser.eatLastToken()
		declaration.Values = parser.parseExpressionArray()
	}
	return declaration
}

func (parser *Parser) parseIfElse() IfElseBlock {
	ifelseblock := IfElseBlock{}

	statement := parser.parseStatement()

	if parser.ReadToken().PrimaryType == SemiColon {
		parser.eatLastToken()
		ifelseblock.InitStatement = statement
		ifelseblock.HasInitStmt = true
		ifelseblock.Conditions = append(ifelseblock.Conditions, parser.parseExpression())
	} else {
		switch statement.(type) {
		case Expression:
			ifelseblock.Conditions = append(ifelseblock.Conditions, statement.(Expression))
		default:
			// Error: expected an expression, got {statement}
		}
	}

	ifelseblock.Blocks = append(ifelseblock.Blocks, parser.parseBlock())

	for token := parser.ReadToken(); token.PrimaryType == ElseKeyword; token = parser.ReadToken() {
		parser.eatLastToken()
		if next := parser.ReadToken(); next.PrimaryType == IfKeyword {
			parser.eatLastToken()
			ifelseblock.Conditions = append(ifelseblock.Conditions, parser.parseExpression())
			ifelseblock.Blocks = append(ifelseblock.Blocks, parser.parseBlock())
		} else {
			ifelseblock.ElseBlock = parser.parseBlock()
		}
	}

	return ifelseblock
}

func (parser *Parser) parseLoop() Loop {
	loop := Loop{}
	loop.Type = 0

	if parser.ReadToken().PrimaryType == LeftCurlyBrace {
		loop.Type = loop.Type | NoneLoop

		loop.Block = parser.parseBlock()
		return loop
	}

	statement := parser.parseStatement()

	if parser.ReadToken().PrimaryType == SemiColon {
		parser.eatLastToken()

		loop.Type = InitLoop
		loop.InitStatement = statement

		st := parser.parseStatement()

		switch st.(type) {
		case Expression:
			loop.Condition = st.(Expression)
			loop.Type = loop.Type | CondLoop
		case NullStatement:
			break
		default:
			// Error: expected expression, got {st}
		}

		if parser.ReadToken().PrimaryType == SemiColon {
			parser.eatLastToken()
		}
		if parser.ReadToken().PrimaryType == LeftCurlyBrace {
			loop.Block = parser.parseBlock()
			return loop
		}

		loop.LoopStatement = parser.parseStatement()
		loop.Type = loop.Type | LoopLoop
	} else {
		switch statement.(type) {
		case Expression:
			loop.Type = CondLoop
			loop.Condition = statement.(Expression)
		default:
			// Error: expected an expression, got {statement}
		}
	}

	loop.Block = parser.parseBlock()
	return loop
}

func (parser *Parser) parseSwitch() Switch {
	swtch := Switch{}

	if parser.ReadToken().PrimaryType != LeftCurlyBrace {
		statement := parser.parseStatement()

		if parser.ReadToken().PrimaryType == SemiColon {
			parser.eatLastToken()
			swtch.InitStatement = statement

			if parser.ReadToken().PrimaryType != LeftCurlyBrace {
				statement2 := parser.parseStatement()

				switch statement2.(type) {
				case Expression:
					swtch.Type = InitCondSwitch
					swtch.Expr = statement2.(Expression)
				default:
					// Error: Expected an expression, got {statement2}
				}
			}
		} else {
			switch statement.(type) {
			case Expression:
				swtch.Type = CondSwitch
				swtch.Expr = statement.(Expression)
			default:
				// Error: expected an expression, got {statement}
			}
		}
		parser.expect(LeftCurlyBrace, SecondaryNullType)
	} else {
		swtch.Type = NoneSwtch
	}

	parser.eatLastToken()

	for parser.ReadToken().PrimaryType == CaseKeyword {
		parser.eatLastToken()

		Case := CaseStruct{}
		Case.Condition = parser.parseExpression()

		parser.expect(PrimaryNullType, Colon)
		parser.eatLastToken()

		for token := parser.ReadToken(); token.PrimaryType != CaseKeyword && token.PrimaryType != DefaultKeyword; token = parser.ReadToken() {
			switch token.PrimaryType {
			case SemiColon:
				parser.eatLastToken()
			case RightCurlyBrace:
				swtch.Cases = append(swtch.Cases, Case)
				parser.eatLastToken()
				return swtch
			default:
				Case.Block.Statements = append(Case.Block.Statements, parser.parseStatement())
			}
		}
		swtch.Cases = append(swtch.Cases, Case)
	}
	if parser.ReadToken().PrimaryType == DefaultKeyword {
		parser.eatLastToken()
		parser.expect(PrimaryNullType, Colon)
		parser.eatLastToken()

		DefaultCase := Block{}
		swtch.HasDefaultCase = true

		for token := parser.ReadToken(); token.PrimaryType != CaseKeyword; token = parser.ReadToken() {
			switch token.PrimaryType {
			case SemiColon:
				parser.eatLastToken()
			case RightCurlyBrace:
				swtch.DefaultCase = DefaultCase
				parser.eatLastToken()
				return swtch
			default:
				DefaultCase.Statements = append(DefaultCase.Statements, parser.parseStatement())
			}
		}
	}
	return swtch
}

func (parser *Parser) parseBlock() Block {
	block := Block{}

	parser.expect(LeftCurlyBrace, SecondaryNullType)
	parser.eatLastToken()

	for token := parser.ReadToken(); token.PrimaryType != RightCurlyBrace; token = parser.ReadToken() {
		block.Statements = append(block.Statements, parser.parseStatement())
		if parser.ReadToken().PrimaryType == SemiColon {
			parser.eatLastToken()
		}
	}

	parser.eatLastToken()
	return block
}

func (parser *Parser) parseReturn() Return {
	return Return{Values: parser.parseExpressionArray()}
}

func (parser *Parser) parseStatement() Statement {
	var st Statement = NullStatement{}

	switch parser.ReadToken().PrimaryType {
	case IfKeyword:
		parser.eatLastToken()
		return parser.parseIfElse()
	case SwitchKeyword:
		parser.eatLastToken()
		return parser.parseSwitch()
	case ForKeyword:
		parser.eatLastToken()
		return parser.parseLoop()
	case DeferKeyword:
		parser.eatLastToken()
		st = parser.parseDefer()
	case LeftCurlyBrace:
		st = parser.parseBlock()
	case ReturnKeyword:
		parser.eatLastToken()
		st = parser.parseReturn()
	case BreakKeyword:
		parser.eatLastToken()
		st = Break{}
	case ContinueKeyword:
		parser.eatLastToken()
		st = Continue{}
	default:
		parser.fork(0)
		expr := parser.parseExpression()

		if token := parser.ReadToken(); token.PrimaryType == AssignmentOperator {
			parser.moveToFork(0)
			st = parser.parseAssignment()
		} else if token.SecondaryType == Colon {
			parser.moveToFork(0)
			st = parser.parseDeclaration()
		} else if token.PrimaryType == Comma {
			parser.moveToFork(0)
			st = parser.parseDeclarationOrAssignment()
		} else {
			st = expr
			if parser.ReadToken().PrimaryType == SemiColon {
				parser.eatLastToken()
			}
		}

		return st
	}

	if parser.ReadToken().PrimaryType == SemiColon {
		parser.eatLastToken()
	}
	return st
}

func (parser *Parser) parseDefer() Defer {
	return Defer{Stmt: parser.parseStatement()}
}

func (parser *Parser) parseAssignment() Assignment {
	as := Assignment{}

	as.Variables = parser.parseExpressionArray()

	parser.expect(AssignmentOperator, SecondaryNullType)
	as.Op = parser.ReadToken()
	parser.eatLastToken()

	if as.Op.SecondaryType != AddAdd && as.Op.SecondaryType != SubSub {
		as.Values = parser.parseExpressionArray()
	}
	return as
}

func (parser *Parser) parseDeclarationOrAssignment() Statement {
	parser.fork(1)
	parser.parseExpressionArray()

	if token := parser.ReadToken(); token.PrimaryType == AssignmentOperator {
		parser.moveToFork(1)
		return parser.parseAssignment()
	} else if token.SecondaryType == Colon {
		parser.moveToFork(1)
		return parser.parseDeclaration()
	}

	return Declaration{}
}

func (parser *Parser) parseCompoundLiteral() CompoundLiteralData {
	parser.fork(2)

	state := 0
	cl := CompoundLiteralData{}
	c := true

	for next := parser.ReadToken(); c; next = parser.ReadToken() {
		switch state {
		case 0:
			if next.PrimaryType == Identifier {
				parser.eatLastToken()
				state = 1
			} else {
				state = 2
			}
		case 1:
			if next.SecondaryType == Colon {
				parser.moveToFork(2)
				state = 3
			} else {
				parser.moveToFork(2)
				state = 2
			}
		case 2:
			cl.Values = parser.parseExpressionArray()
			c = false
		case 3:
			if next.PrimaryType == RightCurlyBrace {
				c = false
				break
			}
			cl.Fields = append(cl.Fields, parser.expect(Identifier, SecondaryNullType))
			parser.eatLastToken()

			parser.expect(PrimaryNullType, Colon)
			parser.eatLastToken()

			cl.Values = append(cl.Values, parser.parseExpr(0))

			if parser.ReadToken().PrimaryType == RightCurlyBrace {
				c = false
				break
			}

			parser.expect(Comma, SecondaryNullType)
			parser.eatLastToken()
		}
	}

	parser.expect(RightCurlyBrace, SecondaryNullType)
	parser.eatLastToken()

	return cl
}

func (parser *Parser) parseExpression() Expression {
	return parser.parseExpr(0)
}

func (parser *Parser) parseExpr(state int) Expression {
	switch state {

	case 0: // ternary op
		Cond := parser.parseExpr(1)

		if token := parser.ReadToken(); token.SecondaryType == QuesMark {
			parser.eatLastToken()
			Left := parser.parseExpr(0) // 0 is intentional (https://en.cppreference.com/w/c/language/operator_precedence#cite_ref-3)

			parser.expect(PrimaryNullType, Colon)
			parser.eatLastToken()

			return TernaryExpr{Cond: Cond, Left: Left, Right: parser.parseExpr(1)}
		}
		return Cond
	case 1: // Logical And/Or
		Left := parser.parseExpr(2)

		if token := parser.ReadToken(); token.PrimaryType == LogicalOperator {
			parser.eatLastToken()
			return BinaryExpr{Left: Left, Op: token, Right: parser.parseExpr(2)}
		}
		return Left
	case 2: // Bitwise And/Or/Xor
		Left := parser.parseExpr(3)

		if token := parser.ReadToken(); token.SecondaryType == Or || token.SecondaryType == And || token.SecondaryType == ExclusiveOr {
			parser.eatLastToken()
			return BinaryExpr{Left: Left, Op: token, Right: parser.parseExpr(3)}
		}
		return Left
	case 3: // Relational Equal/Not equal
		Left := parser.parseExpr(4)

		if token := parser.ReadToken(); token.SecondaryType == EqualEqual || token.SecondaryType == NotEqual {
			parser.eatLastToken()
			return BinaryExpr{Left: Left, Op: token, Right: parser.parseExpr(4)}
		}
		return Left
	case 4: // Relational Greater/Less/Greater or equal/Less or equal
		Left := parser.parseExpr(5)

		if token := parser.ReadToken(); token.SecondaryType == Greater || token.SecondaryType == Less || token.SecondaryType == LessEqual || token.SecondaryType == GreaterEqual {
			parser.eatLastToken()
			return BinaryExpr{Left: Left, Op: token, Right: parser.parseExpr(5)}
		}
		return Left
	case 5: // Bitwise left shift/ right shift
		Left := parser.parseExpr(6)

		if token := parser.ReadToken(); token.SecondaryType == LeftShift || token.SecondaryType == RightShift {
			parser.eatLastToken()
			return BinaryExpr{Left: Left, Op: token, Right: parser.parseExpr(6)}
		}
		return Left
	case 6: // Add/Sub
		Left := parser.parseExpr(7)

		if token := parser.ReadToken(); token.SecondaryType == Add || token.SecondaryType == Sub {
			parser.eatLastToken()
			return BinaryExpr{Left: Left, Op: token, Right: parser.parseExpr(7)}
		}
		return Left
	case 7: // Div/Miv/Mod
		Left := parser.parseExpr(8)
		if token := parser.ReadToken(); token.SecondaryType == Mul || token.SecondaryType == Div || token.SecondaryType == Modulus {
			parser.eatLastToken()
			return BinaryExpr{Left: Left, Op: token, Right: parser.parseExpr(8)}
		}
		return Left
	case 8: // unary */&/+/-/++/--/!/~
		if parser.ReadToken().PrimaryType == NewKeyword {
			parser.eatLastToken()
			return HeapAlloc{parser.parseType(false, false)}
		}
		if token := parser.ReadToken(); token.SecondaryType == Mul || token.SecondaryType == And || token.SecondaryType == Add || token.SecondaryType == Sub || token.SecondaryType == AddAdd || token.SecondaryType == SubSub || token.SecondaryType == Not || token.SecondaryType == BitwiseNot {
			parser.eatLastToken()
			return UnaryExpr{Op: token, Expr: parser.parseExpr(9)}
		}
		return parser.parseExpr(9)
	case 9: // function call, postfix ++/--, members
		var expr Expression
		expr = parser.parseExpr(10)

		for {
			token := parser.ReadToken()

			if token.SecondaryType == AddAdd || token.SecondaryType == SubSub {
				parser.eatLastToken()
				return PostfixUnaryExpr{Op: token, Expr: expr}
			}

			if token.PrimaryType == LeftParen {
				parser.eatLastToken()

				if expr != nil {
					if parser.ReadToken().PrimaryType == RightParen {
						parser.eatLastToken()
						expr = CallExpr{Function: expr, Args: []Expression{}}
						continue
					}
					expr = CallExpr{Function: expr, Args: parser.parseExpressionArray()}

					parser.expect(RightParen, SecondaryNullType)
					parser.eatLastToken()
					continue
				}
				expr = parser.parseExpr(0)
				parser.expect(RightParen, SecondaryNullType)
				parser.eatLastToken()

				if parser.ReadToken().PrimaryType == LeftCurlyBrace {
					parser.eatLastToken()
					expr = CompoundLiteral{Name: expr, Data: parser.parseCompoundLiteral()}
				}
			} else if token.PrimaryType == LeftBrace {
				parser.eatLastToken()
				expr2 := parser.parseExpr(0)

				parser.expect(RightBrace, SecondaryNullType)
				parser.eatLastToken()

				expr = ArrayMemberExpr{Parent: expr, Index: expr2}
			} else if token.SecondaryType == Dot {
				parser.eatLastToken()
				expr2 := parser.parseExpr(0)

				switch expr.(type) {
				case IdentExpr:
					expr = MemberExpr{Base: expr, Expr: expr2}
				case CallExpr:
					expr = MemberExpr{Base: expr, Expr: expr2}
				case MemberExpr:
					expr = MemberExpr{Base: expr, Expr: expr2}
				}
			} else {
				break
			}
		}

		return expr
	case 10: // literals
		token := parser.ReadToken()

		switch token.PrimaryType {
		case FunctionKeyword:
			parser.eatLastToken()
			return parser.parseFunctionExpr()
		case Identifier:
			parser.eatLastToken()
			return IdentExpr{Value: token}
		case StringLiteral:
			parser.eatLastToken()
			return BasicLit{Value: token}
		case CharLiteral:
			parser.eatLastToken()
			return BasicLit{Value: token}
		case NumberLiteral:
			parser.eatLastToken()
			return BasicLit{Value: token}
		}

		return nil
	}

	return nil
}

func (parser *Parser) parseFunctionExpr() FunctionExpression {
	function := FunctionExpression{}
	function.Type = FunctionType(0)

	// check for async/work/inline keyword
	if token := parser.ReadToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	if token := parser.ReadToken(); token.PrimaryType == AsyncKeyword {
		function.Type = function.Type | AsyncFunction
		parser.eatLastToken()
	} else if token.PrimaryType == WorkKeyword {
		function.Type = function.Type | WorkFunction
		parser.eatLastToken()
	} else {
		function.Type = function.Type | OrdFunction
	}

	if token := parser.ReadToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	// parse arguments
	parser.expect(LeftParen, SecondaryNullType)
	parser.eatLastToken()

	function.Args = parser.parseFunctionArgs()

	parser.expect(RightParen, SecondaryNullType)
	parser.eatLastToken()

	// parse return types
	if token := parser.ReadToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()
		function.ReturnTypes = parser.parseTypeArray()

		parser.expect(RightParen, SecondaryNullType)
		parser.eatLastToken()
	} else if token.PrimaryType != LeftCurlyBrace {
		function.ReturnTypes = []Type{parser.parseType(false, false)}
	}

	// parse code block
	function.Block = parser.parseBlock()
	return function
}

func (parser *Parser) parseFunctionArgs() []ArgStruct {
	args := []ArgStruct{}

	if token := parser.ReadToken(); token.PrimaryType == RightParen {
		return args
	}

	args = append(args, parser.parseFunctionArg())

	for token := parser.ReadToken(); token.PrimaryType == Comma; token = parser.ReadToken() {
		parser.eatLastToken()
		args = append(args, parser.parseFunctionArg())
	}
	return args
}

func (parser *Parser) parseFunctionArg() ArgStruct {
	arg := ArgStruct{}

	arg.Identifier = parser.expect(Identifier, SecondaryNullType)
	parser.eatLastToken()

	parser.expect(PrimaryNullType, Colon)
	parser.eatLastToken()

	arg.Type = parser.parseType(false, false)
	return arg
}
