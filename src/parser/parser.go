package parser

type Parser struct {
	lexer        Lexer
	lastToken    Token
	currentToken Token
}

func (parser *Parser) readToken() Token {
	return parser.currentToken
}

func (parser *Parser) eatLastToken() {
	parser.lastToken = parser.currentToken
	parser.currentToken = parser.lexer.NextToken()
}

func (parser *Parser) parseGlobalStatement() StatementStruct {
	var statement StatementStruct

	if token := parser.readToken(); token.PrimaryType == ImportKeyword {
		parser.eatLastToken()
		statement = parser.parseImport()
	} else if token.PrimaryType == StructKeyword {
		parser.eatLastToken()
		statement = parser.parseStructTypedef()
	} else if token.PrimaryType == EnumKeyword {
		parser.eatLastToken()
		statement = parser.parseEnumTypedef()
	} else if token.PrimaryType == TupleKeyword {
		parser.eatLastToken()
		statement = parser.parseTupleTypedef()
	} else if token.PrimaryType == Identifier {
		statement = parser.parseDeclaration()
	} else {
		// Error: Invalid token {token}
	}

	if token := parser.readToken(); token.PrimaryType != SemiColon {
		// Error: Expected ';', got {token}
	}

	return StatementStruct{}
}

func (parser *Parser) parseStructTypedef() StatementStruct {
	return StatementStruct{Type: StructTypedefStatement, Strct: parser.parseStructType(false)}
}

func (parser *Parser) parseTupleTypedef() StatementStruct {
	return StatementStruct{Type: TupleTypedefStatement, Tupl: parser.parseTupleType(false)}
}

func (parser *Parser) parseEnumTypedef() StatementStruct {
	return StatementStruct{Type: EnumTypedefStatement, Enum: parser.parseEnumType()}
}

func (parser *Parser) parseFunctionExpression() FunctionExpressionStruct {
	function := FunctionExpressionStruct{}
	function.Type = FunctionType(0)

	// check for async/work/inline keyword
	if token := parser.readToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	if token := parser.readToken(); token.PrimaryType == AsyncKeyword {
		function.Type = function.Type | AsyncFunction
		parser.eatLastToken()
	} else if token.PrimaryType == WorkKeyword {
		function.Type = function.Type | WorkFunction
		parser.eatLastToken()
	} else {
		function.Type = OrdFunction
	}

	if token := parser.readToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	// parse arguments
	if token := parser.readToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()
	} else {
		// Error: expected '(' got {token}
	}

	function.Args = parser.parseFunctionArgs()

	if parser.readToken().PrimaryType == RightParen {
		parser.eatLastToken()
	} else {
		// Error: Expected ')' got {token}
	}

	// parse return types
	if token := parser.readToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()

		function.ReturnTypes = parser.parseTypeArray()

		if parser.readToken().PrimaryType == RightParen {
			parser.eatLastToken()
		} else {
			// Error: Expected ')' got {token} at {token.Line}:{token.Column}
		}
	} else if token.PrimaryType != LeftCurlyBrace {
		function.ReturnTypes = []TypeStruct{parser.parseType(false, false)}
	}

	// parse code block
	function.Block = parser.parseBlock()
	return function
}

func (parser *Parser) parseFunctionArgs() []ArgStruct {
	args := []ArgStruct{}
	args = append(args, parser.parseFunctionArg())
	for token := parser.readToken(); token.PrimaryType == Comma; parser.eatLastToken() {
		args = append(args, parser.parseFunctionArg())
	}
	return args
}

func (parser *Parser) parseFunctionArg() ArgStruct {
	arg := ArgStruct{}

	if token := parser.readToken(); token.PrimaryType == Identifier {
		arg.Identifier = token
		parser.eatLastToken()
	} else {
		// Error: expected identifier got {token}
	}

	if token := parser.readToken(); token.SecondaryType == Colon {
		parser.eatLastToken()
	} else {
		// Error: Expected colon, got {token}
	}

	arg.Type = parser.parseType(false, false)
	return arg
}

func (parser *Parser) parseType(allowTypeDefs bool, alllowUnnamed bool) TypeStruct {
	var pointerIndex byte = 0
	var typ TypeStruct

	for token := parser.readToken(); token.SecondaryType == And; pointerIndex++ {
		parser.eatLastToken()
	}

	if token := parser.readToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()
	}

	if token := parser.readToken(); token.PrimaryType == StructKeyword && allowTypeDefs {
		parser.eatLastToken()
		structType := parser.parseStructType(alllowUnnamed)
		typ.Type = StructType
		typ.StructType = structType
	} else if token.PrimaryType == TupleKeyword && allowTypeDefs {
		parser.eatLastToken()
		tupleType := parser.parseTupleType(alllowUnnamed)
		typ.Type = TupleType
		typ.TupleType = tupleType
	} else if token.PrimaryType == FunctionKeyword {
		parser.eatLastToken()
		funcType := parser.parseFunctionType()
		typ.Type = FuncType
		typ.FuncType = funcType
	} else if token.PrimaryType == Identifier {
		typ.Identifier = token
		typ.Type = IdentifierType
	}

	typ.PointerIndex = pointerIndex
	return typ
}

func (parser *Parser) parseTypeArray() []TypeStruct {
	types := []TypeStruct{}
	types = append(types, parser.parseType(false, false))

	for token := parser.readToken(); token.PrimaryType == Comma; parser.eatLastToken() {
		types = append(types, parser.parseType(false, false))
	}

	return types
}

func (parser *Parser) parseFunctionType() FunctionTypeStruct {
	function := FunctionTypeStruct{}
	function.Type = FunctionType(0)

	// check for async/work/inline keyword
	if token := parser.readToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	if token := parser.readToken(); token.PrimaryType == AsyncKeyword {
		function.Type = function.Type | AsyncFunction
		parser.eatLastToken()
	} else if token.PrimaryType == WorkKeyword {
		function.Type = function.Type | WorkFunction
		parser.eatLastToken()
	} else {
		function.Type = OrdFunction
	}

	if token := parser.readToken(); token.PrimaryType == InlineKeyword {
		function.Type = function.Type | InlineFunction
		parser.eatLastToken()
	}

	// parse arguments
	if token := parser.readToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()
	} else {
		// Error: expected '(' got {token}
	}

	function.Args = parser.parseTypeArray()

	if parser.readToken().PrimaryType == RightParen {
		parser.eatLastToken()
	} else {
		// Error: Expected ')' got {token}
	}

	// parse return types
	if token := parser.readToken(); token.PrimaryType == LeftParen {
		parser.eatLastToken()

		function.ReturnTypes = parser.parseTypeArray()

		if parser.readToken().PrimaryType == RightParen {
			parser.eatLastToken()
		} else {
			// Error: Expected ')' got {token} at {token.Line}:{token.Column}
		}
	} else if token.PrimaryType != Comma && token.PrimaryType != SemiColon && token.SecondaryType != Equal {
		function.ReturnTypes = []TypeStruct{parser.parseType(false, false)}
	}

	return function
}

func (parser *Parser) parseStructType(allowUnnamed bool) StructTypeStruct {
	strct := StructTypeStruct{}

	if token := parser.readToken(); token.PrimaryType == Identifier {
		parser.eatLastToken()
		strct.Identifier = token
	} else if !allowUnnamed {
		// Error: expected identifier, got {token}
	}

	if token := parser.readToken(); token.PrimaryType == LeftCurlyBrace {
		parser.eatLastToken()
	} else {
		// Error: Expected '{', got {token}
	}

	strct.Props = parser.parseStructProps()

	if token := parser.readToken(); token.PrimaryType == RightCurlyBrace {
		parser.eatLastToken()
	} else {
		// Error: expected '}', got {token}
	}

	return strct
}

func (parser *Parser) parseTupleType(allowUnnamed bool) TupleTypeStruct {
	tupl := TupleTypeStruct{}

	if token := parser.readToken(); token.PrimaryType == Identifier {
		parser.eatLastToken()
		tupl.Identifier = token
	} else if !allowUnnamed {
		// Error: expected identifier, got {token}
	}

	if token := parser.readToken(); token.PrimaryType == LeftCurlyBrace {
		parser.eatLastToken()
	} else {
		// Error: Expected '{', got {token}
	}

	tupl.Types = parser.parseTypeArray()

	if token := parser.readToken(); token.PrimaryType == RightCurlyBrace {
		parser.eatLastToken()
	} else {
		// Error: Expected '}' got {token}
	}

	return tupl
}

func (parser *Parser) parseEnumType() EnumTypeStruct {
	enum := EnumTypeStruct{}

	if token := parser.readToken(); token.PrimaryType == Identifier {
		parser.eatLastToken()
		enum.Identifier = token
	} else {
		// Error: expected identifier, got {token}
	}

	if token := parser.readToken(); token.PrimaryType == LeftCurlyBrace {
		parser.eatLastToken()
	} else {
		// Error: expected '}', got {token}
	}

	for parser.readToken().PrimaryType == Comma {
		parser.eatLastToken()

		if token := parser.readToken(); token.PrimaryType == Identifier {
			parser.eatLastToken()
			enum.Identifiers = append(enum.Identifiers, token)
		} else {
			// Error: epxected identifier, got {token}
		}
	}

	if token := parser.readToken(); token.PrimaryType == RightCurlyBrace {
		parser.eatLastToken()
	} else {
		// Error: expected '}', got {token}
	}

	return enum
}

func (parser *Parser) parseStructProps() []StructPropStruct {
	props := []StructPropStruct{}
	props = append(props, parser.parseStructProp())

	for token := parser.readToken(); token.PrimaryType == SemiColon; parser.eatLastToken() {
		props = append(props, parser.parseStructProp())
	}
	return props
}

func (parser *Parser) parseStructProp() StructPropStruct {
	prop := StructPropStruct{}

	if token := parser.readToken(); token.SecondaryType == DotDot {
		parser.eatLastToken()

		if next := parser.readToken(); next.PrimaryType == Identifier {
			prop.Identifier = next
			return prop
		}
	} else if token.PrimaryType == Identifier {
		prop.Identifier = token
		prop.Type = parser.parseType(true, true)

		if prop.Type.Type != StructType && prop.Type.Type != TupleType && parser.readToken().SecondaryType == Equal {
			parser.eatLastToken()
			prop.Value = parser.parseExpression()
		}
	} else {
		// Error: expected identifier, got {token}
	}

	return prop
}

func (parser *Parser) parseExpressionArray() []ExpressionStruct {
	exprs := []ExpressionStruct{}
	exprs = append(exprs, parser.parseExpression())

	for token := parser.readToken(); token.PrimaryType == Comma; parser.eatLastToken() {
		exprs = append(exprs, parser.parseExpression())
	}

	return exprs
}

// var1, var2, ...varn :[type1, type2, ...typen][= val1, val2, ...valn]
func (parser *Parser) parseDeclaration() StatementStruct {
	declaration := DeclarationStruct{}

	if token := parser.readToken(); token.PrimaryType == Identifier {
		declaration.Identifiers = append(declaration.Identifiers, token)
	} else {
		// Error: Expected identifier, got {token}
	}

	for parser.readToken().PrimaryType == Comma {
		parser.eatLastToken()
		if token := parser.readToken(); token.PrimaryType == Identifier {
			declaration.Identifiers = append(declaration.Identifiers, token)
			parser.eatLastToken()
		} else {
			// Error: expected Identifier, got {token}
		}
	}

	if token := parser.readToken(); token.SecondaryType == Colon {
		parser.eatLastToken()
	} else {
		// Error: Expected ':', got {token}
	}

	if next := parser.readToken(); next.SecondaryType != Equal {
		declaration.Types = parser.parseTypeArray()
	} else {
		declaration.Types = []TypeStruct{}
	}

	if next := parser.readToken(); next.SecondaryType == Equal {
		parser.eatLastToken()
		declaration.Values = parser.parseExpressionArray()
	}

	return StatementStruct{Type: DeclarationStatement, Declaration: declaration}
}

func (parser *Parser) parseIfElse() IfElseBlockStruct {
	ifelseblock := IfElseBlockStruct{}

	statement := parser.parseStatement(false)

	if parser.readToken().PrimaryType == SemiColon {
		parser.eatLastToken()
		ifelseblock.InitStatement = statement
		ifelseblock.Conditions = append(ifelseblock.Conditions, parser.parseExpression())
	} else if statement.Type == ExpressionStatement {
		ifelseblock.Conditions = append(ifelseblock.Conditions, statement.Expression)
	} else {
		// Error: expected an expression, got {statement}
	}

	ifelseblock.Blocks = append(ifelseblock.Blocks, parser.parseBlock())

	for token := parser.readToken(); token.PrimaryType == ElseKeyword; parser.eatLastToken() {
		if next := parser.readToken(); next.PrimaryType == IfKeyword {
			ifelseblock.Conditions = append(ifelseblock.Conditions, parser.parseExpression())
			ifelseblock.Blocks = append(ifelseblock.Blocks, parser.parseBlock())
		} else {
			ifelseblock.ElseBlock = parser.parseBlock()
		}
	}

	return ifelseblock
}

func (parser *Parser) parseLoop() LoopStruct {
	loop := LoopStruct{}

	if parser.readToken().PrimaryType == LeftCurlyBrace {
		loop.Type = NoneLoop
	} else {
		statement := parser.parseStatement(false)

		if parser.readToken().PrimaryType == SemiColon {
			parser.eatLastToken()

			loop.InitStatement = statement
			loop.Condition = parser.parseExpression()

			if parser.readToken().PrimaryType == SemiColon {
				parser.eatLastToken()

				if parser.readToken().PrimaryType == LeftCurlyBrace {
					loop.Type = InitCond
				} else {
					loop.LoopStatement = parser.parseStatement(false)
					loop.Type = InitCondLoop
				}
			} else {
				loop.Type = InitCond
			}
		} else {
			if statement.Type == ExpressionStatement {
				loop.Type = Cond
				loop.Condition = statement.Expression
			} else {
				// Error: expected an expression, got {statement}
			}
		}
	}

	loop.Block = parser.parseBlock()
	return loop
}

func (parser *Parser) parseSwitch() SwitchStruct {
	swtch := SwitchStruct{}

	if parser.readToken().PrimaryType != LeftCurlyBrace {
		statement := parser.parseStatement(false)

		if parser.readToken().PrimaryType == SemiColon {
			parser.eatLastToken()
			statement2 := parser.parseStatement(false)
		}
	}

	return swtch
}

func (parser *Parser) parseBlock() BlockStruct {
	block := BlockStruct{}

	if token := parser.readToken(); token.PrimaryType == LeftCurlyBrace {
		parser.eatLastToken()
	} else {
		// Error: expected '{', got {token}
	}

	for token := parser.readToken(); token.PrimaryType != RightCurlyBrace; parser.eatLastToken() {
		block.Statements = append(block.Statements, parser.parseStatement(true))
	}

	return block
}

func (parser *Parser) parseStatement(needSemiColon bool) StatementStruct {
	return StatementStruct{}
}
func (parser *Parser) parseExpression() ExpressionStruct {
	return ExpressionStruct{}
}
