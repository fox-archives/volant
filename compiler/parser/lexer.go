package parser

//Lexer does stuff
type Lexer struct {
	Buffer   []byte // code read from some file or something
	Position int    // position of char to process next in buffer

	// for error messages and all
	Line   int // line count of the current char in code
	Column int // column count of the current char in the current line
}

func (lexer Lexer) readToBuffer() byte {
	// it'll probably read few bytes from a file or command line or whatever
	lexer.Buffer = append(lexer.Buffer, 'a') // just for now
	return 1                                 // 0 for eof
}

func (lexer Lexer) nextChar() byte {
	// if no more characters to read in the buffer
	if len(lexer.Buffer) == lexer.Position {
		// read few bytes from the source file and push it to buffer
		// return 0 if eof reached
		if lexer.readToBuffer() == 0 {
			return 0 // marks eof (tells the Lexer to stop lexing)
		}
	}

	// its reached only if the buffer has more bytes to read
	character := lexer.Buffer[lexer.Position]

	lexer.Position++
	lexer.Column++

	return character
}

// returns next character in the buffer without incrementing the position counter
// for times when the code may need to fallback to previous character if the assumption about returned character turns out to be false
func (lexer Lexer) peek() byte {
	// if no more characters to read in the buffer
	if len(lexer.Buffer) == lexer.Position {
		// read few bytes from the source file and push it to buffer
		// return 0 if eof reached
		if lexer.readToBuffer() == 0 {
			return 0 // marks eof (tells the Lexer to stop lexing)
		}
	}

	// its reached only if the buffer has more bytes to read
	character := lexer.Buffer[lexer.Position]

	lexer.Position++
	lexer.Column++

	return character
}

// increment the position count of Lexer when the assumption of next character (made using Lexer.peek()) turns out ot be right and the code doesn't need to fallback to the last character anymore
func (lexer Lexer) eatLastByte() {
	lexer.Position++
	lexer.Column++
}

// increment the line count and zero the column count when a newline is reached
func (lexer Lexer) shiftLine() {
	lexer.Position++
	lexer.Line++
	lexer.Column = 0
}

func (lexer Lexer) skipSpaces() {
	for {
		if next := lexer.peek(); next == '\n' {
			lexer.shiftLine()
		} else if next == ' ' || next == '\t' {
			lexer.eatLastByte()
		} else {
			break
		}
	}
}

//NextToken tokenizes the code one step futher and returns the token
func (lexer Lexer) NextToken() Token {
	lexer.skipSpaces() // skiping spaces/tabs/newlines

	// the control flow will reach the end of function only if `character` is a null byte

	// not sure how the lexing functions will handle errors yet
	if character := lexer.peek(); character == 0 {
		// return eof token (tells the parser to stop further parsing)
		return Token{primaryType: EOF, secondaryType: SecondaryNullType, buff: nil}
	} else if IsIdentifierBegining(character) {
		return lexer.lexWord() // identifier or keyword or macros
	} else if IsNumDec(character) {
		return lexer.lexNumber() // number, hex starting with 0x, octals with 0o, decimals either directly or with 0d, and binary with 0b
	} else if IsStringDelimiter(character) { // string delimiter is "
		lexer.eatLastByte()
		return lexer.lexString()
	} else if IsCharDelimiter(character) { // char delimiter is '
		lexer.eatLastByte()
		return lexer.lexChar() // just a single byte
	} else {
		if op := lexer.lexOperator(); op.secondaryType != NotFound {
			return op
		} else if op = lexer.lexDelimiter(); op.secondaryType != NotFound {
			return op
		}
	}

	return Token{primaryType: ErrorToken, secondaryType: UnknownChar, buff: nil}
}

func (lexer Lexer) lexNumber() Token {
	var radix SecondaryTokenType = DecimalRadix
	num := []byte{}

	if character := lexer.nextChar(); character == '0' {
		// check either the number has a base specified or we have to implictly assume it
		if next := lexer.peek(); IsNumDec(next) { // decimal
			num = append(num, next)
			lexer.eatLastByte()
		} else if next == 'd' { // decimal
			radix = DecimalRadix
			lexer.eatLastByte()
		} else if next == 'b' { // binary
			radix = BinaryRadix
			lexer.eatLastByte()
		} else if next == 'o' { // octal
			radix = OctalRadix
			lexer.eatLastByte()
		} else if next == 'x' { // hexadecimal
			radix = HexadecimalRadix
			lexer.eatLastByte()
		} else {
			num = append(num, '0')
			// don't eatLastByte() here
		}
	} else {
		num = append(num, character)
	}

	switch radix {
	case DecimalRadix:
		for n := lexer.peek(); IsNumDec(n) || n == '.'; n = lexer.peek() {
			num = append(num, n)
			lexer.eatLastByte()
		}
	case BinaryRadix:
		for n := lexer.peek(); IsNumBi(n) || n == '.'; n = lexer.peek() {
			num = append(num, n)
			lexer.eatLastByte()
		}
	case OctalRadix:
		for n := lexer.peek(); IsNumOct(n) || n == '.'; n = lexer.peek() {
			num = append(num, n)
			lexer.eatLastByte()
		}
	case HexadecimalRadix:
		for n := lexer.peek(); IsNumHex(n) || n == '.'; n = lexer.peek() {
			num = append(num, n)
			lexer.eatLastByte()
		}
	}

	return Token{buff: num, primaryType: NumberLiteral, secondaryType: radix}
}

func (lexer Lexer) lexChar() Token {
	character := lexer.nextChar()

	// stores the tokenized char
	var chr [1]byte

	if character == '\\' {
		// lex escape sequences, probably just \t, \n, \r and \'
		switch next := lexer.peek(); next {
		case 't':
			chr[0] = '\t'
		case 'n':
			chr[0] = '\n'
		case 'r':
			chr[0] = '\r'
		case '\'':
			chr[0] = '\''
		case '\\':
			chr[0] = '\\'
		default:
			// idk we can prob throw an error here saying that its an invalid escape sequence or can prob ignore this
			chr[0] = next
		}
	} else {
		chr[0] = character
	}

	if nextChar := lexer.peek(); nextChar == '\'' {
		lexer.eatLastByte() // increament the positon as `nextChar` was `'` as expected
		return Token{primaryType: CharLiteral, secondaryType: SecondaryNullType, buff: chr[:]}
	}

	// Error: Invalid character {nedxtChar} at {Lexer.line}:{Lexer.column}. Expected `'`.
	// print error and all (maybe use the buff property of struct for storing error message?)
	return Token{primaryType: ErrorToken, secondaryType: SecondaryNullType, buff: nil}
}

func (lexer Lexer) lexString() Token {
	str := []byte{}

	for character := lexer.peek(); !IsStringDelimiter(character); character = lexer.peek() {
		switch character {
		case 0:
			// Error: Expected end of string literal, found eof.
			// print error and all
			return Token{primaryType: ErrorToken, secondaryType: SecondaryNullType, buff: nil}
		case '\\':
			lexer.eatLastByte() // eat '\\'

			// idk what more escape sequences can we have
			switch next := lexer.peek(); next {
			case 't':
				str = append(str, '\t')
			case 'n':
				str = append(str, '\n')
			case 'r':
				str = append(str, '\r')
			case '"':
				str = append(str, '"')
			case '\\':
				str = append(str, '\\')
			default:
				// idk maybe throw an error?
				str = append(str, next)
			}
		default:
			str = append(str, character)
		}
		lexer.eatLastByte()
	}
	lexer.eatLastByte() // eat '"'
	return Token{primaryType: StringLitral, secondaryType: SecondaryNullType, buff: str}
}

func (lexer Lexer) lexWord() Token {
	word := []byte{}
	word = append(word, lexer.nextChar())

	for w := lexer.peek(); IsIdentifierPart(w); w = lexer.peek() {
		word = append(word, w)
		lexer.eatLastByte()
	}

	return Token{primaryType: GetWordType(string(word)), secondaryType: SecondaryNullType, buff: word}
}

func (lexer Lexer) lexOperator() Token {
	switch character := lexer.peek(); character {
	case '*':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: AssignmentOperator, secondaryType: MulEqual, buff: nil}
		default:
			return Token{primaryType: AirthmaticOperator, secondaryType: Mul, buff: nil}
		}
	case '/':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: AssignmentOperator, secondaryType: DivEqual, buff: nil}
		default:
			return Token{primaryType: AirthmaticOperator, secondaryType: Div, buff: nil}
		}
	case '%':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: AssignmentOperator, secondaryType: ModulusEqual, buff: nil}
		default:
			return Token{primaryType: AirthmaticOperator, secondaryType: Modulus, buff: nil}
		}
	case '+':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: AssignmentOperator, secondaryType: AddEqual, buff: nil}
		case '+':
			lexer.eatLastByte()
			return Token{primaryType: AirthmaticOperator, secondaryType: AddAdd, buff: nil}
		default:
			return Token{primaryType: AirthmaticOperator, secondaryType: Add, buff: nil}
		}
	case '-':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: AssignmentOperator, secondaryType: SubEqual, buff: nil}
		case '-':
			lexer.eatLastByte()
			return Token{primaryType: AssignmentOperator, secondaryType: SubSub, buff: nil}
		default:
			return Token{primaryType: AirthmaticOperator, secondaryType: Sub, buff: nil}
		}
	case '=':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: RelationalOperator, secondaryType: EqualEqual, buff: nil}
		default:
			return Token{primaryType: AssignmentOperator, secondaryType: Equal, buff: nil}
		}
	case '!':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: RelationalOperator, secondaryType: NotEqual, buff: nil}
		default:
			return Token{primaryType: BitwiseOperator, secondaryType: Not, buff: nil}
		}
	case '>':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: RelationalOperator, secondaryType: GreaterEqual, buff: nil}
		case '>':
			lexer.eatLastByte()
			return Token{primaryType: BitwiseOperator, secondaryType: RightShift, buff: nil}
		default:
			return Token{primaryType: RelationalOperator, secondaryType: Greater, buff: nil}
		}
	case '<':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '=':
			lexer.eatLastByte()
			return Token{primaryType: RelationalOperator, secondaryType: LessEqual, buff: nil}
		case '<':
			lexer.eatLastByte()
			return Token{primaryType: BitwiseOperator, secondaryType: LeftShift, buff: nil}
		default:
			return Token{primaryType: RelationalOperator, secondaryType: Less, buff: nil}
		}
	case '&':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '&':
			lexer.eatLastByte()
			return Token{primaryType: LogicalOperator, secondaryType: AndAnd, buff: nil}
		default:
			return Token{primaryType: BitwiseOperator, secondaryType: And, buff: nil}
		}
	case '|':
		lexer.eatLastByte()
		switch next := lexer.peek(); next {
		case '|':
			lexer.eatLastByte()
			return Token{primaryType: LogicalOperator, secondaryType: OrOr, buff: nil}
		default:
			return Token{primaryType: BitwiseOperator, secondaryType: Or, buff: nil}
		}
	case '^':
		lexer.eatLastByte()
		return Token{primaryType: BitwiseOperator, secondaryType: ExclusiveOr, buff: nil}
	case '.':
		lexer.eatLastByte()
		return Token{primaryType: SpecialOperator, secondaryType: Dot, buff: nil}
	case ':':
		lexer.eatLastByte()
		return Token{primaryType: SpecialOperator, secondaryType: Colon, buff: nil}
	case '?':
		lexer.eatLastByte()
		return Token{primaryType: SpecialOperator, secondaryType: QuesMark, buff: nil}
	}
	return Token{primaryType: ErrorToken, secondaryType: NotFound, buff: nil}
}

func (lexer Lexer) lexDelimiter() Token {
	switch character := lexer.peek(); character {
	case '(':
		lexer.eatLastByte()
		return Token{primaryType: LeftParen, secondaryType: SecondaryNullType, buff: nil}
	case ')':
		lexer.eatLastByte()
		return Token{primaryType: RightParen, secondaryType: SecondaryNullType, buff: nil}
	case '{':
		lexer.eatLastByte()
		return Token{primaryType: LeftCurlyBrace, secondaryType: SecondaryNullType, buff: nil}
	case '}':
		lexer.eatLastByte()
		return Token{primaryType: RightCurlyBrace, secondaryType: SecondaryNullType, buff: nil}
	case '[':
		lexer.eatLastByte()
		return Token{primaryType: LeftBrace, secondaryType: SecondaryNullType, buff: nil}
	case ']':
		lexer.eatLastByte()
		return Token{primaryType: RightBrace, secondaryType: SecondaryNullType, buff: nil}
	}
	return Token{primaryType: ErrorToken, secondaryType: NotFound, buff: nil}
}
