package compiler

import . "parser"

type Compiler struct {
	Buff       []byte
	ScopeCount int
}

func (c *Compiler) append(buff []byte) {
	c.Buff = append(c.Buff, []byte(buff)...)
}

func (c *Compiler) space() {
	c.append([]byte(" "))
}

func (c *Compiler) comma() {
	c.append([]byte(","))
}

func (c *Compiler) semicolon() {
	c.append([]byte(";"))
}

func (c *Compiler) newline() {
	c.append([]byte("\n"))
}

func (c *Compiler) openParen() {
	c.append([]byte("("))
}

func (c *Compiler) closeParen() {
	c.append([]byte(")"))
}

func (c *Compiler) openBrace() {
	c.append([]byte("{"))
}

func (c *Compiler) closeBrace() {
	c.append([]byte("}"))
}

func (c *Compiler) pushScope() {
	c.ScopeCount++
}

func (c *Compiler) popScope() {
	c.ScopeCount--
}

func (c *Compiler) indent() {
	for i := 0; i < c.ScopeCount; i++ {
		c.append([]byte("	"))
	}
}

func (c *Compiler) Statement(stmt Statement) {
	c.indent()

	switch stmt.(type) {
	case Declaration:
		c.declaration(stmt.(Declaration))
	case Return:
		c.rturn(stmt.(Return))
	default:
		c.expression(stmt.(Expression))
		c.semicolon()
		c.newline()
	}
}

/*
func (c *Compiler) statement(stmt StatementStruct) {
	switch stmt.Type {
	case ImportStatement:
		c.imprt(stmt)
	case StructTypedefStatement:
		c.structTypedef(stmt.Struct)
	case EnumTypedefStatement:
		c.enumTypedef(stmt)
	case TupleTypedefStatement:
		c.tupleTypedef(stmt)
	case DeclarationStatement:
		c.declaration(stmt)
	case AssignmentStatement:
		c.assignment(stmt)
	case IfElseStatement:
		c.ifElse(stmt)
	case LoopStatement:
		c.loop(stmt)
	case ReturnStatememt:
		c.retrn(stmt)
	case SwitchStatement:
		c.swtch(stmt)
	}
}

func (c *Compiler) loop(loop LoopStruct) {
	c.append("for(")

	switch loop.Type {
	case InitCondLoop:
		c.statement(loop.InitStatement)
		c.statement(loop.Condition)
		c.statement(loop.LoopStatement)
	case InitCond:
		c.statement(loop.InitStatement)
		c.statement(loop.Condition)
		c.append(";")
	case Cond:
		c.append(";")
		c.statement()
		c.append(";")
	case NoneLoop:
		c.append(";;")
	}

	c.append(")")
	c.block(loop.Block)
}

func (c *Compiler) block(block BlockStruct) {
	c.append("{\n")
	for stmt := range block.Statements {
		c.statement(stmt)
	}
	c.append("\n}")
}

func (c *Compiler) ifElse(ifElse IfElseBlockStruct) {

	if ifElse.InitStatement {
		c.statement(ifElse.InitStatement)
	}

	for condition, i := range ifElse.Conditions {
		c.append("if(")
		c.expression(condition)
		c.append(")")
		c.block(ifElse.Blocks[i])
		c.append("else ")
	}

	c.block(ElseBlock)
}
*/
func (c *Compiler) declaration(dec Declaration) {
	hasValues := len(dec.Values) > 0
	//print(dec.Values[0].(BasicLit).Value.Buff)
	switch len(dec.Types) {
	case 0:
	case 1:
		Type := dec.Types[0]

		switch Type.Type {
		case IdentifierType:
			for i, Var := range dec.Identifiers {
				c.basicTypeNoArray(Type)
				c.append([]byte(" "))
				c.append(Var.Buff)
				if hasValues {
					c.append([]byte(" = "))
					c.expression(dec.Values[i])
				}
				c.semicolon()
				c.newline()
			}
			c.newline()
		case FuncType:
			for i, Var := range dec.Identifiers {

				c.append(dec.Values[i].(FunctionExpression).ReturnTypes[0].Identifier.Buff)
				c.space()
				c.append(Var.Buff)
				c.openParen()

				for _, Arg := range dec.Values[i].(FunctionExpression).Args {
					c.arg(Arg)
				}

				c.closeParen()
				c.block(dec.Values[i].(FunctionExpression).Block)
			}
		}
	}
}

func (c *Compiler) rturn(rtrn Return) {
	c.append([]byte("return"))
	c.space()

	if len(rtrn.Values) > 0 {
		c.expression(rtrn.Values[0])
	}

	c.semicolon()
	c.newline()
}

func (c *Compiler) arg(arg ArgStruct) {
	c.basicTypeNoArray(arg.Type)
	c.space()
	c.identifier(arg.Identifier)
	c.comma()
	c.space()
}

func (c *Compiler) block(block Block) {
	c.openBrace()
	c.newline()
	c.pushScope()
	for _, statement := range block.Statements {
		c.Statement(statement)
	}
	c.popScope()
	c.closeBrace()
}

func (c *Compiler) expression(expr Expression) {

	switch expr.(type) {
	case FunctionCall:
		call := expr.(FunctionCall)
		c.append(call.Name.Buff)
		c.openParen()

		if len(call.Args) > 0 {
			c.expression(call.Args[0])

			for i := 1; i < len(call.Args); i++ {
				c.comma()
				c.space()
				c.expression(call.Args[i])
			}
		}
		c.closeParen()
	default:
		c.append(expr.(BasicLit).Value.Buff)
	}
}

func (c *Compiler) identifier(identifer Token) {
	c.append(identifer.Buff)
}

func (c *Compiler) basicTypeNoArray(Type TypeStruct) {
	c.append(Type.Identifier.Buff)

	for x := byte(0); x < Type.PointerIndex; x++ {
		c.append([]byte("*"))
	}
}

/*
func (c *Compiler) assignment(as AssignmentStruct) {
	c.expression(as.Variable)
	c.operator(as.Op)
	c.expression(as.Value)
}

func (c *Compiler) structTypedef(st StructTypeStruct) {
	c.append("typedef struct {")

	for prop := range st.Props {
		switch prop.Type.Type {
		case IdentifierType:
			c.basicTypeNoArray(prop.Type)
			c.append(prop.Identifier.Buff)
			c.append(";\n")
		}
	}
}
*/
