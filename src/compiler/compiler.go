package compiler

import (
	. "parser"
	"strconv"
)

type Compiler struct {
	Buff       []byte
	ScopeCount int
}

func (c *Compiler) append(buff []byte) {
	c.Buff = append(c.Buff, []byte(buff)...)
}

func (c *Compiler) colon() {
	c.append([]byte(":"))
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

func (c *Compiler) openCurlyBrace() {
	c.append([]byte("{"))
}

func (c *Compiler) closeCurlyBrace() {
	c.append([]byte("}"))
}

func (c *Compiler) openBrace() {
	c.append([]byte("["))
}

func (c *Compiler) closeBrace() {
	c.append([]byte("]"))
}

func (c *Compiler) dot() {
	c.append([]byte("."))
}

func (c *Compiler) equal() {
	c.append([]byte("="))
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
	c.newline()
	switch stmt.(type) {
	case Declaration:
		c.declaration(stmt.(Declaration))
	case Return:
		c.rturn(stmt.(Return))
	case IfElseBlock:
		c.ifElse(stmt.(IfElseBlock))
	case Loop:
		c.loop(stmt.(Loop))
	case Assignment:
		c.assignment(stmt.(Assignment))
	case Switch:
		c.swtch(stmt.(Switch))
	case Struct:
		c.structTypedef(stmt.(Struct))
	case Enum:
		c.enumTypedef(stmt.(Enum))
	case Tuple:
		c.tupleTypedef(stmt.(Tuple))
	case Break:
		c.indent()
		c.append([]byte("break;"))
	case Continue:
		c.indent()
		c.append([]byte("continue;"))
	case NullStatement:
		c.semicolon()
	default:
		c.indent()
		c.expression(stmt.(Expression))
		c.semicolon()
	}
}

func (c *Compiler) loop(loop Loop) {

	if loop.Type&InitLoop == InitLoop {
		c.indent()
		c.openCurlyBrace()
		c.pushScope()
		c.Statement(loop.InitStatement)
	}

	c.indent()
	c.append([]byte("while"))
	c.openParen()

	if loop.Type&CondLoop == CondLoop {
		c.expression(loop.Condition)
	} else {
		c.append([]byte("1"))
	}

	c.closeParen()

	if loop.Type&LoopLoop == LoopLoop {
		loop.Block.Statements = append(loop.Block.Statements, loop.LoopStatement)
	}

	c.block(loop.Block)

	if loop.Type&InitLoop == InitLoop {
		c.popScope()
		c.newline()
		c.indent()
		c.closeCurlyBrace()
	}
}

func (c *Compiler) declaration(dec Declaration) {
	hasValues := len(dec.Values) > 0

	switch len(dec.Types) {
	case 0:
	case 1:
		Type := dec.Types[0]

		switch Type.Type {
		case IdentifierType:
			for i, Var := range dec.Identifiers {
				c.indent()
				c.basicTypeNoArray(Type)
				c.append([]byte(" "))
				c.append(Var.Buff)
				if hasValues {
					c.space()
					c.equal()
					c.space()
					c.expression(dec.Values[i])
				}
				c.semicolon()
				c.newline()
			}
		case FuncType:
			for i, Var := range dec.Identifiers {
				c.indent()
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
		return
	}

	for i, Var := range dec.Identifiers {
		Type := dec.Types[i]

		switch Type.Type {
		case IdentifierType:
			c.indent()
			c.basicTypeNoArray(Type)
			c.space()
			c.append(Var.Buff)

			if hasValues {
				c.space()
				c.equal()
				c.space()
				c.expression(dec.Values[i])
			}

			c.semicolon()
			c.newline()

		case FuncType:
			c.indent()
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

func (c *Compiler) rturn(rtrn Return) {
	c.indent()
	c.append([]byte("return"))
	c.space()

	if len(rtrn.Values) > 0 {
		c.expression(rtrn.Values[0])
	}

	c.semicolon()
}

func (c *Compiler) arg(arg ArgStruct) {
	c.basicTypeNoArray(arg.Type)
	c.space()
	c.identifier(arg.Identifier)
	c.comma()
	c.space()
}

func (c *Compiler) block(block Block) {
	c.openCurlyBrace()
	c.pushScope()
	for _, statement := range block.Statements {
		c.Statement(statement)
	}
	c.popScope()
	c.newline()
	c.indent()
	c.closeCurlyBrace()
}

func (c *Compiler) expression(expr Expression) {

	switch expr.(type) {
	case CallExpr:
		c.functionCall(expr.(CallExpr))
	case BasicLit:
		c.append(expr.(BasicLit).Value.Buff)
	case IdentExpr:
		c.append(expr.(IdentExpr).Value.Buff)
	case BinaryExpr:
		switch expr.(BinaryExpr).Left.(type) {
		case BasicLit:
			c.expression(expr.(BinaryExpr).Left)
		case IdentExpr:
			c.expression(expr.(BinaryExpr).Left)
		default:
			c.openParen()
			c.expression(expr.(BinaryExpr).Left)
			c.closeParen()
		}

		c.operator(expr.(BinaryExpr).Op)

		switch expr.(BinaryExpr).Right.(type) {
		case BasicLit:
			c.expression(expr.(BinaryExpr).Right)
		case IdentExpr:
			c.expression(expr.(BinaryExpr).Right)
		default:
			c.openParen()
			c.expression(expr.(BinaryExpr).Right)
			c.closeParen()
		}
	case UnaryExpr:
		c.operator(expr.(UnaryExpr).Op)

		switch expr.(UnaryExpr).Expr.(type) {
		case BasicLit:
			c.expression(expr.(UnaryExpr).Expr)
		case IdentExpr:
			c.expression(expr.(UnaryExpr).Expr)
		default:
			c.openParen()
			c.expression(expr.(UnaryExpr).Expr)
			c.closeParen()
		}
	case PostfixUnaryExpr:
		switch expr.(PostfixUnaryExpr).Expr.(type) {
		case BasicLit:
			c.expression(expr.(PostfixUnaryExpr).Expr)
		case IdentExpr:
			c.expression(expr.(PostfixUnaryExpr).Expr)
		default:
			c.openParen()
			c.expression(expr.(PostfixUnaryExpr).Expr)
			c.closeParen()
		}
		c.operator(expr.(PostfixUnaryExpr).Op)
	case ArrayMemberExpr:
		switch expr.(ArrayMemberExpr).Parent.(type) {
		case MemberExpr:
			c.expression(expr.(ArrayMemberExpr).Parent)
		case IdentExpr:
			c.expression(expr.(ArrayMemberExpr).Parent)
		default:
			c.openParen()
			c.expression(expr.(ArrayMemberExpr).Parent)
			c.closeParen()
		}
		c.openBrace()
		c.expression(expr.(ArrayMemberExpr).Index)
		c.closeBrace()
	case MemberExpr:
		c.expression(expr.(MemberExpr).Base)
		c.append([]byte("."))
		c.expression(expr.(MemberExpr).Expr)
	case TernaryExpr:
		c.expression(expr.(TernaryExpr).Cond)
		c.space()
		c.append([]byte("?"))
		c.space()
		c.expression(expr.(TernaryExpr).Left)
		c.space()
		c.colon()
		c.space()
		c.expression(expr.(TernaryExpr).Right)
	case CompoundLiteral:
		c.openParen()
		c.expression(expr.(CompoundLiteral).Name)
		c.closeParen()

		c.openCurlyBrace()
		if len(expr.(CompoundLiteral).Data.Fields) > 0 {
			for i, field := range expr.(CompoundLiteral).Data.Fields {
				c.dot()
				c.append(field.Buff)
				c.space()
				c.equal()
				c.space()
				c.expression(expr.(CompoundLiteral).Data.Values[i])
				c.comma()
				c.space()
			}
		} else if len(expr.(CompoundLiteral).Data.Values) > 0 {
			for _, val := range expr.(CompoundLiteral).Data.Values {
				c.expression(val)
				c.comma()
				c.space()
			}
		}
		c.closeCurlyBrace()
	}
}

func (c *Compiler) functionCall(call CallExpr) {
	c.expression(call.Function)
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

func (c *Compiler) ifElse(ifElse IfElseBlock) {

	if ifElse.HasInitStmt {
		c.indent()
		c.openCurlyBrace()
		c.pushScope()
		c.Statement(ifElse.InitStatement)
	}

	c.indent()
	for i, condition := range ifElse.Conditions {
		c.append([]byte("if"))
		c.openParen()
		c.expression(condition)
		c.closeParen()
		c.block(ifElse.Blocks[i])
		c.append([]byte(" else "))
	}

	c.block(ifElse.ElseBlock)

	if ifElse.HasInitStmt {
		c.popScope()
		c.newline()
		c.indent()
		c.closeCurlyBrace()
	}
}

func (c *Compiler) assignment(as Assignment) {

	for i, Var := range as.Variables {
		c.indent()
		c.expression(Var)

		//if as.Op.SecondaryType != AddAdd && as.Op.SecondaryType != SubSub {
		c.space()
		c.operator(as.Op)
		c.space()
		if len(as.Values) > 1 {
			c.expression(as.Values[i])
		} else {
			c.expression(as.Values[0])
		}
		//} else {
		//	c.operator(as.Op)
		//}

		c.semicolon()
		c.newline()
	}
}

func (c *Compiler) swtch(swtch Switch) {
	if swtch.Type == InitCondSwitch {
		c.indent()
		c.openCurlyBrace()
		c.pushScope()
		c.Statement(swtch.InitStatement)
	}

	c.indent()
	c.append([]byte("switch"))

	c.openParen()
	if swtch.Type == NoneSwtch {
		c.append([]byte("1"))
	} else {
		c.expression(swtch.Condition)
	}
	c.closeParen()
	c.openCurlyBrace()

	for _, Case := range swtch.Cases {
		c.newline()
		c.indent()
		c.append([]byte("case"))
		c.space()
		c.expression(Case.Condition)
		c.colon()

		c.pushScope()
		for _, stmt := range Case.Statements {
			c.Statement(stmt)
		}
		c.popScope()
	}

	if swtch.HasDefaultCase {
		c.newline()
		c.indent()
		c.append([]byte("default"))
		c.colon()

		c.pushScope()
		for _, stmt := range swtch.DefaultCase.Statements {
			c.Statement(stmt)
		}
		c.popScope()
	}

	c.newline()
	c.indent()
	c.closeCurlyBrace()

	if swtch.Type == InitCondSwitch {
		c.popScope()
		c.newline()
		c.indent()
		c.closeCurlyBrace()
	}
}

func (c *Compiler) operator(op Token) {
	c.append(op.Buff)
}

func (c *Compiler) structTypedef(st Struct) {
	c.append([]byte("typedef struct {"))
	c.newline()
	c.pushScope()

	for _, prop := range st.Props {
		c.declaration(prop)
	}

	c.popScope()
	c.closeCurlyBrace()
	c.space()
	c.append(st.Identifier.Buff)
	c.semicolon()
	c.newline()
}

func (c *Compiler) enumTypedef(en Enum) {
	c.append([]byte("typedef enum {"))
	c.newline()
	c.pushScope()

	for x, prop := range en.Identifiers {
		c.indent()
		c.append(prop.Buff)
		val := en.Values[x]

		if val != nil {
			c.space()
			c.equal()
			c.space()
			c.expression(val)
		}
		c.comma()
		c.newline()
	}

	c.popScope()
	c.closeCurlyBrace()
	c.space()
	c.append(en.Name.Buff)
	c.semicolon()
	c.newline()
}

func (c *Compiler) tupleTypedef(en Tuple) {
	c.append([]byte("typedef struct {"))
	c.newline()
	c.pushScope()

	for x, prop := range en.Types {
		c.indent()
		c.basicTypeNoArray(prop)
		c.space()

		c.append([]byte("_" + strconv.Itoa(x)))

		c.semicolon()
		c.newline()
	}

	c.popScope()
	c.closeCurlyBrace()
	c.space()
	c.append(en.Identifier.Buff)
	c.semicolon()
	c.newline()
}
