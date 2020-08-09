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

func (c *Compiler) operator(op Token) {
	c.append(op.Buff)
}

func (c *Compiler) identifier(identifer Token) {
	c.append(identifer.Buff)
}

func (c *Compiler) indent() {
	for i := 0; i < c.ScopeCount; i++ {
		c.append([]byte("	"))
	}
}

func (c *Compiler) GlobalStatement(stmt Statement) {
	c.newline()
	switch stmt.(type) {
	case Declaration:
		c.globalDeclaration(stmt.(Declaration))
	case Switch:
		c.swtch(stmt.(Switch))
	case StructTypedef:
		c.structTypedef(stmt.(StructTypedef))
	case EnumTypedef:
		c.enumTypedef(stmt.(EnumTypedef))
	case TupleTypedef:
		c.tupleTypedef(stmt.(TupleTypedef))
	case NullStatement:
		c.semicolon()
	}
}

func (c *Compiler) statement(stmt Statement) {
	c.newline()
	switch stmt.(type) {
	case Declaration:
		c.globalDeclaration(stmt.(Declaration))
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
	case Break:
		c.indent()
		c.append([]byte("break;"))
	case Continue:
		c.indent()
		c.append([]byte("continue;"))
	case NullStatement:
		c.semicolon()
	case Block:
		c.indent()
		c.block(stmt.(Block))
	case Defer:
		c.defr(stmt.(Defer))
	case Delete:
		c.delete(stmt.(Delete))
	default:
		c.indent()
		c.expression(stmt.(Expression))
		c.semicolon()
	}
}

func (c *Compiler) delete(delete Delete) {
	c.indent()
	c.append([]byte("delete"))
	c.openParen()
	c.expression(delete.Expr)
	c.closeParen()
	c.semicolon()
}

func (c *Compiler) defr(defr Defer) {
	c.indent()
	c.append([]byte("defer"))
	c.space()
	c.openCurlyBrace()
	c.pushScope()
	c.statement(defr.Stmt)
	c.newline()
	c.popScope()
	c.indent()
	c.closeCurlyBrace()
	c.semicolon()
}

func (c *Compiler) loop(loop Loop) {

	if loop.Type&InitLoop == InitLoop {
		c.indent()
		c.openCurlyBrace()
		c.pushScope()
		c.statement(loop.InitStatement)
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

func (c *Compiler) globalDeclaration(dec Declaration) {
	hasValues := len(dec.Values) > 0

	for i, Var := range dec.Identifiers {
		Typ := dec.Types[i]

		switch Typ.(type) {
		case FuncType:
			if hasValues {
				Func := dec.Values[i].(FuncExpr)

				c.indent()
				c.declarationType(Func.ReturnTypes[0], Var)
				c.openParen()

				args := Func.Args

				if len(args) > 0 {
					c.declarationType(args[0].Type, args[0].Identifier)

					for i := 1; i < len(args); i++ {
						c.comma()
						c.space()
						c.declarationType(args[i].Type, args[i].Identifier)
					}
				}
				c.closeParen()
				c.block(Func.Block)
				/*
					c.indent()
					c.declarationType(Func.ReturnTypes[0], Var)
					c.space()
					c.openParen()
					c.append([]byte("^"))
					c.identifier(Var)
					c.closeParen()
					c.openParen()

					args := Func.Args

					if len(args) > 0 {
						c.declarationType(args[0].Type, args[0].Identifier)

						for i := 1; i < len(args); i++ {
							c.comma()
							c.space()
							c.declarationType(args[i].Type, args[i].Identifier)
						}
					}
					c.closeParen()
					c.space()
					c.equal()
					c.space()
					c.append([]byte("^"))
					c.Type(Func.ReturnTypes[0])

					c.openParen()

					if len(args) > 0 {
						c.declarationType(args[0].Type, args[0].Identifier)

						for i := 1; i < len(args); i++ {
							c.comma()
							c.space()
							c.declarationType(args[i].Type, args[i].Identifier)
						}
					}
					c.closeParen()
					c.block(Func.Block)
					c.semicolon()
				*/
			}
		default:
			c.indent()
			c.declarationType(Typ, Var)

			if hasValues {
				c.space()
				c.equal()
				c.space()
				c.expression(dec.Values[i])
			}

			c.semicolon()
			c.newline()
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

func (c *Compiler) block(block Block) {
	c.openCurlyBrace()
	c.pushScope()
	for _, statement := range block.Statements {
		c.statement(statement)
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
		c.identifier(expr.(BasicLit).Value)
	case IdentExpr:
		c.identifier(expr.(IdentExpr).Value)
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
		c.openParen()
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
		c.closeParen()
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
				c.identifier(field)
				c.space()
				c.equal()
				c.space()
				c.expression(expr.(CompoundLiteral).Data.Values[i])
				c.comma()
				c.space()
			}
		} else {
			for _, val := range expr.(CompoundLiteral).Data.Values {
				c.expression(val)
				c.comma()
				c.space()
			}
		}
		c.closeCurlyBrace()
	case TypeCast:
		c.openParen()
		c.Type(expr.(TypeCast).Type.(Type))
		c.closeParen()
		c.openParen()
		c.expression(expr.(TypeCast).Expr)
		c.closeParen()
	case HeapAlloc:
		c.heapAlloc(expr.(HeapAlloc))
	case ArrayLiteral:
		c.openCurlyBrace()
		for _, expr2 := range expr.(ArrayLiteral).Exprs {
			c.expression(expr2)
			c.comma()
			c.space()
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

func (c *Compiler) declarationType(Typ Type, Ident Token) {
	switch Typ.(type) {
	case BasicType:
		c.expression(Typ.(BasicType).Expr)
		c.space()
		c.identifier(Ident)
	case PointerType:
		c.Type(Typ.(PointerType).BaseType)
		c.append([]byte("*"))
		c.space()
		c.identifier(Ident)
	case ConstType:
		c.Type(Typ.(ConstType).BaseType)
		c.space()
		c.append([]byte("const"))
		c.space()
		c.identifier(Ident)
	case ArrayType:
		c.declarationType(Typ.(ArrayType).BaseType, Ident)
		switch Typ.(ArrayType).BaseType.(type) {
		case DynamicType:
			break
		default:
			c.openBrace()
			c.identifier(Typ.(ArrayType).Size)
			c.closeBrace()
		}
		c.openBrace()
		c.identifier(Typ.(ArrayType).Size)
		c.closeBrace()
	case ImplictArrayType:
		c.declarationType(Typ.(ArrayType).BaseType, Ident)
		switch Typ.(ArrayType).BaseType.(type) {
		case DynamicType:
			break
		default:
			c.openBrace()
			c.closeBrace()
		}
	case DynamicType:
		c.append([]byte("__mem_block "))
		c.identifier(Ident)
	}
}

func (c *Compiler) Type(Typ Type) {
	switch Typ.(type) {
	case BasicType:
		c.expression(Typ.(BasicType).Expr)
	case PointerType:
		c.Type(Typ.(PointerType).BaseType)
		c.append([]byte("*"))
	case ConstType:
		c.Type(Typ.(ConstType).BaseType)
		c.space()
		c.append([]byte("const"))
	case ArrayType:
		c.Type(Typ.(ArrayType).BaseType)
		switch Typ.(ArrayType).BaseType.(type) {
		case DynamicType:
			break
		default:
			c.openBrace()
			c.identifier(Typ.(ArrayType).Size)
			c.closeBrace()
		}
	case ImplictArrayType:
		c.Type(Typ.(ImplictArrayType).BaseType)
		switch Typ.(ImplictArrayType).BaseType.(type) {
		case DynamicType:
			break
		default:
			c.openBrace()
			c.closeBrace()
		}
	case DynamicType:
		c.append([]byte("__mem_block"))
	}
}

func (c *Compiler) ifElse(ifElse IfElseBlock) {

	if ifElse.HasInitStmt {
		c.indent()
		c.openCurlyBrace()
		c.pushScope()
		c.statement(ifElse.InitStatement)
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
		c.statement(swtch.InitStatement)
	}

	c.indent()
	c.append([]byte("switch"))

	c.openParen()
	if swtch.Type == NoneSwtch {
		c.append([]byte("1"))
	} else {
		c.expression(swtch.Expr)
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
		for _, stmt := range Case.Block.Statements {
			c.statement(stmt)
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
			c.statement(stmt)
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

func (c *Compiler) structTypedef(st StructTypedef) {
	c.append([]byte("typedef struct {"))
	c.newline()
	c.pushScope()

	for _, prop := range st.Type.Props {
		c.globalDeclaration(prop)
	}

	c.popScope()
	c.closeCurlyBrace()
	c.space()
	c.identifier(st.Identifier)
	c.semicolon()
	c.newline()
}

func (c *Compiler) enumTypedef(en EnumTypedef) {
	c.append([]byte("typedef enum {"))
	c.newline()
	c.pushScope()

	for x, prop := range en.Type.Identifiers {
		c.indent()
		c.identifier(prop)
		val := en.Type.Values[x]

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
	c.identifier(en.Name)
	c.semicolon()
	c.newline()
}

func (c *Compiler) tupleTypedef(en TupleTypedef) {
	c.append([]byte("typedef struct {"))
	c.newline()
	c.pushScope()

	for x, prop := range en.Type.Types {
		c.indent()
		c.Type(prop)
		c.space()

		c.append([]byte("_" + strconv.Itoa(x)))

		c.semicolon()
		c.newline()
	}

	c.popScope()
	c.closeCurlyBrace()
	c.space()
	c.identifier(en.Identifier)
	c.semicolon()
	c.newline()
}

func (c *Compiler) heapAlloc(expr HeapAlloc) {
	c.append([]byte("new"))
	c.openParen()
	c.Type(expr.Type.(Type))
	c.comma()

	switch expr.Type.(type) {
	case BasicType:
		c.Type(expr.Type.(Type))
	case PointerType:
		c.Type(expr.Type.(PointerType).BaseType)
	case ConstType:
		c.Type(expr.Type.(ConstType).BaseType)
	case DynamicType:
		Typ := expr.Type.(DynamicType).BaseType
		switch Typ.(type) {
		case ImplictArrayType:
			c.Type(Typ.(ImplictArrayType).BaseType)
		default:
			c.Type(Typ)
		}
	case ImplictArrayType:
		c.Type(expr.Type.(ImplictArrayType).BaseType)
	case ArrayType:
		c.Type(expr.Type.(ArrayType).BaseType)
	}
	c.closeParen()
}
