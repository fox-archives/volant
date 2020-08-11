package compiler

import (
	. "parser"
	"strconv"
)

type Compiler struct {
	Buff       []byte
	ScopeCount int
}

func CompileFile(ast File) []byte {
	c := Compiler{}
	c.ScopeCount = 0

	for _, statement := range ast.Statements {
		c.globalStatement(statement)
	}

	return c.Buff
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

func (c *Compiler) globalStatement(stmt Statement) {
	c.newline()
	switch stmt.(type) {
	case Declaration:
		c.globalDeclaration(stmt.(Declaration))
	case Switch:
		c.swtch(stmt.(Switch))
	case Typedef:
		c.typedef(stmt.(Typedef))
	case NullStatement:
		c.semicolon()
	}
}

func (c *Compiler) statement(stmt Statement) {
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

func (c *Compiler) typedef(typedef Typedef) {
	c.append([]byte("typedef"))
	c.space()
	c.declarationType(typedef.Type, typedef.Name)
	c.semicolon()
	c.newline()
	switch typedef.Type.(type) {
	case StructType:
		c.strctDefault(typedef)
	}
}

func (c *Compiler) delete(delete Delete) {
	for _, Expr := range delete.Exprs {
		c.indent()
		c.append([]byte("delete"))
		c.openParen()
		c.expression(Expr)
		c.closeParen()
		c.semicolon()
		c.newline()
	}
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
		c.append([]byte("static"))
		c.space()
		c.declarationType(dec.Types[i], Var)

		if hasValues {
			switch dec.Types[i].(type) {
			case FuncType:
				c.block(dec.Values[i].(FuncExpr).Block)
			default:
				c.space()
				c.equal()
				c.space()
				c.expression(dec.Values[i])
			}
		}
		c.semicolon()
		c.newline()
	}
}

func (c *Compiler) declaration(dec Declaration) {
	hasValues := len(dec.Values) > 0

	for i, Var := range dec.Identifiers {
		c.indent()
		c.declarationType(dec.Types[i], Var)

		if hasValues {
			switch dec.Types[i].(type) {
			case FuncType:
				c.block(dec.Values[i].(FuncExpr).Block)
			default:
				c.space()
				c.equal()
				c.space()
				c.expression(dec.Values[i])
			}
		}
		c.semicolon()
		c.newline()
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
	case LenExpr:
		c.lenExpr(expr.(LenExpr))
	case SizeExpr:
		c.sizeExpr(expr.(SizeExpr))
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

func (c *Compiler) declarationType(Typ Type, Name Token) {
	typ := Typ
	sizes := []Token{}
	pointers := 0

	for {
		switch typ.(type) {
		case ArrayType:
			sizes = append(sizes, typ.(ArrayType).Size)
			typ = typ.(ArrayType).BaseType
			continue
		case ImplictArrayType:
			sizes = append(sizes, Token{
				Buff: []byte(""),
			})
			typ = typ.(ImplictArrayType).BaseType
			continue
		}
		break
	}

	for {
		switch typ.(type) {
		case PointerType:
			pointers++
			typ = typ.(PointerType).BaseType
			continue
		}
		break
	}

	for {
		switch typ.(type) {
		case FuncType:
			c.Type(typ.(FuncType).ReturnTypes[0])
			c.space()
			c.openParen()
			for i := 0; i < pointers; i++ {
				c.append([]byte("*"))
			}
			c.identifier(Name)
			for _, size := range sizes {
				c.openBrace()
				c.identifier(size)
				c.closeBrace()
			}
			c.closeParen()

			argNames := typ.(FuncType).ArgNames
			argTypes := typ.(FuncType).ArgTypes

			c.openParen()
			if len(argNames) > 0 {
				c.declarationType(argTypes[0], argNames[0])

				for i := 1; i < len(argNames); i++ {
					c.comma()
					c.space()
					c.declarationType(argTypes[i], argNames[i])
				}
			} else {
				c.Type(argTypes[0])
				for i := 1; i < len(argNames); i++ {
					c.comma()
					c.space()
					c.Type(argTypes[i])
				}
			}

			c.closeParen()
		case StructType:
			c.strct(typ.(StructType))
			c.space()
			c.identifier(Name)
		case EnumType:
			c.enum(typ.(EnumType))
			c.space()
			c.identifier(Name)
		case TupleType:
			c.tupl(typ.(TupleType))
			c.space()
			c.identifier(Name)
		default:
			c.Type(typ)
			c.space()
			c.openParen()
			for i := 0; i < pointers; i++ {
				c.append([]byte("*"))
			}
			c.identifier(Name)
			c.closeParen()
			for _, size := range sizes {
				c.openBrace()
				c.identifier(size)
				c.closeBrace()
			}
		}
		break
	}
}

func (c *Compiler) Type(Typ Type) {
	typ := Typ
	sizes := []Token{}
	pointers := 0

	for {
		switch typ.(type) {
		case ArrayType:
			sizes = append(sizes, typ.(ArrayType).Size)
			typ = typ.(ArrayType).BaseType
			continue
		case ImplictArrayType:
			sizes = append(sizes, Token{
				Buff: []byte(""),
			})
			typ = typ.(ImplictArrayType).BaseType
			continue
		}
		break
	}

	for {
		switch typ.(type) {
		case PointerType:
			pointers++
			typ = typ.(PointerType).BaseType
			continue
		}
		break
	}

	for {
		switch typ.(type) {
		case FuncType:
			c.Type(typ.(FuncType).ReturnTypes[0])
			c.space()
			c.openParen()
			for i := 0; i < pointers; i++ {
				c.append([]byte("*"))
			}
			for _, size := range sizes {
				c.openBrace()
				c.identifier(size)
				c.closeBrace()
			}
			c.closeParen()
			argTypes := typ.(FuncType).ArgTypes
			c.openParen()
			c.Type(argTypes[0])
			for i := 1; i < len(argTypes); i++ {
				c.comma()
				c.space()
				c.Type(argTypes[i])
			}
			c.closeParen()
			return
		case BasicType:
			c.expression(typ.(BasicType).Expr)
		case ConstType:
			c.Type(typ.(ConstType).BaseType)
			c.space()
			c.append([]byte("const"))
		case DynamicType:
			c.append([]byte("__mem_block"))
		case PointerType:
			c.Type(typ.(PointerType).BaseType)
			c.space()
			c.append([]byte("*"))
		case StructType:
			c.strct(typ.(StructType))
		case EnumType:
			c.enum(typ.(EnumType))
		case TupleType:
			c.tupl(typ.(TupleType))
		}

		for i := 0; i < pointers; i++ {
			c.append([]byte("*"))
		}
		for _, size := range sizes {
			c.openBrace()
			c.identifier(size)
			c.closeBrace()
		}
		break
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

		c.space()
		c.operator(as.Op)
		c.space()
		if len(as.Values) > 1 {
			c.expression(as.Values[i])
		} else {
			c.expression(as.Values[0])
		}

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

func (c *Compiler) strctPropDeclaration(dec Declaration) {
	for i, Var := range dec.Identifiers {
		c.indent()

		switch dec.Types[i].(type) {
		case FuncType:
			break
		default:
			c.declarationType(dec.Types[i], Var)
		}

		c.semicolon()
		c.newline()
	}
}

func (c *Compiler) strct(typ StructType) {
	c.append([]byte("struct "))
	c.openCurlyBrace()
	c.pushScope()
	c.newline()
	for _, prop := range typ.Props {
		c.strctPropDeclaration(prop)
	} /*
		for _, superStructType := range typ.SuperStructTypes {
			for _, prop := range superStructType.Props {
				c.strctPropDeclaration(prop)
			}
		}*/
	c.popScope()
	c.indent()
	c.closeCurlyBrace()
}

func (c *Compiler) strctDefault(strct Typedef) {
	c.identifier(strct.Name)
	c.space()
	c.identifier(getStrctDefaultName(getActualName(strct.Name)))

	c.space()
	c.equal()
	c.space()

	c.openParen()
	c.identifier(strct.Name)
	c.closeParen()

	c.openCurlyBrace()
	for _, prop := range strct.Type.(StructType).Props {
		if len(prop.Values) == 0 {
			break
		}

		for x, Ident := range prop.Identifiers {
			Val := prop.Values[x]

			switch Val.(type) {
			case FuncExpr:
				break
			default:
				c.dot()
				c.identifier(Ident)
				c.space()
				c.equal()
				c.space()
				c.expression(prop.Values[x])
				c.comma()
				c.space()
			}
		}
	}
	c.closeCurlyBrace()
	c.semicolon()
	c.newline()
}

func (c *Compiler) enum(en EnumType) {
	c.append([]byte("enum {"))
	c.newline()
	c.pushScope()

	for x, prop := range en.Identifiers {
		c.indent()
		c.identifier(prop)
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
}

func (c *Compiler) tupl(tupl TupleType) {
	c.append([]byte("struct {"))
	c.newline()
	c.pushScope()

	for x, prop := range tupl.Types {
		c.indent()
		c.declarationType(prop, Token{
			Buff:        []byte("_" + strconv.Itoa(x)),
			PrimaryType: Identifier,
		})
		c.semicolon()
		c.newline()
	}

	c.popScope()
	c.closeCurlyBrace()
}

func (c *Compiler) heapAlloc(expr HeapAlloc) {
	c.append([]byte("new"))
	c.openParen()
	c.Type(expr.Type)
	c.closeParen()
}

func (c *Compiler) lenExpr(expr LenExpr) {

	switch expr.Type.(type) {
	case DynamicType:
		Typ := expr.Type.(DynamicType).BaseType
		c.append([]byte("len"))
		c.openParen()
		c.expression(expr.Expr)
		c.comma()
		switch Typ.(type) {
		case ImplictArrayType:
			c.Type(Typ.(ImplictArrayType).BaseType)
		default:
			c.Type(Typ)
		}
		c.closeParen()
	case ArrayType:
		c.append([]byte("len2"))
		c.openParen()
		c.Type(expr.Type)
		c.comma()
		c.Type(expr.Type.(ArrayType).BaseType)
		c.closeParen()
	case Typedef:
		c.append([]byte("len3"))
		c.openParen()
		c.Type(expr.Type.(Typedef).Type)
		c.closeParen()
	default:
		c.append([]byte("len3"))
		c.openParen()
		c.Type(expr.Type)
		c.closeParen()
	}
}

func (c *Compiler) sizeExpr(expr SizeExpr) {
	switch expr.Type.(type) {
	case DynamicType:
		c.append([]byte("size"))
	default:
		c.append([]byte("size2"))
	}
	c.openParen()
	c.expression(expr.Expr)
	c.closeParen()
}
