package compiler

import (
	. "parser"
	"strconv"
)

type SemanticAnalyzer struct {
	Symbols      SymbolTable
	CurrentScope int
}

func AnalyzeFile(ast File) File {
	s := SemanticAnalyzer{
		Symbols: SymbolTable{
			First: &Node{},
		},
		CurrentScope: 0,
	}

	newAst := File{}

	for _, statement := range ast.Statements {
		newAst.Statements = append(newAst.Statements, s.statement(statement))
	}

	return newAst
}

func (s *SemanticAnalyzer) getSymbol(Ident Token) *Node {
	for i := 0; i <= s.CurrentScope; i++ {
		if symbol := s.Symbols.Find(Ident, i); symbol != nil {
			return symbol
		}
	}

	return nil
}

func (s *SemanticAnalyzer) addSymbol(Ident Token, Type Type) {
	s.Symbols.Add(&Node{Identifier: Ident, Scope: s.CurrentScope, Type: Type})
}

func (s *SemanticAnalyzer) pushScope() {
	s.CurrentScope++
}

func (s *SemanticAnalyzer) popScope() {
	s.Symbols.DeleteAll(s.CurrentScope)
	s.CurrentScope--
}

func (s *SemanticAnalyzer) statement(stmt Statement) Statement {
	switch stmt.(type) {
	case Typedef:
		return s.typedef(stmt.(Typedef))
	case EnumTypedef:
		return s.enum(stmt.(EnumTypedef))
	case StructTypedef:
		return s.strct(stmt.(StructTypedef))
	case Declaration:
		return s.declaration(stmt.(Declaration))
	case Block:
		return s.block(stmt.(Block))
	case Assignment:
		return s.assignment(stmt.(Assignment))
	case Return:
		return s.rturn(stmt.(Return))
	case Delete:
		return s.delete(stmt.(Delete))
	case Expression:
		return s.expr(stmt.(Expression))
	}

	return stmt
}

func (s *SemanticAnalyzer) enum(enum EnumTypedef) EnumTypedef {
	s.addSymbol(enum.Name, enum)
	for i, ident := range enum.Type.Identifiers {
		enum.Type.Identifiers[i].Buff = getEnumPropName(enum.Name.Buff, ident.Buff)
	}
	return enum
}

func (s *SemanticAnalyzer) typedef(typedef Typedef) Typedef {
	s.addSymbol(typedef.Name, typedef)
	return typedef
}

func (s *SemanticAnalyzer) delete(delete Delete) Delete {
	return Delete{Exprs: s.exprArray(delete.Exprs)}
}

func (s *SemanticAnalyzer) rturn(rturn Return) Return {
	return Return{Values: s.exprArray(rturn.Values)}
}

func (s *SemanticAnalyzer) assignment(as Assignment) Assignment {
	return Assignment{Variables: s.exprArray(as.Variables), Op: as.Op, Values: s.exprArray(as.Values)}
}

func (s *SemanticAnalyzer) block(block Block) Block {
	s.pushScope()
	for i, stmt := range block.Statements {
		block.Statements[i] = s.statement(stmt)
	}
	s.popScope()
	return block
}

func (s *SemanticAnalyzer) declaration(dec Declaration) Declaration {

	if len(dec.Types) == 1 {
		Type := dec.Types[0]
		for i := 1; i < len(dec.Identifiers); i++ {
			dec.Types = append(dec.Types, Type)
		}
	} else if len(dec.Types) == 0 {
		for _, Val := range dec.Values {
			dec.Types = append(dec.Types, s.getType(Val))
		}
	}

	if len(dec.Values) > 0 && len(dec.Types) != len(dec.Values) {
		NewError(SyntaxError, "Invalid number of types or values specified", dec.Identifiers[0].Line, dec.Identifiers[0].Column)
	}

	for i, Ident := range dec.Identifiers {
		if s.getSymbol(Ident) != nil {
			NewError(SyntaxError, string(Ident.Buff)+" has already been declared.", Ident.Line, Ident.Column)
		} else {
			NewIdent := getVarName(Ident)
			s.addSymbol(NewIdent, dec.Types[i])
			dec.Identifiers[i] = NewIdent
		}
	}

	for i, Val := range dec.Values {
		switch Val.(type) {
		case FuncExpr:
			Func := Val.(FuncExpr)
			dec.Values[i] = FuncExpr{
				Block:       s.block(Func.Block),
				ReturnTypes: Func.ReturnTypes,
				Args:        Func.Args,
				Type:        Func.Type,
			}
		default:
			dec.Values[i] = s.expr(Val)
		}
	}

	return dec
}

func (s *SemanticAnalyzer) strct(strct StructTypedef) StructTypedef {
	for i, prop := range strct.Type.Props {
		strct.Type.Props[i] = s.declaration(prop)
	}
	return strct
}

func (s *SemanticAnalyzer) expr(expr Expression) Expression {
	expr2 := expr

	switch expr.(type) {
	case IdentExpr:
		expr2 = IdentExpr{Value: getVarName(expr.(IdentExpr).Value)}
	case UnaryExpr:
		expr2 = UnaryExpr{Op: expr.(UnaryExpr).Op, Expr: s.expr(expr.(UnaryExpr).Expr)}
	case BinaryExpr:
		expr2 = BinaryExpr{Left: s.expr(expr.(BinaryExpr).Left), Op: expr.(BinaryExpr).Op, Right: s.expr(expr.(BinaryExpr).Right)}
	case PostfixUnaryExpr:
		expr2 = PostfixUnaryExpr{Op: expr.(PostfixUnaryExpr).Op, Expr: s.expr(expr.(PostfixUnaryExpr).Expr)}
	case TernaryExpr:
		expr2 = TernaryExpr{Cond: s.expr(expr.(TernaryExpr).Cond), Left: s.expr(expr.(TernaryExpr).Left), Right: s.expr(expr.(TernaryExpr).Right)}
	case ArrayLiteral:
		return ArrayLiteral{Exprs: s.exprArray(expr.(ArrayLiteral).Exprs)}
	case CallExpr:
		expr2 = CallExpr{Function: s.expr(expr.(CallExpr).Function), Args: s.exprArray(expr.(CallExpr).Args)}
	case TypeCast:
		expr2 = TypeCast{Type: expr.(TypeCast).Type, Expr: s.expr(expr.(TypeCast).Expr)}
	case ArrayMemberExpr:
		expr2 = ArrayMemberExpr{Parent: s.decayToPointer(s.expr(expr.(ArrayMemberExpr).Parent)), Index: s.expr(expr.(ArrayMemberExpr).Index)}
	case MemberExpr:
		expr2 = s.memberExpr(expr.(MemberExpr))
	}

	return expr2
}

func (s *SemanticAnalyzer) memberExpr(expr MemberExpr) Expression {
	switch expr.Base.(type) {
	case IdentExpr:
		break
	default:
		return MemberExpr{Base: s.expr(expr.Base), Expr: s.expr(expr.Expr)}
	}

	Typ := s.getType(expr.Base)

	switch Typ.(type) {
	case EnumTypedef:

		switch expr.Expr.(type) {
		case IdentExpr:
			break;
		default:
			// NewError(SyntaxError, "Expected identifier, got expression", )
		}
		return IdentExpr{
			Value: Token{
				Buff: getEnumPropName(expr.Base.(IdentExpr).Value.Buff, expr.Expr.(IdentExpr).Value.Buff),
				PrimaryType: Identifier,
				SecondaryType: SecondaryNullType,
				Line: expr.Expr.(IdentExpr).Value.Line,
				Column: expr.Expr.(IdentExpr).Value.Line,
			},
		}
	default:
		return MemberExpr{Base: s.expr(expr.Base), Expr: s.expr(expr.Expr)}
	}
}

func (s *SemanticAnalyzer) exprArray(array []Expression) []Expression {
	Exprs := []Expression{}
	for _, Expr := range array {
		Exprs = append(Exprs, s.expr(Expr))
	}
	return Exprs
}

func (s *SemanticAnalyzer) decayToPointer(expr Expression) Expression {
	Typ := s.getType(expr)

	switch Typ.(type) {
	case DynamicType:

		switch Typ.(DynamicType).BaseType.(type) {
		case ImplictArrayType:
			return TypeCast{
				Type: Type(PointerType{BaseType: Typ.(DynamicType).BaseType.(ImplictArrayType).BaseType}),
				Expr: Expression(MemberExpr{
					Base: expr,
					Expr: Expression(IdentExpr{
						Value: Token{
							Buff:          []byte("_ptr"),
							PrimaryType:   Identifier,
							SecondaryType: SecondaryNullType,
						},
					}),
				}),
			}
		}
		return TypeCast{
			Type: Type(PointerType{BaseType: Typ.(DynamicType).BaseType}),
			Expr: Expression(MemberExpr{
				Base: expr,
				Expr: Expression(IdentExpr{
					Value: Token{
						Buff:          []byte("_ptr"),
						PrimaryType:   Identifier,
						SecondaryType: SecondaryNullType,
					},
				}),
			}),
		}
	}
	return expr
}

func (s *SemanticAnalyzer) getType(expr Expression) Type {
	switch expr.(type) {
	case BasicLit:
		switch expr.(BasicLit).Value.PrimaryType {
		case CharLiteral:
		case NumberLiteral:
			return BasicType{
				Expr: IdentExpr{
					Value: Token{
						Buff:          []byte("i64"),
						PrimaryType:   Identifier,
						SecondaryType: SecondaryNullType,
					},
				},
			}
		case StringLiteral:
			return ArrayType{
				Size: Token{
					PrimaryType:   NumberLiteral,
					SecondaryType: DecimalRadix,
					Buff:          []byte(strconv.Itoa(expr.(BasicLit).Value.Size)),
				},
				BaseType: BasicType{
					Expr: IdentExpr{
						Value: Token{
							Buff:          []byte("u8"),
							PrimaryType:   Identifier,
							SecondaryType: SecondaryNullType,
						},
					},
				},
			}
		}
	case IdentExpr:
		symbol := s.getSymbol(expr.(IdentExpr).Value)
		if symbol != nil {
			return symbol.Type
		}
		symbol = s.getSymbol(getVarName(expr.(IdentExpr).Value))
		if symbol != nil {
			return symbol.Type
		}
	case TernaryExpr:
		return s.getType(expr.(TernaryExpr).Left)
	case TypeCast:
		return expr.(TypeCast).Type
	case UnaryExpr:
		if expr.(UnaryExpr).Op.SecondaryType == Mul {
			return s.getType(expr.(UnaryExpr).Expr).(PointerType).BaseType
		} else if expr.(UnaryExpr).Op.SecondaryType == And {
			return PointerType{BaseType: s.getType(expr.(UnaryExpr).Expr)}
		} else {
			return s.getType(expr.(UnaryExpr).Expr)
		}
	case PostfixUnaryExpr:
		return s.getType(expr.(PostfixUnaryExpr).Expr)
	case CallExpr:
		return s.getType(expr.(CallExpr).Function).(FuncType).ReturnTypes[0]
	case ArrayMemberExpr:
		Typ := s.getType(expr.(ArrayMemberExpr).Parent)

		switch Typ.(type) {
		case ArrayType:
			return Typ.(ArrayType).BaseType
		case ImplictArrayType:
			return Typ.(ImplictArrayType).BaseType
		case PointerType:
			return Typ.(PointerType).BaseType
		case DynamicType:
			switch Typ.(DynamicType).BaseType.(type) {
			case ImplictArrayType:
				return Typ.(DynamicType).BaseType.(ImplictArrayType).BaseType
			default:
				return Typ.(DynamicType).BaseType
			}
		}

		return Typ
	case FuncExpr:
		Typ := FuncType{
			ReturnTypes: expr.(FuncExpr).ReturnTypes,
			Type:        expr.(FuncExpr).Type,
		}

		for _, arg := range expr.(FuncExpr).Args {
			Typ.Args = append(Typ.Args, arg.Type)
		}
		return Typ
	case HeapAlloc:
		switch expr.(HeapAlloc).Type.(type) {
		case ArrayType:
			return DynamicType{
				BaseType: ImplictArrayType{
					BaseType: expr.(HeapAlloc).Type.(ArrayType).BaseType,
				},
			}
		default:
			return DynamicType{BaseType: expr.(HeapAlloc).Type}
		}
	}

	return BasicType{}
}
