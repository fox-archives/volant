package SemanticAnalyzer

import . "parser"

type SemanticAnalyzer struct {
	Symbols      SymbolTable
	CurrentScope int
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

func (s *SemanticAnalyzer) Statement(stmt Statement) Statement {
	switch stmt.(type) {
	case Declaration:
		return s.declaration(stmt.(Declaration))
	case StructTypedef:
		return s.strct(stmt.(StructTypedef))
	case Block:
		return s.block(stmt.(Block))
	case Assignment:
		return s.assignment(stmt.(Assignment))
	case Expression:
		return s.expr(stmt.(Expression))
	case Return:
		return s.rturn(stmt.(Return))
	}

	return stmt
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
		block.Statements[i] = s.Statement(stmt)
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
	}

	if len(dec.Values) > 0 && len(dec.Types) != len(dec.Values) {
		NewError(SyntaxError, "Invalid number of types or values specified", dec.Identifiers[0].Line, dec.Identifiers[0].Column)
	}

	for i, Ident := range dec.Identifiers {
		if s.getSymbol(Ident) != nil {
			NewError(SyntaxError, string(Ident.Buff)+" has already been declared.", Ident.Line, Ident.Column)
		} else {
			s.addSymbol(Ident, dec.Types[i])
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
	}

	return expr2
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
	case IdentExpr:
		symbol := s.getSymbol(expr.(IdentExpr).Value)
		if symbol != nil {
			return symbol.Type
		}
	case TypeCast:
		return expr.(TypeCast).Type
	case UnaryExpr:
		if expr.(UnaryExpr).Op.SecondaryType == Mul {
			return s.getType(expr.(UnaryExpr).Expr).(PointerType).BaseType
		} else if expr.(UnaryExpr).Op.SecondaryType == And {
			return PointerType{BaseType: s.getType(expr.(UnaryExpr).Expr)}
		}
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
		}
	}

	return BasicType{}
}
