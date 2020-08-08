package SemanticAnalyzer

import . "parser"

type SemanticAnalyzer struct {
	Symbols      SymbolTable
	CurrentScope int
}

func (s *SemanticAnalyzer) getSymbol(Ident Token) *Node {
	return s.Symbols.Find(Ident, s.CurrentScope)
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
	case Expression:
		return s.expr(stmt.(Expression), false)
	}

	return stmt
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
			dec.Values[i] = s.expr(Val, false)
		}
	}

	return dec
}

func (n *SemanticAnalyzer) strct(strct StructTypedef) StructTypedef {
	for i, prop := range strct.Type.Props {
		strct.Type.Props[i] = n.declaration(prop)
	}
	return strct
}

func (s *SemanticAnalyzer) expr(expr Expression, decay bool) Expression {
	switch expr.(type) {
	case BasicLit:
		return expr
	case IdentExpr:
		if !decay {
			return expr
		}
		// haha here we do our almighty magic
		symbol := s.getSymbol(expr.(IdentExpr).Value)

		if symbol == nil {
			return expr
		}

		switch symbol.Type.(type) {
		case DynamicType:
			switch symbol.Type.(DynamicType).BaseType.(type) {
			case ImplictArrayType:
				return TypeCast{
					Type: Type(PointerType{symbol.Type.(DynamicType).BaseType.(ImplictArrayType).BaseType}),
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
			default:
				return TypeCast{
					Type: Type(PointerType{symbol.Type.(DynamicType).BaseType}),
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
		default:
			return expr
		}
	case UnaryExpr:
		return UnaryExpr{Op: expr.(UnaryExpr).Op, Expr: s.expr(expr.(UnaryExpr).Expr, false)}
	case BinaryExpr:
		return BinaryExpr{Left: s.expr(expr.(BinaryExpr).Left, false), Op: expr.(BinaryExpr).Op, Right: s.expr(expr.(BinaryExpr).Right, false)}
	case PostfixUnaryExpr:
		return PostfixUnaryExpr{Op: expr.(PostfixUnaryExpr).Op, Expr: s.expr(expr.(PostfixUnaryExpr).Expr, false)}
	case TernaryExpr:
		return TernaryExpr{Cond: s.expr(expr.(TernaryExpr).Cond, false), Left: s.expr(expr.(TernaryExpr).Left, false), Right: s.expr(expr.(TernaryExpr).Right, false)}
	case ArrayLiteral:
		return ArrayLiteral{Exprs: s.exprArray(expr.(ArrayLiteral).Exprs, false)}
	case CallExpr:
		return CallExpr{Function: s.expr(expr.(CallExpr).Function, false), Args: s.exprArray(expr.(CallExpr).Args, false)}
	case TypeCast:
		return TypeCast{Type: expr.(TypeCast).Type, Expr: s.expr(expr.(TypeCast).Expr, false)}
	case ArrayMemberExpr:
		return ArrayMemberExpr{Parent: s.expr(expr.(ArrayMemberExpr).Parent, true), Index: s.expr(expr.(ArrayMemberExpr).Index, false)}
	}

	return expr
}

func (s *SemanticAnalyzer) exprArray(array []Expression, decay bool) []Expression {
	Exprs := []Expression{}
	for _, Expr := range array {
		Exprs = append(Exprs, s.expr(Expr, decay))
	}
	return Exprs
}
