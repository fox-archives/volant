package compiler

import (
	"bytes"
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

func (s *SemanticAnalyzer) enum(enum EnumType, Name Token) EnumType {
	s.pushScope()
	for i, Ident := range enum.Identifiers {
		enum.Identifiers[i] = getEnumProp(Name.Buff, Ident)
	}
	s.popScope()
	return enum
}

func (s *SemanticAnalyzer) typedef(typedef Typedef) Typedef {
	defer s.addSymbol(typedef.Name, typedef)

	Typ := typedef.Type

	switch typedef.Type.(type) {
	case StructType:
		strct := Typ.(StructType)
		for _, superStruct := range Typ.(StructType).SuperStructs {
			for _, prop := range s.getType(superStruct).(Typedef).Type.(StructType).Props {
				strct.Props = append(strct.Props, prop)
			}
		}
		typedef.Type = strct
		Typ = s.strct(strct)
	case TupleType:
		Typ = s.tupl(Typ.(TupleType))
	case EnumType:
		Typ = s.enum(Typ.(EnumType), typedef.Name)
	}

	return Typedef{Type: s.typ(Typ), Name: getNewVarName(typedef.Name)}
}

func (s *SemanticAnalyzer) tupl(typ TupleType) TupleType {
	return TupleType{Types: s.typeArray(typ.Types)}
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
		if len(dec.Values) == 0 {
			NewError(SyntaxError, "Cannot declare a variable without type and value.", dec.Identifiers[0].Line, dec.Identifiers[0].Column)
		}
		for _, Val := range dec.Values {
			dec.Types = append(dec.Types, s.getType(Val))
		}
	}

	for i, typ := range dec.Types {
		dec.Types[i] = s.typ(typ)
	}

	if len(dec.Values) > 0 && len(dec.Types) != len(dec.Values) {
		NewError(SyntaxError, "Invalid number of types or values specified", dec.Identifiers[0].Line, dec.Identifiers[0].Column)
	}

	for i, Ident := range dec.Identifiers {
		if s.getSymbol(Ident) != nil {
			NewError(SyntaxError, string(Ident.Buff)+" has already been declared.", Ident.Line, Ident.Column)
		} else {
			s.addSymbol(Ident, dec.Types[i])
			dec.Identifiers[i] = getNewVarName(Ident)
		}
	}

	for i, Val := range dec.Values {
		dec.Values[i] = s.expr(Val)
	}

	return dec
}

func (s *SemanticAnalyzer) strct(strct StructType) StructType {
	s.pushScope()
	for i, prop := range strct.Props {
		strct.Props[i] = s.declaration(prop)
	}
	s.popScope()
	return strct
}

func (s *SemanticAnalyzer) expr(expr Expression) Expression {
	expr2 := expr

	switch expr.(type) {
	case IdentExpr:
		expr2 = IdentExpr{Value: getNewVarName(expr.(IdentExpr).Value)}
	case UnaryExpr:
		expr2 = UnaryExpr{Op: expr.(UnaryExpr).Op, Expr: s.expr(expr.(UnaryExpr).Expr)}
	case BinaryExpr:
		expr2 = BinaryExpr{Left: s.expr(expr.(BinaryExpr).Left), Op: expr.(BinaryExpr).Op, Right: s.expr(expr.(BinaryExpr).Right)}
	case PostfixUnaryExpr:
		expr2 = PostfixUnaryExpr{Op: expr.(PostfixUnaryExpr).Op, Expr: s.expr(expr.(PostfixUnaryExpr).Expr)}
	case TernaryExpr:
		expr2 = TernaryExpr{Cond: s.expr(expr.(TernaryExpr).Cond), Left: s.expr(expr.(TernaryExpr).Left), Right: s.expr(expr.(TernaryExpr).Right)}
	case ArrayLiteral:
		expr2 = ArrayLiteral{Exprs: s.exprArray(expr.(ArrayLiteral).Exprs)}
	case CallExpr:
		expr2 = CallExpr{Function: s.expr(expr.(CallExpr).Function), Args: s.exprArray(expr.(CallExpr).Args)}
	case TypeCast:
		expr2 = s.typeCast(expr.(TypeCast))
	case ArrayMemberExpr:
		expr2 = s.arrayMemberExpr(expr.(ArrayMemberExpr))
	case MemberExpr:
		expr2 = s.memberExpr(expr.(MemberExpr))
	case LenExpr:
		expr2 = s.lenExpr(expr.(LenExpr))
	case SizeExpr:
		expr2 = s.sizeExpr(expr.(SizeExpr))
	case CompoundLiteral:
		expr2 = s.compoundLiteral(expr.(CompoundLiteral))
	case FuncExpr:
		expr2 = FuncExpr{Block: s.block(expr.(FuncExpr).Block), Type: s.typ(expr.(FuncExpr).Type).(FuncType)}
	case HeapAlloc:
		expr2 = HeapAlloc{Type: s.typ(expr.(HeapAlloc).Type)}
	}

	return expr2
}

func (s *SemanticAnalyzer) typ(typ Type) Type {
	switch typ.(type) {
	case BasicType:
		return BasicType{Expr: s.expr(typ.(BasicType).Expr)}
	case PointerType:
		return PointerType{BaseType: s.typ(typ.(PointerType).BaseType)}
	case DynamicType:
		return DynamicType{BaseType: s.typ(typ.(DynamicType).BaseType)}
	case ConstType:
		return ConstType{BaseType: s.typ(typ.(ConstType).BaseType)}
	case ImplictArrayType:
		return ImplictArrayType{BaseType: s.typ(typ.(ImplictArrayType).BaseType)}
	case ArrayType:
		return ArrayType{Size: typ.(ArrayType).Size, BaseType: s.typ(typ.(ArrayType).BaseType)}
	case FuncType:
		return FuncType{Type: typ.(FuncType).Type, ArgTypes: s.typeArray(typ.(FuncType).ArgTypes), ArgNames: typ.(FuncType).ArgNames, ReturnTypes: s.typeArray(typ.(FuncType).ReturnTypes)}
	}

	return typ
}

func (s *SemanticAnalyzer) typeArray(types []Type) []Type {
	typs := []Type{}
	for _, typ := range types {
		typs = append(typs, s.typ(typ))
	}
	return typs
}

func (s *SemanticAnalyzer) typeCast(expr TypeCast) TypeCast {
	switch expr.Type.(type) {
	case BasicType:
		break
	default:
		return TypeCast{Type: s.typ(expr.Type), Expr: s.expr(expr.Expr)}
	}

	Typ := s.getType(expr.Type.(BasicType).Expr)

	switch Typ.(type) {
	case StructType:
		break
	default:
		return TypeCast{Type: s.typ(expr.Type), Expr: s.expr(expr.Expr)}
	}

	return TypeCast{
		Type: expr.Type,
		Expr: UnaryExpr{
			Op: Token{
				Buff:          []byte("*"),
				PrimaryType:   AirthmaticOperator,
				SecondaryType: Mul,
			},
			Expr: TypeCast{
				Type: PointerType{
					BaseType: expr.Type,
				},
				Expr: BinaryExpr{
					Left: UnaryExpr{
						Op: Token{
							Buff:          []byte("&"),
							PrimaryType:   BitwiseOperator,
							SecondaryType: And,
						},
						Expr: s.expr(expr),
					},
					Op: Token{
						Buff:          []byte("+"),
						PrimaryType:   AirthmaticOperator,
						SecondaryType: Add,
					},
					Right: CallExpr{
						Function: IdentExpr{
							Value: Token{
								Buff:        []byte("offsetof"),
								PrimaryType: Identifier,
							},
						},
						Args: []Expression{
							expr.Type.(BasicType).Expr,
							Typ.(StructType).Props[0].Types[0],
						},
					},
				},
			},
		},
	}
}

//cast(strct)var -> *((strct *)(&var +offsetof(strct, a)))

func (s *SemanticAnalyzer) compoundLiteral(expr CompoundLiteral) CompoundLiteral {
	Name := s.expr(expr.Name)
	Typ := s.getType(Name)

	switch Typ.(type) {
	case Typedef:
		break
	default:
		NewError(SyntaxError, "Unexpected expression. Expected struct or tuple type.", 0, 0)
	}

	switch Typ.(Typedef).Type.(type) {
	case StructType:
		break
	case TupleType:
		return CompoundLiteral{Name: Name, Data: s.compoundLiteralData(expr.Data)}
	default:
		NewError(SyntaxError, "Unexpected expression. Expected struct or tuple type.", 0, 0)
	}

	strct := Typ.(Typedef).Type.(StructType)
	data := s.compoundLiteralData(expr.Data)

	if len(data.Fields) == 0 && len(data.Values) > 0 {
		return CompoundLiteral{Name: Name, Data: data}
	}

	for _, prop := range strct.Props {
		if len(prop.Values) == 0 {
			break
		}

		for i, Ident := range prop.Identifiers {
			if hasField(data.Fields, Ident) {
				continue
			}

			if len(prop.Types) > 1 {
				switch prop.Types[i].(type) {
				case FuncType:
					break
				default:
					data.Fields = append(data.Fields, Ident)
					data.Values = append(data.Values, MemberExpr{
						Base: IdentExpr{
							Value: getStrctDefaultName(Typ.(Typedef).Name),
						},
						Expr: IdentExpr{
							Value: Ident,
						},
					})
				}
			} else {
				switch prop.Types[0].(type) {
				case FuncType:
					break
				default:
					data.Fields = append(data.Fields, Ident)
					data.Values = append(data.Values, MemberExpr{
						Base: IdentExpr{
							Value: getStrctDefaultName(Typ.(Typedef).Name),
						},
						Expr: IdentExpr{
							Value: Ident,
						},
					})
				}
			}
		}
	}

	return CompoundLiteral{Name: Name, Data: data}
}

func hasField(fields []Token, field Token) bool {
	for _, tok := range fields {
		if bytes.Compare(tok.Buff, field.Buff) == 0 {
			return true
		}
	}
	return false
}

func (s *SemanticAnalyzer) compoundLiteralData(data CompoundLiteralData) CompoundLiteralData {
	for i, Val := range data.Values {
		data.Values[i] = s.expr(Val)
	}
	for i, Field := range data.Fields {
		data.Fields[i] = getNewVarName(Field)
	}
	return data
}

func (s *SemanticAnalyzer) arrayMemberExpr(expr ArrayMemberExpr) Expression {
	Typ := s.getType(expr)

	switch Typ.(type) {
	case BasicType:
		break
	default:
		return ArrayMemberExpr{Parent: s.decayToPointer(s.expr(expr.Parent)), Index: s.expr(expr.Index)}
	}

	Typ = s.getType(Typ.(BasicType).Expr)

	if Typ == nil {
		return ArrayMemberExpr{Parent: s.decayToPointer(s.expr(expr.Parent)), Index: s.expr(expr.Index)}
	}

	switch Typ.(type) {
	case Typedef:
		break
	default:
		return ArrayMemberExpr{Parent: s.decayToPointer(s.expr(expr.Parent)), Index: s.expr(expr.Index)}
	}

	switch Typ.(Typedef).Type.(type) {
	case TupleType:
		break
	default:
		return ArrayMemberExpr{Parent: s.decayToPointer(s.expr(expr.Parent)), Index: s.expr(expr.Index)}
	}

	switch expr.Index.(type) {
	case BasicLit:
		break
	default:
		NewError(SyntaxError, "Only number literals are allowed in tupl element reference.", 0, 0)
	}

	switch expr.Index.(BasicLit).Value.PrimaryType {
	case NumberLiteral:
		break
	default:
		NewError(SyntaxError, "Only number literals are allowed in tupl element reference.", 0, 0)
	}

	return MemberExpr{
		Base: s.expr(expr.Parent),
		Expr: IdentExpr{
			Value: Token{
				Buff:          []byte("_" + string(expr.Index.(BasicLit).Value.Buff)),
				PrimaryType:   Identifier,
				SecondaryType: SecondaryNullType,
			},
		},
	}
}

func (s *SemanticAnalyzer) lenExpr(expr LenExpr) LenExpr {
	Expr := s.expr(expr.Expr)
	return LenExpr{Expr: Expr, Type: s.getType(Expr)}
}

func (s *SemanticAnalyzer) sizeExpr(expr SizeExpr) SizeExpr {
	Expr := s.expr(expr.Expr)
	return SizeExpr{Expr: Expr, Type: s.getType(Expr)}
}

func (s *SemanticAnalyzer) memberExpr(expr MemberExpr) Expression {
	Typ := s.getType(expr.Base)

	switch Typ.(type) {
	case Typedef:
		break
	default:
		return MemberExpr{Base: s.expr(expr.Base), Expr: s.expr(expr.Expr)}
	}

	switch Typ.(Typedef).Type.(type) {
	case EnumType:
		break
	default:
		return MemberExpr{Base: s.expr(expr.Base), Expr: s.expr(expr.Expr)}
	}

	switch expr.Expr.(type) {
	case IdentExpr:
		break
	default:
		// NewError(SyntaxError, "Expected identifier, got expression", )
	}

	return IdentExpr{Value: getEnumProp(expr.Base.(IdentExpr).Value.Buff, expr.Expr.(IdentExpr).Value)}
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
					Buff:          []byte(strconv.Itoa(expr.(BasicLit).Value.Flags)),
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
		Ident := expr.(IdentExpr).Value
		var symbol *Node
		if Ident.Flags == 1 {
			symbol = s.getSymbol(getActualName(Ident))
		} else {
			symbol = s.getSymbol(Ident)
		}
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
		return expr.(FuncExpr).Type
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
	case CompoundLiteral:
		return BasicType{Expr: s.expr(expr.(CompoundLiteral).Name)}
	}

	return BasicType{}
}
