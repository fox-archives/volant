package SemanticAnalyzer

import . "parser"

type SemanticAnalyzer struct {
	Symbols      SymbolTable
	CurrentScope int
}

func (s *SemanticAnalyzer) getSymbol(Ident Token) *Node {
	return s.Symbols.Find(Ident, s.CurrentScope)
}

func (s *SemanticAnalyzer) pushScope() {
	s.CurrentScope++
}

func (s *SemanticAnalyzer) popScope() {
	s.Symbols.DeleteAll(s.CurrentScope)
	s.CurrentScope--
}

func (s *SemanticAnalyzer) Statement(stmt Statement) {
	switch stmt.(type) {
	case Declaration:
		s.declaration(stmt.(Declaration))
	case Block:
		s.block(stmt.(Block))
	}
}

func (s *SemanticAnalyzer) block(block Block) {
	s.pushScope()
	for _, stmt := range block.Statements {
		s.Statement(stmt)
	}
	s.popScope()
}

func (s *SemanticAnalyzer) declaration(dec Declaration) {

	if len(dec.Types) == 1 {
		Type := dec.Types[0]
		for i := 1; i < len(dec.Identifiers); i++ {
			dec.Types = append(dec.Types, Type)
		}
	}

	if len(dec.Types) != len(dec.Values) {
		NewError(SyntaxError, "Invalid number of types or values specified", dec.Identifiers[0].Line, dec.Identifiers[0].Column)
	}

	for i, Ident := range dec.Identifiers {
		if s.getSymbol(Ident) != nil {
			NewError(SyntaxError, string(Ident.Buff)+" has already been declared.", Ident.Line, Ident.Column)
		} else {
			s.addSymbol(Ident, dec.Types[i])
		}
	}

	for _, Val := range dec.Values {
		switch Val.(type) {
		case FuncExpr:
			s.block(Val.(FuncExpr).Block)
		}
	}
}

func (s *SemanticAnalyzer) addSymbol(Ident Token, Type TypeStruct) {
	s.Symbols.Add(&Node{Identifier: Ident, Scope: s.CurrentScope, Type: Type})
}
