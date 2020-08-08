package SemanticAnalyzer

import "github.com/eankeen/volant/parser"

type SemanticAnalyzer struct {
	Symbols      SymbolTable
	CurrentScope int
}

func (s *SemanticAnalyzer) getSymbol(Ident parser.Token) *Node {
	return s.Symbols.Find(Ident, s.CurrentScope)
}

func (s *SemanticAnalyzer) pushScope() {
	s.CurrentScope++
}

func (s *SemanticAnalyzer) popScope() {
	s.Symbols.DeleteAll(s.CurrentScope)
	s.CurrentScope--
}

func (s *SemanticAnalyzer) Statement(stmt parser.Statement) {
	switch stmt.(type) {
	case parser.Declaration:
		s.declaration(stmt.(parser.Declaration))
	case parser.Block:
		s.block(stmt.(parser.Block))
	}
}

func (s *SemanticAnalyzer) block(block parser.Block) {
	s.pushScope()
	for _, stmt := range block.Statements {
		s.Statement(stmt)
	}
	s.popScope()
}

func (s *SemanticAnalyzer) declaration(dec parser.Declaration) {

	if len(dec.Types) == 1 {
		Type := dec.Types[0]
		for i := 1; i < len(dec.Identifiers); i++ {
			dec.Types = append(dec.Types, Type)
		}
	}

	if len(dec.Values) > 0 && len(dec.Types) != len(dec.Values) {
		parser.NewError(parser.SyntaxError, "Invalid number of types or values specified", dec.Identifiers[0].Line, dec.Identifiers[0].Column)
	}

	for i, Ident := range dec.Identifiers {
		if s.getSymbol(Ident) != nil {
			parser.NewError(parser.SyntaxError, string(Ident.Buff)+" has already been declared.", Ident.Line, Ident.Column)
		} else {
			s.addSymbol(Ident, dec.Types[i])
		}
	}

	for _, Val := range dec.Values {
		switch Val.(type) {
		case parser.FuncExpr:
			s.block(Val.(parser.FuncExpr).Block)
		}
	}
}

func (s *SemanticAnalyzer) addSymbol(Ident parser.Token, Type parser.TypeStruct) {
	s.Symbols.Add(&Node{Identifier: Ident, Scope: s.CurrentScope, Type: Type})
}
