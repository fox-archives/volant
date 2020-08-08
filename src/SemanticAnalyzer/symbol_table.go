package SemanticAnalyzer

import (
	"bytes"
	. "parser"
)

type SymbolTable struct {
	First *Node
}

type Node struct {
	Identifier Token
	Scope      int
	Type       TypeStruct
	Next       *Node
}

func (t *SymbolTable) Add(node *Node) {
	node.Next = t.First
	t.First = node
}

func (t *SymbolTable) Find(Ident Token, Scope int) *Node {
	node := t.First

	if node == nil {
		return nil
	}

	for bytes.Compare(node.Identifier.Buff, Ident.Buff) != 0 || node.Scope != Scope {
		node = node.Next

		if node == nil {
			return nil
		}
	}

	return node
}

func (t *SymbolTable) Delete(Ident Token, Scope int) {
	node := t.First
	last := (*Node)(nil)

	for bytes.Compare(node.Identifier.Buff, Ident.Buff) != 0 || node.Scope != Scope {
		last = node
		node = node.Next
	}

	last.Next = node.Next
}

func (t *SymbolTable) DeleteAll(Scope int) {
	node := t.First
	last := (*Node)(nil)

	for node != nil {
		if node.Scope == Scope {
			last.Next = node.Next
		}
		last = node
		node = node.Next
	}
}

func (node *Node) Print() {
	print(node.Identifier.Serialize())
}
