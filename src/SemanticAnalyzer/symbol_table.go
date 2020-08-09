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
	Type       Type
	Next       *Node
}

func (t *SymbolTable) Add(node *Node) {
	node.Next = t.First.Next
	t.First.Next = node
}

func (t *SymbolTable) Find(Ident Token, Scope int) *Node {
	node := t.First.Next

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
	last := t.First
	node := last.Next

	for node != nil {
		if bytes.Compare(node.Identifier.Buff, Ident.Buff) == 0 && node.Scope == Scope {
			last.Next = node.Next
			return
		}
		last = node
		node = node.Next
	}
}

func (t *SymbolTable) DeleteAll(Scope int) {
	last := t.First
	node := last.Next

	for node != nil {
		if node.Scope == Scope {
			last.Next = node.Next
		} else {
			last = node
		}
		node = node.Next
	}
}

func (node *Node) Print() {
	print(node.Identifier.Serialize())
}
