// This file basically makes the ast more verbose and converts the semantics of volant to c (kinda) and ik its prob badly named.

package compiler

import "github.com/eankeen/volant/parser"

type Normalizer struct{}

func (n *Normalizer) GlobalStatement(stmt parser.Statement) parser.Statement {
	switch stmt.(type) {
	case StructTypedef:
		return n.strct(stmt.(StructTypedef))
	case Declaration:
		return n.declaration(stmt.(Declaration))
	case EnumTypedef:
		return n.enum(stmt.(EnumTypedef))
	case TupleTypedef:
		return n.tple(stmt.(TupleTypedef))
	}
	return stmt
}

func (n *Normalizer) declaration(dec parser.Declaration) parser.Declaration {
	switch len(dec.Types) {
	case 1:
		Type := dec.Types[0]
		for i := 1; i < len(dec.Identifiers); i++ {
			dec.Types = append(dec.Types, Type)
		}
	}

	for i, Val := range dec.Values {
		dec.Values[i] = n.expression(Val)
	}

	return dec
}

func (n *Normalizer) strct(strct parser.StructTypedef) parser.StructTypedef {
	for i, prop := range strct.Type.Props {
		strct.Type.Props[i] = n.declaration(prop)
	}
	return strct
}

func (n *Normalizer) enum(enum parser.EnumTypedef) parser.EnumTypedef {
	return enum
}

func (n *Normalizer) tple(tple parser.TupleTypedef) parser.TupleTypedef {
	return tple
}

func (n *Normalizer) expression(expr parser.Expression) parser.Expression {
	return expr
}
