// This file basically makes the ast more verbose and converts the sematics of volant to c (kinda) and ik its prob badly named.

package compiler

import . "parser"

type Normalizer struct{}

func (n *Normalizer) GlobalStatement(stmt Statement) Statement {
	switch stmt.(type) {
	case Struct:
		return n.strct(stmt.(Struct))
	case Declaration:
		return n.declaration(stmt.(Declaration))
	case Enum:
		return n.enum(stmt.(Enum))
	case Tuple:
		return n.tple(stmt.(Tuple))
	}
	return stmt
}

func (n *Normalizer) declaration(dec Declaration) Declaration {
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

func (n *Normalizer) strct(strct Struct) Struct {
	for i, prop := range strct.Props {
		strct.Props[i] = n.declaration(prop)
	}
	return strct
}

func (n *Normalizer) enum(enum Enum) Enum {
	return enum
}

func (n *Normalizer) tple(tple Tuple) Tuple {
	return tple
}

func (n *Normalizer) expression(expr Expression) Expression {
	return expr
}
