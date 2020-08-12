package compiler

import (
	"bytes"
	. "parser"
	Path "path"
)

type Namespace struct {
	Base     string
	BasePath string
}

func (n *Namespace) Init(path string, isMain bool) {
	n.BasePath = Path.Join(n.BasePath, Path.Dir(path))

	if isMain {
		n.Base = "main_"
		return
	}
	n.Base = ""
	for _, chr := range path {
		if (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') || (chr >= '0' && chr <= '9') {
			n.Base += string(chr)
		} else {
			n.Base += "_"
		}
	}
	n.Base += "_"
}

func (n *Namespace) getEnumProp(enumName []byte, prop Token) Token {
	return Token{Buff: []byte("e_" + n.Base + string(enumName) + "_" + string(prop.Buff)), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: prop.Line, Column: prop.Column, Flags: 3}
}

func (n *Namespace) getNewVarName(token Token) Token {
	if token.Flags == 1 || token.Flags == 2 {
		return token
	}
	if len(token.Buff) > 19 && bytes.Compare(token.Buff[:19], []byte("__UNSAFE_INTERNAL__")) == 0 {
		return Token{Buff: token.Buff[19:], PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 2}
	}
	return Token{Buff: []byte("v_" + n.Base + string(token.Buff)), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 1}
}

func (n *Namespace) getImportPrefix(path string) string {
	Base := "v_"
	for _, chr := range path {
		if (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') || (chr >= '0' && chr <= '9') {
			Base += string(chr)
		} else {
			Base += "_"
		}
	}
	Base += "_"
	return Base
}

func (n *Namespace) getActualName(token Token) Token {
	switch token.Flags {
	case 1:
		token.Buff = token.Buff[2+len(n.Base):]
	case 2:
		token.Buff = append([]byte("__UNSAFE_INTERNAL__"), token.Buff...)
	case 5:
		token.Buff = token.Buff[len(n.Base):]
	}
	token.Flags = 0
	return token
}

func (n *Namespace) getStrctDefaultName(strct Token) Token {
	strct.Buff = []byte("d_" + n.Base + string(strct.Buff))
	strct.Flags = 4
	return strct
}

func (n *Namespace) joinName(prefix []byte, name Token) Token {
	name.Buff = append(prefix, name.Buff...)
	return name
}
