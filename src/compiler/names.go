package compiler

import (
	"bytes"
	. "parser"
	"strconv"
)

var num int = 0

type Namespace struct {
	Base     string
	BasePath string
	Num      int
}

func (n *Namespace) Init(basePath string) {
	n.BasePath = basePath
	n.Base = strconv.Itoa(num) + "_"
	n.Num = num
	num++
}

func (n *Namespace) getEnumProp(enumName []byte, prop Token) Token {
	return Token{Buff: []byte("e" + n.Base + string(enumName) + "_" + string(prop.Buff)), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: prop.Line, Column: prop.Column, Flags: 3}
}

func (n *Namespace) getNewVarName(token Token) Token {
	if token.Flags != 0 {
		return token
	}
	// print(string(token.Buff), "\t", len(token.Buff) > 19 && bytes.Compare(token.Buff[:19], []byte("__UNSAFE_INTERNAL__")) == 0, "\n")
	if len(token.Buff) > 19 && bytes.Compare(token.Buff[:19], []byte("__UNSAFE_INTERNAL__")) == 0 {
		return Token{Buff: token.Buff[19:], PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 2}
	}
	return Token{Buff: []byte("v" + n.Base + string(token.Buff)), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 1}
}

func (n *Namespace) getLastImportPrefix() string {
	return "v" + strconv.Itoa(num-1) + "_"
}

func (n *Namespace) getActualName(token Token) Token {
	if token.Flags == 0 {
		return token
	}
	switch token.Flags {
	case 2:
		token.Buff = append([]byte("__UNSAFE_INTERNAL__"), token.Buff...)
	default:
		token.Buff = token.Buff[1+len(n.Base):]
	}
	token.Flags = 0
	return token
}

func (n *Namespace) getStrctDefaultName(strct Token) Token {
	strct.Buff = []byte("d" + n.Base + string(strct.Buff))
	strct.Flags = 4
	return strct
}

func (n *Namespace) getStrctMethodName(name Token, strct Token) Token {
	strct.Buff = []byte("m" + n.Base + string(name.Buff) + "_" + string(strct.Buff))
	strct.Flags = 5
	return strct
}

func (n *Namespace) joinName(prefix []byte, name Token) Token {
	name.Buff = append(prefix, name.Buff...)
	return name
}
