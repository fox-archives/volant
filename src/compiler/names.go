package compiler

import (
	"bytes"
	. "parser"
)

func getEnumProp(enumName []byte, prop Token) Token {
	return Token{Buff: []byte("enum_" + string(enumName) + "_" + string(prop.Buff)), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: prop.Line, Column: prop.Column, Flags: 3}
}

func getNewVarName(token Token) Token {
	if token.Flags == 1 || token.Flags == 2 {
		return token
	}
	if len(token.Buff) > 19 && bytes.Compare(token.Buff[:19], []byte("__UNSAFE_INTERNAL__")) == 0 {
		return Token{Buff: token.Buff[19:], PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 2}
	}
	return Token{Buff: []byte("v_" + string(token.Buff)), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 1}
}

func getActualName(token Token) Token {
	switch token.Flags {
	case 1:
		return Token{Buff: token.Buff[2:], PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 0}
	case 2:
		return Token{Buff: append([]byte("__UNSAFE_INTERNAL__"), token.Buff...), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column, Flags: 0}
	}
	return token
}

func getStrctDefaultName(strct Token) Token {
	strct.Buff = []byte("d_" + string(strct.Buff))
	strct.Flags = 4
	return strct
}
