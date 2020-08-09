package compiler

import . "parser"

func getEnumPropName(enumName []byte, propName []byte) []byte {
    name := "enum_" + string(enumName) + "_" + string(propName)
	return []byte(name)
}

func getVarName(token Token) Token {
	return Token{Buff: []byte("v_"+string(token.Buff)), PrimaryType: Identifier, SecondaryType: SecondaryNullType, Line: token.Line, Column: token.Column} 
}

