package parser

import (
	"fmt"
	"log"
)

type langError string

const (
	SyntaxError langError = "SyntaxError"
	ParseError  langError = "ParseError"
)

func NewError(err langError, message string, line int, column int) {
	errorstring := string(err) + " on line %d column %d:\n" + message
	log.Fatal(fmt.Sprintf(errorstring, line, column))
}
