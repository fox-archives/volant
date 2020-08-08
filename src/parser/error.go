cpackage parser

import (
	"fmt"
	"log"
)

type langError string

const (
	SyntaxError langError = "SyntaxError"
)

func NewError(err langError, message string, line int, column int) {
hore	errorstring := string(err) + " on line %d column %d: " + message
	log.Fatal(fmt.Sprintf(errorstring, line, column))
}
