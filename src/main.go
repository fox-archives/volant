// This is just a test file
package main

import (
	"fmt"
	"parser"
)

var lexer = parser.Lexer{
	Buffer:   []byte("an_identifier 0d010212 0x0102012 2122323 020102 for {} += << | 'k' '™' '􁙶' '\\u2122' \"\\u2122™\" "),
	Position: 0,
	Line:     0,
	Column:   0,
}

func main() {
	for token := lexer.NextToken(); token.PrimaryType != parser.EOF; token = lexer.NextToken() {
		fmt.Println(token.Serialize())
	}
}
