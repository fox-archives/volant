package main

import (
	"fmt"
	"parser"
)

const lexer = parser.Lexer{
	buffer:   " an_identifier 0d010212 0x0102012 2122323 020102 for",
	position: 0,
	line:     0,
	column:   0,
}

func main() {
	fmt.Println("heh")
}
