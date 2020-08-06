// This is just a test file
package main

import (
	"compiler"
	"fmt"
	"io/ioutil"
	"parser"
)

func main() {

	dat, _ := ioutil.ReadFile("test.um")

	var lexer = parser.Lexer{
		Buffer: dat,
	}
	var p = parser.Parser{
		Lexer: &lexer,
		Forks: map[byte]int{},
	}
	var c = compiler.Compiler{
		ScopeCount: 0,
		Buff:       []byte(""),
	}

	c.Buff, _ = ioutil.ReadFile("deafult.c")

	for p.ReadToken().PrimaryType != parser.EOF {
		c.Statement(p.ParseGlobalStatement())
	}

	fmt.Println(string(c.Buff))
}
