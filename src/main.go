// This is just a test file
package main

import (
	"compiler"
	"fmt"
	"io/ioutil"
	"parser"
)

func main() {

	dat, _ := ioutil.ReadFile("test.vo")

	var lexer = parser.Lexer{
		Buffer: dat,
	}
	var p = parser.Parser{
		Lexer: &lexer,
		Forks: map[byte]int{},
	}
	var n = compiler.Normalizer{}
	var c = compiler.Compiler{
		ScopeCount: 0,
		Buff:       []byte(""),
	}

	c.Buff, _ = ioutil.ReadFile("default.c")

	for p.ReadToken().PrimaryType != parser.EOF {
		c.GlobalStatement(n.GlobalStatement(p.ParseGlobalStatement()))
	}

	fmt.Println(string(c.Buff))
}
