// This is just a test file
package main

import (
	"fmt"
	"io/ioutil"

	"github.com/eankeen/volant/SemanticAnalyzer"
	"github.com/eankeen/volant/compiler"
	"github.com/eankeen/volant/parser"
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
	var s = SemanticAnalyzer.SemanticAnalyzer{}

	c.Buff, _ = ioutil.ReadFile("default.c")

	for p.ReadToken().PrimaryType != parser.EOF {
		stmt := p.ParseGlobalStatement()
		s.Statement(stmt)
		c.GlobalStatement(n.GlobalStatement(stmt))
	}

	fmt.Println(string(c.Buff))
}
