// This is just a test file
package main

import (
	. "compiler"
	"fmt"
	"io/ioutil"
	. "parser"
)

func main() {
	Code, _ := ioutil.ReadFile("test.vo")
	Default, _ := ioutil.ReadFile("default.c")

	fmt.Println(string(Default))
	fmt.Println(string(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: Code})))))
}
