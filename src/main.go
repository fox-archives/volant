// This is just a test file
package main

import (
	. "compiler"
	"fmt"
	"io/ioutil"
	. "parser"
	"time"
)

func main() {
	Code, _ := ioutil.ReadFile("test.vo")
	Default, _ := ioutil.ReadFile("default.c")

	fmt.Println(string(Default))

	t1 := time.Now()
	code := string(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: Code}))))
	t2 := time.Now()

	print("Compiling took " + t2.Sub(t1).String() + ".\n\n")
	fmt.Println(code)
}
