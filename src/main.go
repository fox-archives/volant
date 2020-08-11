// This is just a test file
package main

import (
	. "compiler"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	. "parser"
	"time"
)

func main() {
	fileName := flag.String("compile", "", "file to be compiled")
	flag.Parse()
	if *fileName == "" {
		fmt.Println("file name not given")
		os.Exit(1)
	}
	path, _ := os.Getwd()
	Code, _ := ioutil.ReadFile(path + "/" + *fileName)
	f, _ := os.Create(path + "/test.c")
	f.Write(Default)

	t1 := time.Now()
	_, err := f.Write(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: Code, Line: 1}))))
	t2 := time.Now()

	out, err := exec.Command("clang", "test.c", "-pthread", "-fblocks", "-lBlocksRuntime", "-lgc").CombinedOutput()
	if err != nil {
		fmt.Println(string(out))
	}
	t3 := time.Now()
	//exec.Command("rm", "test.c").Run()
	fmt.Println("Compiling to C took " + t2.Sub(t1).String())
	fmt.Println("Compiling from C took " + t3.Sub(t2).String())

}
