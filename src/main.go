// This is just a test file
package main

import (
	. "compiler"
	"flag"
	"fmt"
	. "importer"
	"os"
	"os/exec"
	"path"
	"time"
)

var exPath, _ = os.Executable()
var libPath = path.Join(path.Dir(exPath), "../lib")

func main() {
	fileName := flag.String("compile", "", "file to be compiled")
	flag.Parse()

	file := path.Clean(*fileName)

	if file == "" {
		fmt.Println("file name not given")
		os.Exit(1)
	}

	t1 := time.Now()
	ImportFile(path.Dir(file), path.Base(file), true, CompileFile, AnalyzeFile)
	t2 := time.Now()

	out, err := exec.Command("clang", "-I"+libPath, path.Join(path.Dir(file), "_build", path.Base(file)+".c"), "-pthread", "-fblocks", "-lBlocksRuntime", "-lgc").CombinedOutput()
	t3 := time.Now()

	if err != nil {
		fmt.Println(string(out))
	}

	fmt.Println("Compiling to C took " + t2.Sub(t1).String())
	fmt.Println("Compiling from C took " + t3.Sub(t2).String())
}
