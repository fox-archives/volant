package main

import (
	. "compiler"
	"flag"
	"fmt"
	. "importer"
	"os"
	"os/exec"
	"path"
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
	ImportFile(path.Dir(file), path.Base(file), true, CompileFile, AnalyzeFile)
	out, err := exec.Command("clang", path.Join(path.Dir(file), "_build", path.Base(file)+".c"), "-pthread", "-fblocks", "-lBlocksRuntime", "-lgc").CombinedOutput()

	if err != nil {
		fmt.Println(string(out))
	}
}
