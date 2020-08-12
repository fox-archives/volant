package importer

import (
	"io/ioutil"
	"os"
	. "parser"
	Path "path"
)

var exPath, _ = os.Executable()
var libPath = Path.Join(Path.Dir(exPath), "../lib")
var wd, _ = os.Getwd()
var defaultVo, _ = ioutil.ReadFile(Path.Join(libPath, "internal/default.vo"))
var defaultC, _ = ioutil.ReadFile(Path.Join(libPath, "internal/default.c"))

func ImportFile(dir string, base string, isMain bool, CompileFile func(File) []byte, AnalyzeFile func(File, string, bool) File) {
	path := Path.Join(dir, base)
	OutPath := Path.Join(dir, "_build", base)

	if isMain {
		OutPath += ".c"
	} else {
		OutPath += ".h"
	}

	var Code []byte
	var err error

	Code, err = ioutil.ReadFile(path)

	if err != nil && !isMain {
		Code, _ = ioutil.ReadFile(Path.Join(libPath, base))
	}

	f, err := os.Create(OutPath)

	if isMain {
		f.Write(defaultC)
	}

	f.Write(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: defaultVo, Line: 1}), path, isMain)))
	f.Write(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: Code, Line: 1}), path, isMain)))
}
