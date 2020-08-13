package importer

import (
	"error"
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

func ImportFile(dir string, base string, isMain bool, CompileFile func(File) []byte, AnalyzeFile func(File, string) File) {
	path := Path.Join(dir, base)
	OutPath := Path.Join(dir, "_build", base)

	if isMain {
		OutPath += ".c"
	} else {
		OutPath += ".h"
	}

	Code, err := ioutil.ReadFile(path)

	if err != nil && !isMain {
		Code, err = ioutil.ReadFile(Path.Join(libPath, base))
	}

	if err != nil {
		error.NewGenError("error finding import: " + err.Error())
	}

	f, err := os.Create(OutPath)
	if err != nil {
		error.NewGenError("error creating files: " + err.Error())
	}
	if isMain {
		f.Write(defaultC)
	}

	f.Write(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: append(append(defaultVo, []byte("\n")...), Code...), Line: 1}), path)))
}
