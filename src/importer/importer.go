package importer

import (
	"io/ioutil"
	"os"
	. "parser"
	Path "path"
	"error"
)

exPath,err := os.Executable()
if err!=nil {error.NewGenError("error finding volant executable: " + err.Error())}

var libPath = Path.Join(Path.Dir(exPath), "../lib")

wd, err := os.Getwd()
if err!=nil {error.NewGenError("error finding current working directory: " + err.Error())}

defaultVo, err := ioutil.ReadFile(Path.Join(libPath, "internal/default.vo"))
if err!=nil {
	error.NewGenError("error finding volant internals: " + err.Error())
}
defaultC, err := ioutil.ReadFile(Path.Join(libPath, "internal/default.c"))
if err != nil {
	error.NewGenError("error finding volant internals: " + err.Error())
}

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
		Code, err = ioutil.ReadFile(Path.Join(libPath, base))
		if err != nil {
			error.NewGenError("error finding import: " + err.Error())
		}
	}

	f, err := os.Create(OutPath)
	if err!=nil{error.NewGenError("error creating files: " + err.Error())}
	if isMain {
		f.Write(defaultC)
	}

	f.Write(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: defaultVo, Line: 1}), path, isMain)))
	f.Write(CompileFile(AnalyzeFile(ParseFile(&Lexer{Buffer: Code, Line: 1}), path, isMain)))
}
