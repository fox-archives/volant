package error

import (
	"fmt"
	"log"
)

type langError string

func New(message string, line int, column int) {
	errorstring := "Error: line %d column %d: " + message
	log.Fatal(fmt.Sprintf(errorstring, line, column))
}
// for general (non-code) errors
func NewGenError(message string){
	log.Fatal(message)
}
