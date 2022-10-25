package main

import (
	"bufio"
	"os"

	"github.com/kr/pretty"
)

func main() {
	f, err := os.Open("./Main.class")
	if err != nil {
		panic("error opening class file")
	}

	cf := &ClassFile{}
	cf.Read(bufio.NewReader(f))

	pretty.Println(cf.Classes())
}
