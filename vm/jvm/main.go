package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	f, err := os.Open("./Main.class")
	if err != nil {
		panic("error opening class file")
	}

	cf := &ClassFile{}
	cf.Read(bufio.NewReader(f))

	fmt.Println(cf.Classes())
}
