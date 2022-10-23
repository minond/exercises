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

	r := bufio.NewReader(f)
	class := Read(r)
	fmt.Println(class)
}
