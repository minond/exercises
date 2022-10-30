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

	classes, err := cf.Classes()
	if err != nil {
		panic(err)
	}

	for _, class := range classes {
		fmt.Printf("%s:\n", class.Name.Value)
		for _, method := range class.Methods {
			fmt.Printf("  %s:\n", method.Name.Value)
			method.PrintInstructions()
		}
	}

	fmt.Println("-----------------")
	for _, method := range cf.Methods {
		name, ok := cf.ConstantPool[method.NameIndex-1].(*Utf8Info)
		if !ok {
			panic("...")
		}

		m := Method{Name: name, Impl: []*MethodInfo{method}}
		fmt.Printf("  %s:\n", m.Name.Value)
		m.PrintInstructions()
	}
}
