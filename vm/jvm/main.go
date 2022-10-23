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
	cf := Read(r)

	for _, info := range cf.ConstantPool {
		classInfo, ok := info.(*ClassInfo)
		if !ok {
			continue
		}

		utf8Info, ok := cf.ConstantPool[classInfo.NameIndex-1].(*Utf8Info)
		if !ok {
			panic(fmt.Sprintf("expecting utf8_info"))
		}

		fmt.Println(utf8Info.Value)
		fmt.Println(classInfo)
	}
	fmt.Println(cf)
}
