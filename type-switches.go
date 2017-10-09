package main

import (
	"fmt"
)

func do(i interface{}) {
	switch v := i.(type) {
	case int:
		fmt.Println("An int")

	case string:
		fmt.Println("A string")

	default:
		fmt.Printf("I don't know what %v is\n", v)
	}
}

func main() {
	do(21)
	do("hi")
	do(true)
}
