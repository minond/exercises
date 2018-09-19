package main

import (
	"fmt"
)

type Vertex struct {
	X int
	Y int
}

func main() {
	v := Vertex{1, 2}

	fmt.Println("---- normal reference updates ----")
	p1 := v
	p1.Y = 1e9
	fmt.Printf("v = %v\n", v)
	fmt.Printf("p1 = %v\n", p1)

	fmt.Println("---- pointer reference updates ----")
	p2 := &v
	p2.X = 1e9
	fmt.Printf("v = %v\n", v)
	fmt.Printf("p2 = %v\n", p2)
	fmt.Printf("*p2 = %v\n", *p2)

	fmt.Printf("&p2 = %v\n", &p2)
}
