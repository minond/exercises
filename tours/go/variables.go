package main

import (
	"fmt"
)

var c, python, java bool

var f float32

type X struct {
	empty interface{}
	X     int
}

var empty interface{}
var x X

func main() {
	var i int
	fmt.Println(x, empty, f, i, c, python, java)
}
