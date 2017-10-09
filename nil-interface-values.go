package main

import (
	"fmt"
)

type I interface {
	M()
}

func describe(i I) {
	fmt.Printf("(%v of %T)\n", i, i)
}

func main() {
	var i I
	describe(i)
	i.M()
}
