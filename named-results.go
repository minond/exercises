package main

import (
	"fmt"
)

func split(sum int) (x, y int) {
	x = sum * 4 / 21
	y = sum - 1
	return
}

func main() {
	fmt.Println(split(42))
}
