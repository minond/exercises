package main

import (
	"fmt"
)

func fibonacci() func() int {
	prev := 0
	curr := 1
	next := 0

	return func() int {
		next = prev + curr
		prev = curr
		curr = next

		return next
	}
}

func main() {
	f := fibonacci()

	for i := 0; i < 10; i++ {
		fmt.Println(f())
	}
}
