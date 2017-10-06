package main

import (
	"fmt"
)

func main() {
	q := []int{2, 3, 5, 7, 11, 13}
	fmt.Println(q)

	r := []bool{true, false, true, true, false, true}
	fmt.Println(r)

	s := []struct {
		i int
		b bool
	}{
		{1, true},
		{2, false},
		{3, true},
		{4, false},
		{5, true},
		{6, false},
	}

	fmt.Println(s)
}
