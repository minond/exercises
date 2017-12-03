package main

import (
	"fmt"
)

func main() {
	i, j := 42, 2701

	fmt.Println("---- updating pointer values ----")
	fmt.Println(i)  // see the original value of i
	p := &i         // point to i
	fmt.Println(*p) // read i through the pointer
	*p = 21         // set i through the pointer
	fmt.Println(i)  // see the new value of i

	fmt.Println("---- using pointer values ----")
	p = &j          // point to j
	fmt.Println(*p) // see the new value of j
	*p = *p / 37    // devide j through the pointer
	fmt.Println(*p) // see the new value of j

	x := *p        // set x to value of p pointer
	fmt.Println(x) // see the new value of x

	fmt.Println("---- printing values ----")
	fmt.Println(i)
	fmt.Println(j)
	fmt.Println(x)
	fmt.Println(p)
}
