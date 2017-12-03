package main

import (
	"fmt"

	"golang.org/x/tour/tree"
)

func walk(t *tree.Tree, ch chan int) {
	if t != nil {
		walk(t.Left, ch)
		ch <- t.Value
		walk(t.Right, ch)
	}
}

// The function tree.New(k) constructs a randomly-structured (but always
// sorted) binary tree holding the values k, 2k, 3k, ..., 10k. Then read and
// print 10 values from the channel. It should be the numbers 1, 2, 3, ..., 10.
func Walk(t *tree.Tree, ch chan int) {
	walk(t, ch)
	close(ch)
}

// Implement the Same function using Walk to determine whether t1 and t2 store
// the same values. Same(tree.New(1), tree.New(1)) should return true, and
// Same(tree.New(1), tree.New(2)) should return false.
func Same(t1, t2 *tree.Tree) bool {
	ch1 := make(chan int)
	ch2 := make(chan int)

	go Walk(t1, ch1)
	go Walk(t2, ch2)

	for {
		v1, ok1 := <-ch1
		v2, ok2 := <-ch2

		if v1 != v2 || ok1 != ok2 {
			return false
		}

		if !ok1 {
			break
		}
	}

	return true
}

func main() {
	fmt.Println(Same(tree.New(1), tree.New(1)))
	fmt.Println(Same(tree.New(1), tree.New(2)))

	ch := make(chan int)

	go Walk(tree.New(1), ch)

	for v := range ch {
		fmt.Println(v)
	}
}
