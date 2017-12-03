/**
 * channels are a typed condiut through which you can send and receive values
 * with the channel operator, `<-`.
 *
 *   ch <- v   // send v to channel ch
 *   v := <-ch // receive from ch, and assign value to v
 *
 * by default, sends and receives block until the other side is ready. this
 * allows goroutines to synchronize without explicit locks or condition
 * variables.
 *
 * this example code sums the numbers in a slice, distributing the work between
 * two goroutines. once both goroutines have completed their computation, it
 * calculates the final result.
 */
package main

import (
	"fmt"
	"time"
)

func sum(s []int, c chan<- int) {
	sum := 0

	for _, v := range s {
		sum += v
	}

	c <- sum
}

func main() {
	s := []int{7, 2, 8, -9, 4, 0}
	c := make(chan int)

	go sum(s[:len(s)/2], c)
	time.Sleep(100 * time.Millisecond)
	go sum(s[len(s)/2:], c)

	x, y := <-c, <-c

	fmt.Println(x, y, x*y)
}
