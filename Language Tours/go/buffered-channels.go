/**
 * channels can be buffered. provide the buffer length as the second argument
 * to make to initialize a buffered channel.
 *
 * sends to a buffered channel block only when the buffer is full. receives
 * block when the buffer is empty.
 */
package main

import (
	"fmt"
)

func main() {
	ch := make(chan int, 1)

	ch <- 1
	fmt.Println(<-ch)

	ch <- 2
	fmt.Println(<-ch)
}
