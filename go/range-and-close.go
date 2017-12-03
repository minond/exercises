/**
 * a sender can close a channel to indicate that no more values will be sent.
 * receivers can test whether a channel has been closed by assigning a second
 * parameter to the receive expression:
 *
 *     v, ok := <-ch
 *
 * ok if false if there are no more values to receive and the channel is
 * closed.
 *
 * the loop receieves values from the channel repeatedly until it is closed.
 *
 * note: only the sender should close a channel, never the received. sending on
 * a closed channel will cause a panic
 *
 * note: channels aren't like files; you don't usually need to close them.
 * closing is only necessary when the receiver must be told there are no more
 * values coming, such as to therminate a range loop
 */
package main

import (
	"fmt"
)

func fibonacci(n int, c chan int) {
	x, y := 0, 1

	for i := 0; i < n; i++ {
		c <- x
		x, y = y, x+y
	}

	close(c)
}

func main() {
	c := make(chan int, 10)
	go fibonacci(cap(c), c)

	for v := range c {
		fmt.Println(v)
	}
}
