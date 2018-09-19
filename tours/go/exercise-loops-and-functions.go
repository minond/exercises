/**
 * as a simple way to play wiht functions and loops, implement the square root
 * function using Newton's method.
 *
 * In this case, Newton's method is to approximate `Sqrt(x)` by picking a
 * starting point `z` and the repeating:
 *
 * z^n+1 = Z^n - ((z^2^n - x) / 2 * z^n)
 *
 * To begin with, just repeat that calculation 10 times and see how close you
 * get to the answer for various values.
 *
 * Next, change the loop condition to stop once the value has stopped changing
 * (or only changes by a very small delta). See if that's ore of fewer
 * iterations. How close are you to the `math.Sqrt`?
 */
package main

import (
	"fmt"
	"math"
)

func SqrtWithLoop(x float64) float64 {
	i := 0
	z := 1.0

	if x == 0 {
		return 0
	}

	for i < 10 {
		i++

		z = z - (z*z-x)/(2*x)
	}

	return z
}

func SqrtWithDeltal(x float64) float64 {
	calc := func(z float64) float64 {
		return z - (z*z-x)/(2*x)
	}

	delta := 0.0001
	z := calc(1.0)
	next := calc(z)

	for math.Abs(next-z) > delta {
		z = next
		next = calc(z)
	}

	return z
}

func main() {
	i := 0

	for i < 100 {
		fmt.Println(
			i,
			math.Sqrt(float64(i)),
			SqrtWithLoop(float64(i)),
			SqrtWithDeltal(float64(i)),
		)

		i++
	}
}
