package main

import (
	"fmt"
	"math"
	"time"
)

type MathError struct {
	When time.Time
	What string
}

func (e *MathError) Error() string {
	return fmt.Sprintf("At %v, %s", e.When, e.What)
}

func Sqrt(x float64) (float64, error) {
	if x < 0 {
		return 0, &MathError{
			time.Now(),
			"negatives not allowed",
		}
	}

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

	return z, nil
}

func main() {
	for i := 0; i < 10; i++ {
		fmt.Println(Sqrt(2))
		fmt.Println(Sqrt(-2))
	}
}
