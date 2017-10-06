package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("When is Saturday?")
	today := time.Now().Weekday()

	switch time.Saturday {
	case today + 0:
		fmt.Println("Today.")

	case today + 1:
		fmt.Println("Tomorrow.")

	default:
		fmt.Println("Too far away.")

	case today + 4:
		fmt.Println("In two days.")
	}
}
