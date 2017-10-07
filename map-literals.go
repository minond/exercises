package main

import (
	"fmt"
)

type Vertex struct {
	Lat, Long float64
}

var m = map[string]Vertex{
	"Bell Labs": Vertex{
		32.324, 54.654,
	},

	"Google": Vertex{
		123.4343, 23.234,
	},
}

func main() {
	fmt.Println(m)
}
