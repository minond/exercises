package main

import "fmt"

type Vertex struct {
	Lat, Long float64
}

var m1 = map[string]interface{}{
	"Bell Labs": Vertex{40.68433, -74.39967},
	"Google":    Vertex{37.42202, -122.08408},
}

var m2 = map[string]Vertex{
	"Bell Labs": Vertex{40.68433, -74.39967},
	"Google":    Vertex{37.42202, -122.08408},
}

func main() {
	fmt.Printf("%q\n", m1)
	fmt.Printf("%q\n", m2)
}
