package main

import (
	"fmt"
	"image"
)

func main() {
	m := image.NewRGBA(image.Rect(0, 0, 100, 100))
	fmt.Printf("Bounds() = %q of %T\n", m.Bounds(), m.Bounds())
	fmt.Printf("At() = %q of %T\n", m.At(0, 0), m.At(0, 0))
	fmt.Println(m.At(0, 0).RGBA())
}
