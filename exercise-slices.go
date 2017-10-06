package main

import (
	"golang.org/x/tour/pic"
)

func Pic(dx, dy int) [][]uint8 {
	rows := make([][]uint8, dy)

	for y := range rows {
		row := make([]uint8, dx)
		rows[y] = row

		for x := range row {
			row[x] = uint8(x ^ 42)
		}
	}

	return rows
}

func main() {
	pic.Show(Pic)
}
