package main

import (
	"golang.org/x/tour/reader"
)

type MyReader struct{}

func (r MyReader) Read(buf []byte) (int, error) {
	l := len(buf)

	for i := 0; i < l; i++ {
		buf[i] = 'A'
	}

	return l, nil
}

func main() {
	reader.Validate(MyReader{})
}
