package main

import (
	"io"
	"os"
	"strings"
)

type rot13Reader struct {
	r io.Reader
}

func (r rot13Reader) Read(buf []byte) (int, error) {
	c, err := r.r.Read(buf)

	for i := 0; i < c; i++ {
		letter := buf[i]
		update := letter

		if letter >= 'A' && letter <= 'z' {
			update = buf[i] + 13

			if update > 'z' {
				update = 'A' - 1 + (update - 'z')
			}
		}

		buf[i] = update
	}

	return c, err
}

func main() {
	s := strings.NewReader("Lbh penpxrq gur pbqr!")
	r := rot13Reader{s}

	io.Copy(os.Stdout, &r)
}
