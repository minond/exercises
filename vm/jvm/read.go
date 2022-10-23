package main

import (
	"bufio"
	"encoding/binary"
)

func read(r *bufio.Reader, size uint32) []byte {
	buf := make([]byte, size)
	r.Read(buf)
	return buf
}

func peek_u8(r *bufio.Reader) uint8 {
	data, err := r.Peek(1)
	if err != nil {
		panic("error reading data from reader")
	}
	return data[0]
}

func read_u8(r *bufio.Reader) uint8 {
	return read(r, 1)[0]
}

func read_u16(r *bufio.Reader) uint16 {
	return binary.BigEndian.Uint16(read(r, 2))
}

func read_u32(r *bufio.Reader) uint32 {
	return binary.BigEndian.Uint32(read(r, 4))
}
