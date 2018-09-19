package main

import "fmt"

func djb2(str string) uint64 {
	var hash uint64 = 5381

	for _, c := range str {
		hash = ((hash << 5) + hash) + uint64(c)
	}

	return hash
}

func sdbm(str string) uint64 {
	var hash uint64 = 0

	for _, c := range str {
		hash = uint64(c) + (hash << 6) + (hash << 16) - hash
	}

	return hash
}

func main() {
	fmt.Println(djb2("123"))
	fmt.Println(sdbm("123"))
}
