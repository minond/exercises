package main

import (
	"golang.org/x/tour/wc"

	"strings"
)

/**
 * Implement WordCount. It should return a map of the counts of each “word” in
 * the string s. The wc.Test function runs a test suite against the provided
 * function and prints success or failure.
 */
func WordCount(s string) map[string]int {
	counter := make(map[string]int)
	words := strings.Fields(s)

	for _, word := range words {
		val, found := counter[word]

		if found {
			counter[word] = val + 1
		} else {
			counter[word] = 1
		}
	}

	return counter
}

func main() {
	wc.Test(WordCount)
}
