package main

import (
	"fmt"
	"time"
)

type String []rune

const (
	M = rune('M')
	I = rune('I')
	U = rune('U')
)

func (s String) String() string {
	return string(s)
}

func (s String) Eq(str String) bool {
	if len(s) != len(str) {
		return false
	}

	for i, c := range str {
		if c != s[i] {
			return false
		}
	}

	return true
}

// Rule I: if you possess a string whose last letter is I, you can add on a U
// at the end.
func RuleI(str String) []String {
	if str[len(str)-1] == I {
		return []String{append(str, U)}
	}
	return []String{}
}

// Rule II: Supposed you have Mx. Then you may add Mxx to your collection.
func RuleII(str String) []String {
	if str[0] == M {
		return []String{append(str, str[1:len(str)]...)}
	}
	return []String{}
}

// Rule III: If III occurs in one of the strings in your collection, you may
// make a new string with U in place of III.
func RuleIII(str String) []String {
	var variation String
	var variations []String

	for i := 1; i+2 < len(str); i++ {
		if str[i : i+3].Eq(String{I, I, I}) {
			if len(str) > i+3 {
				variation = append(str[:i-1], append(String{M}, str[i+3:]...)...)
			} else {
				variation = append(str[:i-1], M)
			}
			variations = append(variations, variation)
		}
	}

	return variations
}

// Rule IV: If UU occurs inside one of your strings, you can drop it.
func RuleIV(str String) []String {
	var variation String
	var variations []String

	for i := 1; i+1 < len(str); i++ {
		if str[i : i+2].Eq(String{U, U}) {
			if len(str) > i+2 {
				variation = append(str[:i-1], str[i+2:]...)
			} else {
				variation = str[:i-1]
			}
			variations = append(variations, variation)
		}
	}

	return variations
}

func main() {
	next := []String{{M, I}}
	curr := []String{}
	secs := 0
	i := 0

	go func() {
		for {
			select {
			case <-time.After(time.Second):
				secs++
				fmt.Printf("Generation %d with %d strings after %d seconds\n", i, len(curr), secs)
			}
		}
	}()

	for ; i < 100; i++ {
		curr = []String{}

		for _, str := range next {
			curr = append(curr, RuleI(str)...)
			curr = append(curr, RuleII(str)...)
			curr = append(curr, RuleIII(str)...)
			curr = append(curr, RuleIV(str)...)
		}

		for _, str := range curr {
			if str.Eq(String{M, U}) {
				println("found match")
				return
			}
		}

		next = curr
	}
}
