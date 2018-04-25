package main

import (
	"testing"
)

func TestTokenizerWithEmptyString(t *testing.T) {
	if len(tokenize("")) != 0 {
		t.Error("Empty string did return empty slice")
	}
}

func TestTokenizerWithJustWhiteSpace(t *testing.T) {
	if len(tokenize(`
	
	
	
	
	`)) != 0 {
		t.Error("Empty string did return empty slice")
	}
}

func TestTokenizerWithJustComments(t *testing.T) {
	if len(tokenize(`
	
// comment
// comment
// comment
// comment`)) != 0 {
		t.Error("Empty string did return empty slice")
	}
}

func TestTokenizerHandlesPush(t *testing.T) {
	toks := tokenize(`push constant 99`)
	ok := toks[0].id == pushToken &&
		toks[1].id == constantToken &&
		toks[2].id == numToken &&
		*toks[2].val == "99"

	if !ok {
		t.Error("Empty string did return empty slice")
	}
}
