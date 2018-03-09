package main

import (
	"fmt"
	"unicode"
)

type tokenId string

type scanner struct {
	pos   int
	chars []rune
}

type token struct {
	id     tokenId
	lexeme string
	pos    int
}

const (
	andToken     tokenId = "and"
	atToken      tokenId = "at"
	bitToken     tokenId = "bit"
	commentToken tokenId = "comment"
	cparenToken  tokenId = "cparen"
	eolToken     tokenId = "eol"
	eqToken      tokenId = "eq"
	errToken     tokenId = "err"
	idToken      tokenId = "id"
	minusToken   tokenId = "minus"
	nlToken      tokenId = "nl"
	notToken     tokenId = "not"
	numToken     tokenId = "num"
	oparenToken  tokenId = "oparen"
	orToken      tokenId = "or"
	plusToken    tokenId = "plus"
	scolonToken  tokenId = "scolon"
)

var (
	runeToks = map[rune]tokenId{
		rune('!'):  notToken,
		rune('&'):  andToken,
		rune('('):  oparenToken,
		rune(')'):  cparenToken,
		rune('+'):  plusToken,
		rune('-'):  minusToken,
		rune('0'):  bitToken,
		rune('1'):  bitToken,
		rune(';'):  scolonToken,
		rune('='):  eqToken,
		rune('@'):  atToken,
		rune('\n'): nlToken,
		rune('|'):  orToken,
	}
)

func main() {
	source := `@2
D=A
@3
D=D+A
@0
M=D`

	fmt.Println(scan(source))
}

func scan(source string) []token {
	s := scanner{chars: []rune(source)}
	return s.scan()
}

func (s scanner) scan() []token {
	var tokens []token

	for !s.done() {
		curr := s.curr()

		if id, ok := runeToks[curr]; ok {
			lexeme := string(curr)

			if id == nlToken {
				lexeme = "<nl>"
			}

			tokens = append(tokens, token{
				pos:    s.pos,
				id:     id,
				lexeme: lexeme,
			})

			s.consume()
		} else if lexeme := s.takeWhile(unicode.IsLetter); len(lexeme) > 0 {
			tokens = append(tokens, token{
				pos:    s.pos,
				id:     idToken,
				lexeme: lexeme,
			})
		} else {
			tokens = append(tokens, token{
				pos:    s.pos,
				id:     errToken,
				lexeme: string(curr),
			})
			s.consume()
		}
	}

	return tokens
}

func (s scanner) done() bool {
	return s.pos >= len(s.chars)
}

func (s scanner) curr() rune {
	if s.done() {
		return rune(0)
	} else {
		return s.chars[s.pos]
	}
}

func (s scanner) peek() rune {
	if s.pos+1 >= len(s.chars) {
		return rune(0)
	} else {
		return s.chars[s.pos+1]
	}
}

func (s *scanner) consume() {
	s.pos += 1
}

func (s *scanner) takeWhile(f func(rune) bool) string {
	var buff []rune

	for !s.done() {
		if !f(s.curr()) {
			break
		}

		buff = append(buff, s.curr())
		s.consume()
	}

	return string(buff)
}
