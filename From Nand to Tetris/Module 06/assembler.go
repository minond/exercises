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
	andToken    tokenId = "and"
	atToken     tokenId = "at"
	bitToken    tokenId = "bit"
	cparenToken tokenId = "cparen"
	eolToken    tokenId = "eol"
	eqToken     tokenId = "eq"
	errToken    tokenId = "err"
	idToken     tokenId = "id"
	minusToken  tokenId = "minus"
	nlToken     tokenId = "nl"
	notToken    tokenId = "not"
	numToken    tokenId = "num"
	oparenToken tokenId = "oparen"
	orToken     tokenId = "or"
	plusToken   tokenId = "plus"
	scolonToken tokenId = "scolon"
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
	source := `

// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/max/Max.asm

// Computes R2 = max(R0, R1)  (R0,R1,R2 refer to RAM[0],RAM[1],RAM[2])

   @R0
   D=M              // D = first number
   @R1
   D=D-M            // D = first number - second number
   @OUTPUT_FIRST
   D;JGT            // if D>0 (first is greater) goto output_first
   @R1
   D=M              // D = second number
   @OUTPUT_D
   0;JMP            // goto output_d
(OUTPUT_FIRST)
   @R0
   D=M              // D = first number
(OUTPUT_D)
   @R2
   M=D              // M[2] = D (greatest number)
(INFINITE_LOOP)
   @INFINITE_LOOP
   0;JMP            // infinite loop
D=D+A
@0


`

	fmt.Println(scan(source))
}

func scan(source string) []token {
	s := scanner{chars: []rune(source)}
	return s.scan()
}

func (s scanner) scan() []token {
	var tokens []token
	idfn := or(unicode.IsDigit, unicode.IsLetter, is(rune('_')))

	for !s.done() {
		curr := s.curr()

		if unicode.IsSpace(curr) {
			s.consume()
			continue
		} else if id, ok := runeToks[curr]; ok {
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
		} else if curr == rune('/') && s.peek() == rune('/') {
			s.takeUntil(func(r rune) bool {
				return r == rune('\n')
			})
		} else if lexeme := s.takeWhile(idfn); len(lexeme) > 0 {
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

func (s *scanner) takeUntil(f func(rune) bool) string {
	var buff []rune

	for !s.done() {
		if f(s.curr()) {
			s.consume()
			break
		}

		buff = append(buff, s.curr())
		s.consume()
	}

	return string(buff)
}

func or(fs ...func(rune) bool) func(rune) bool {
	return func(r rune) bool {
		for _, f := range fs {
			if f(r) {
				return true
			}
		}

		return false
	}
}

func is(v rune) func(rune) bool {
	return func(r rune) bool {
		return r == v
	}
}
