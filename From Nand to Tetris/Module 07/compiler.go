package main

import (
	"fmt"
	"unicode"
)

type tokenid int
type segment int

type token struct {
	id   tokenid
	line int
	val  *string
}

type statement interface {
	asm() []string
}

type pushStmt struct {
	seg segment
	val int
}

type popStmt struct {
	seg segment
	val int
}

type addStmt struct{}
type andStmt struct{}
type eqStmt struct{}
type gtStmt struct{}
type ltStmt struct{}
type negStmt struct{}
type notStmt struct{}
type orStmt struct{}
type subStmt struct{}

const (
	constantMem segment = iota
	localMem
	argumentMem
	thisMem
	thatMem
	tempMem
	staticMem
)

const (
	errToken tokenid = iota
	numToken
	pushToken
	popToken
	addToken
	andToken
	eqToken
	gtToken
	ltToken
	negToken
	notToken
	orToken
	subToken
	constantToken
	localToken
	argumentToken
	thisToken
	thatToken
	tempToken
	staticToken
)

const (
	nlRn     = rune('\n')
	nilRn    = rune(0)
	fslashRn = rune('/')
)

var (
	tokensMap = map[string]tokenid{
		"push":     pushToken,
		"pop":      popToken,
		"add":      addToken,
		"and":      andToken,
		"eq":       eqToken,
		"gt":       gtToken,
		"lt":       ltToken,
		"neg":      negToken,
		"not":      notToken,
		"or":       orToken,
		"sub":      subToken,
		"constant": constantToken,
		"local":    localToken,
		"argument": argumentToken,
		"this":     thisToken,
		"that":     thatToken,
		"temp":     tempToken,
		"static":   staticToken,
	}
)

func (pushStmt) asm() string {
	return ""
}

func (popStmt) asm() string {
	return ""
}

func (addStmt) asm() string {
	return ""
}

func (andStmt) asm() string {
	return ""
}

func (eqStmt) asm() string {
	return ""
}

func (gtStmt) asm() string {
	return ""
}

func (ltStmt) asm() string {
	return ""
}

func (negStmt) asm() string {
	return ""
}

func (notStmt) asm() string {
	return ""
}

func (orStmt) asm() string {
	return ""
}

func (subStmt) asm() string {
	return ""
}

type tokenizer struct {
	chars []rune
	pos   int
}

func (t tokenizer) run() (tokens []token) {
	line := 1
	isNl := func(r rune) bool {
		return r == nlRn
	}

	for !t.done() {
		if t.curr() == nlRn {
			line += 1
			t.eat()
			continue
		} else if t.curr() == nilRn {
			t.eat()
			continue
		} else if unicode.IsSpace(t.curr()) {
			t.eat()
			continue
		} else if t.curr() == fslashRn && t.peek() == fslashRn {
			t.eatUntil(isNl)
		} else if unicode.IsDigit(t.curr()) {
			val := string(t.eatUntil(unicode.IsSpace))
			tokens = append(tokens, token{
				id:   numToken,
				val:  &val,
				line: line,
			})
		} else if unicode.IsLetter(t.curr()) {
			str := string(t.eatWhile(unicode.IsLetter))
			val := ""
			id, ok := tokensMap[str]

			if !ok {
				val = str
				id = errToken
			}

			tokens = append(tokens, token{id: id, val: &val, line: line})
		} else {
			t.eat()
		}
	}

	return tokens
}

func (t tokenizer) curr() rune {
	return t.chars[t.pos]
}

func (t tokenizer) peek() rune {
	if t.pos+1 < len(t.chars) {
		return t.chars[t.pos+1]
	} else {
		return rune(0)
	}
}

func (t tokenizer) done() bool {
	return t.pos >= len(t.chars)
}

func (t *tokenizer) eat() rune {
	next := t.chars[t.pos]
	t.pos += 1
	return next
}

func (t *tokenizer) eatWhile(f func(rune) bool) (buff []rune) {
	for !t.done() {
		if !f(t.curr()) {
			break
		}

		buff = append(buff, t.eat())
	}

	return buff
}

func (t *tokenizer) eatUntil(f func(rune) bool) (buff []rune) {
	for !t.done() {
		if f(t.curr()) {
			break
		}

		buff = append(buff, t.eat())
	}

	return buff
}

func tokenize(src string) []token {
	toks := tokenizer{chars: []rune(src)}
	return toks.run()
}

func main() {
	sample := `
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/07/MemoryAccess/BasicTest/BasicTest.vm

// Executes pop and push commands using the virtual memory segments.
push constant 10
pop local 0
push constant 21
push constant 22
pop argument 2
pop argument 1
push constant 36
pop this 6
push constant 42
push constant 45
pop that 5
pop that 2
push constant 510
pop temp 6
push local 0
push that 5
add
push argument 1
sub
push this 6
push this 6
add
sub
puh temp 6
add`

	fmt.Println(tokenize(sample))
}
