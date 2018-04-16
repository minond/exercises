package main

import (
	"errors"
	"fmt"
	"strconv"
	"unicode"
)

type tokenid int
type segment int

type parser struct {
	tokens []token
	pos    int
}

type tokenizer struct {
	chars []rune
	pos   int
}

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

type errStmt struct {
	token token
	error error
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
	argumentMem segment = iota
	constantMem
	localMem
	staticMem
	tempMem
	thatMem
	thisMem
)

const (
	errToken tokenid = iota
	numToken
	pushToken
	popToken
	addToken
	andToken
	argumentToken
	constantToken
	eqToken
	gtToken
	localToken
	ltToken
	negToken
	notToken
	orToken
	staticToken
	subToken
	tempToken
	thatToken
	thisToken
)

const (
	fslashRn = rune('/')
	nilRn    = rune(0)
	nlRn     = rune('\n')
)

var (
	segmentsMap = map[tokenid]segment{
		argumentToken: argumentMem,
		constantToken: constantMem,
		localToken:    localMem,
		staticToken:   staticMem,
		tempToken:     tempMem,
		thatToken:     thatMem,
		thisToken:     thisMem,
	}

	tokensMap = map[string]tokenid{
		"add":      addToken,
		"and":      andToken,
		"argument": argumentToken,
		"constant": constantToken,
		"eq":       eqToken,
		"gt":       gtToken,
		"local":    localToken,
		"lt":       ltToken,
		"neg":      negToken,
		"not":      notToken,
		"or":       orToken,
		"pop":      popToken,
		"push":     pushToken,
		"static":   staticToken,
		"sub":      subToken,
		"temp":     tempToken,
		"that":     thatToken,
		"this":     thisToken,
	}

	tokensPopMem = []tokenid{
		argumentToken,
		constantToken,
		localToken,
		staticToken,
		tempToken,
		thatToken,
		thisToken,
	}

	tokensPushMem = []tokenid{
		argumentToken,
		localToken,
		staticToken,
		tempToken,
		thatToken,
		thisToken,
	}
)

func (pushStmt) asm() []string {
	return []string{}
}

func (popStmt) asm() []string {
	return []string{}
}

func (addStmt) asm() []string {
	return []string{}
}

func (andStmt) asm() []string {
	return []string{}
}

func (eqStmt) asm() []string {
	return []string{}
}

func (gtStmt) asm() []string {
	return []string{}
}

func (ltStmt) asm() []string {
	return []string{}
}

func (negStmt) asm() []string {
	return []string{}
}

func (notStmt) asm() []string {
	return []string{}
}

func (orStmt) asm() []string {
	return []string{}
}

func (subStmt) asm() []string {
	return []string{}
}

func (errStmt) asm() []string {
	return []string{}
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

func (p parser) run() (statements []statement, ok bool) {
	ok = true

	for !p.done() {
		switch p.eat().id {
		case pushToken:
			statements = append(statements, p.parsePushPop(tokensPushMem))
		case popToken:
			statements = append(statements, p.parsePushPop(tokensPopMem))
		case addToken:
			statements = append(statements, addStmt{})
		case andToken:
			statements = append(statements, addStmt{})
		case eqToken:
			statements = append(statements, eqStmt{})
		case gtToken:
			statements = append(statements, gtStmt{})
		case ltToken:
			statements = append(statements, ltStmt{})
		case negToken:
			statements = append(statements, negStmt{})
		case notToken:
			statements = append(statements, notStmt{})
		case orToken:
			statements = append(statements, orStmt{})
		case subToken:
			statements = append(statements, subStmt{})

		case errToken:
			p.eatLine()
			ok = false
			statements = append(statements, errStmt{
				token: p.prev(),
				error: errors.New("Invalid token."),
			})

		default:
			p.eatLine()
			ok = false
			statements = append(statements, errStmt{
				token: p.prev(),
				error: errors.New("Unexpected token."),
			})
		}
	}

	return statements, ok
}

func (p *parser) parsePushPop(memTokens []tokenid) statement {
	segTok, err := p.expect(memTokens...)

	if err != nil {
		p.eatLine()
		return errStmt{
			token: p.curr(),
			error: err,
		}
	}

	str, err := p.expect(numToken)

	if err != nil {
		p.eatLine()
		return errStmt{
			token: p.curr(),
			error: err,
		}
	}

	if str.val == nil {
		p.eatLine()
		return errStmt{
			token: str,
			error: errors.New("Expecting a number value."),
		}
	}

	num, err := strconv.Atoi(*str.val)

	if err != nil {
		p.eatLine()
		return errStmt{
			token: str,
			error: fmt.Errorf("Unable to convert %s to a number.", *str.val),
		}
	}

	seg, ok := segmentsMap[segTok.id]

	if !ok {
		p.eatLine()
		return errStmt{
			token: segTok,
			error: fmt.Errorf("Expecting %q but found %s instead.",
				tokensPushMem, segTok.id),
		}
	}

	return pushStmt{
		seg: seg,
		val: num,
	}
}

func (p parser) done() bool {
	return p.pos >= len(p.tokens)
}

func (p *parser) eat() token {
	next := p.tokens[p.pos]
	p.pos += 1
	return next
}

func (p *parser) eatLine() {
	line := p.eat().line
	for line != 0 && !p.done() && p.curr().line == line {
		p.eat()
	}
}

func (p parser) curr() token {
	return p.tokens[p.pos]
}

func (p parser) prev() token {
	return p.tokens[p.pos-1]
}

func (p parser) expect(ids ...tokenid) (token, error) {
	curr := p.curr()
	for _, id := range ids {
		if curr.id == id {
			return curr, nil
		}
	}

	return token{}, fmt.Errorf("Expecting (one of) %q but found %s instead.",
		ids, curr.id)
}

func tokenize(src string) []token {
	toks := tokenizer{chars: []rune(src)}
	return toks.run()
}

func parse(tokens []token) ([]statement, bool) {
	parse := parser{tokens: tokens}
	return parse.run()
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
push temp 6
add`

	statements, ok := parse(tokenize(sample))
	fmt.Println(ok)
	fmt.Println(len(statements))
	fmt.Println(statements)
}
