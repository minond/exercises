package main

import (
	"fmt"
	"strconv"
	"unicode"
)

type tokenId string

type assembler struct {
	pos  int
	stmt []statement
	next int
	addr map[string]int
	lbls map[string]int
}

type scanner struct {
	pos   int
	chars []rune
}

type parser struct {
	pos    int
	tokens []token
}

type token struct {
	id     tokenId
	lexeme string
	pos    int
}

type statement interface {
	error() error
	binary() string
	String() string
}

type label struct {
	err error
	val token
}

type computation struct {
	bit *token
	reg *token
	op  *token
	lhs *computation
	rhs *computation
}

type cinstruction struct {
	err  error
	dest *token
	comp computation
	jump *token
}

type ainstruction struct {
	err error
	val token
}

const (
	andToken    tokenId = "and"
	atToken     tokenId = "at"
	bitToken    tokenId = "bit"
	cparenToken tokenId = "cparen"
	eofToken    tokenId = "eof"
	eolToken    tokenId = "eol"
	eqToken     tokenId = "eq"
	errToken    tokenId = "err"
	idToken     tokenId = "id"
	minusToken  tokenId = "minus"
	notToken    tokenId = "not"
	numToken    tokenId = "num"
	oparenToken tokenId = "oparen"
	orToken     tokenId = "or"
	plusToken   tokenId = "plus"
	scolonToken tokenId = "scolon"

	eofRn    = rune(0)
	fslashRn = rune('/')
	nlRn     = rune('\n')
)

var (
	runeToks = map[rune]tokenId{
		rune('!'): notToken,
		rune('&'): andToken,
		rune('('): oparenToken,
		rune(')'): cparenToken,
		rune('+'): plusToken,
		rune('-'): minusToken,
		rune(';'): scolonToken,
		rune('='): eqToken,
		rune('@'): atToken,
		rune('|'): orToken,
	}

	symbolAddrs = map[string]int{
		"SP":     0,
		"LCL":    1,
		"ARG":    2,
		"THIS":   3,
		"THAT":   4,
		"R0":     0,
		"R1":     1,
		"R2":     2,
		"R3":     3,
		"R4":     4,
		"R5":     5,
		"R6":     6,
		"R7":     7,
		"R8":     8,
		"R9":     9,
		"R10":    10,
		"R11":    11,
		"R12":    12,
		"R13":    13,
		"R14":    14,
		"R15":    15,
		"SCREEN": 16384,
		"KBD":    24576,
	}

	numFn = or(unicode.IsDigit)
	nlFn  = is(nlRn)
	idFn  = or(unicode.IsDigit, unicode.IsLetter,
		is(rune('_')), is(rune('.')), is(rune('$')))
)

func main() {
	instruction := assemble(parse(scan(`

@s
@s
@s
@a

	`)))

	for _, s := range instruction {
		fmt.Println(s)
	}
}

/******************************************************************************
 *
 * Parser
 *
 *****************************************************************************/

func assemble(stmt []statement) []string {
	a := assembler{stmt: stmt}
	a.next = 16
	a.addr = symbolAddrs
	return a.assemble()
}

func (a assembler) assemble() []string {
	var buff []string

	for i, s := range a.stmt {
		switch v := s.(type) {
		case label:
			// XXX Fix offset issue
			a.lbls[v.val.lexeme] = i

		case ainstruction:
			switch v.val.id {
			case idToken:
				a.address(v.val.lexeme)
			}
		}
	}

	for _, s := range a.stmt {
		switch v := s.(type) {
		case ainstruction:
			var addr int

			switch v.val.id {
			case numToken:
				addr, _ = strconv.Atoi(v.val.lexeme)
			case idToken:
				addr = a.address(v.val.lexeme)
			}

			buff = append(buff, fmt.Sprintf("0%0+15s",
				strconv.FormatInt(int64(addr), 2)))
		}
	}

	return buff
}

func (a *assembler) address(id string) int {
	addr, known := a.addr[id]

	if !known {
		addr = a.next
		a.addr[id] = addr
		a.next += 1
	}

	return addr
}

/******************************************************************************
 *
 * Parser
 *
 *****************************************************************************/

func parse(tokens []token) []statement {
	p := parser{tokens: tokens}
	return p.parse()
}

func (l label) String() string {
	if l.err != nil {
		return fmt.Sprintf("(%s) // ERROR: %s", l.val.lexeme, l.err)
	} else {
		return fmt.Sprintf("(%s)", l.val.lexeme)
	}
}

func (l label) binary() string {
	return ""
}

func (l label) error() error {
	return l.err
}

func (i cinstruction) String() string {
	buff := ""

	if i.dest != nil {
		buff += fmt.Sprintf("%s=", i.dest.lexeme)
	}

	buff += fmt.Sprintf("%s", i.comp)

	if i.jump != nil {
		buff += fmt.Sprintf(";%s", i.jump.lexeme)
	}

	if i.err != nil {
		return fmt.Sprintf("    %s // ERROR: %s", buff, i.err)
	} else {
		return fmt.Sprintf("    %s", buff)
	}
}

func (i cinstruction) binary() string {
	return ""
}

func (i cinstruction) error() error {
	return i.err
}

func (i ainstruction) String() string {
	if i.err != nil {
		return fmt.Sprintf("    @%s // ERROR: %s", i.val.lexeme, i.err)
	} else {
		return fmt.Sprintf("    @%s", i.val.lexeme)
	}
}

func (i ainstruction) binary() string {
	return ""
}

func (i ainstruction) error() error {
	return i.err
}

func (c computation) String() string {
	if c.bit != nil {
		return c.bit.lexeme
	} else if c.reg != nil {
		return c.reg.lexeme
	} else if c.lhs != nil && c.rhs != nil && c.op != nil {
		return fmt.Sprintf("%s%s%s", c.lhs, c.op.lexeme, c.rhs)
	} else if c.rhs != nil && c.op != nil {
		return fmt.Sprintf("%s%s", c.op, c.rhs)
	} else {
		return ""
	}
}

func (p parser) parse() []statement {
	var stmt []statement

	for !p.done() {
		if p.is(eolToken, eofToken) {
			p.eat()
		} else if p.is(atToken) {
			stmt = append(stmt, p.skipCheck(p.ainstruction()))
		} else if p.is(oparenToken) {
			stmt = append(stmt, p.skipCheck(p.label()))
		} else {
			stmt = append(stmt, p.skipCheck(p.cinstruction()))
		}
	}

	return stmt
}

func (p *parser) cinstruction() cinstruction {
	ci := cinstruction{}
	eq := p.peek()

	if eq.id == eqToken {
		dest, err := p.expect(idToken)
		// Eat the equals sign
		p.eat()

		if err != nil {
			ci.err = err
			return ci
		}

		ci.dest = &dest
	}

	comp, err := p.computation()
	ci.comp = comp

	if err != nil {
		ci.err = err
		return ci
	}

	if p.is(scolonToken) {
		// Eat semicolon
		p.eat()
		jump, err := p.expect(idToken)

		if err != nil {
			ci.err = err
			return ci
		}

		ci.jump = &jump
	}

	_, err = p.expect(eolToken, eofToken)

	if err != nil {
		ci.err = err
		return ci
	}

	return ci
}

func (p *parser) computation() (computation, error) {
	c := computation{}

	if p.is(notToken, minusToken) {
		// Binary operation
		op := p.curr()
		c.op = &op
		p.eat()

		if p.is(bitToken) {
			bit := p.curr()
			c.bit = &bit
			p.eat()
		} else if p.is(idToken) {
			reg := p.curr()
			c.reg = &reg
			p.eat()
		} else {
			_, err := p.expect(bitToken, idToken)
			return c, err
		}
	} else if p.is(bitToken) {
		// Single bit value
		bit := p.curr()
		c.bit = &bit
		p.eat()
	} else if p.is(idToken) {
		// Either a single register value or a binary operation
		reg := p.curr()
		p.eat()

		if p.is(minusToken, plusToken, andToken, orToken) {
			op := p.curr()
			p.eat()

			rhs, err := p.computation()

			c.op = &op
			c.lhs = &computation{reg: &reg}
			c.rhs = &rhs

			return c, err
		} else {
			c.reg = &reg
		}
	}

	return c, nil
}

func (p *parser) label() label {
	// Eat the opening paren
	p.eat()
	val, err := p.expect(idToken)

	if err != nil {
		return label{err: err}
	}

	_, err = p.expect(cparenToken)

	if err != nil {
		return label{err: err}
	}

	_, err = p.expect(eolToken, eofToken)

	if err != nil {
		return label{err: err}
	}

	return label{err: nil, val: val}
}

func (p *parser) ainstruction() ainstruction {
	// Eat the at-sign
	p.eat()
	val, err := p.expect(idToken, numToken, bitToken)

	if err != nil {
		return ainstruction{err: err}
	}

	_, err = p.expect(eolToken, eofToken)

	if err != nil {
		return ainstruction{err: err}
	}

	return ainstruction{err: nil, val: val}
}

func (p *parser) skipCheck(stmt statement) statement {
	if stmt.error() != nil {
		// Error on last parse, eat until end of current line
		for !p.done() && !p.is(eolToken, eofToken) {
			p.eat()
		}
	}

	return stmt
}

func (p *parser) is(ids ...tokenId) bool {
	curr := p.curr()

	for _, id := range ids {
		if id == curr.id {
			return true
		}
	}

	return false
}

func (p *parser) expect(ids ...tokenId) (token, error) {
	curr := p.curr()

	for _, id := range ids {
		if id == curr.id {
			p.eat()
			return curr, nil
		}
	}

	return token{}, fmt.Errorf("Expecting one of %v but found [%s] instead.",
		ids, curr.id)
}

func (p parser) peek() token {
	if p.pos+1 >= len(p.tokens) {
		return p.tokens[p.pos]
	} else {
		return p.tokens[p.pos+1]
	}
}

func (p parser) curr() token {
	return p.tokens[p.pos]
}

func (p *parser) eat() {
	p.pos += 1
}

func (p parser) done() bool {
	return p.pos >= len(p.tokens)
}

/******************************************************************************
 *
 * Scanner
 *
 *****************************************************************************/

func scan(source string) []token {
	s := scanner{chars: []rune(source)}
	s.chars = append(s.chars, rune(0))
	return s.scan()
}

func (s scanner) scan() []token {
	var tokens []token

	for !s.done() {
		curr := s.curr()

		if curr == nlRn {
			tokens = append(tokens, tok(eolToken, "<eol>", s.pos))
			s.eat()
		} else if curr == eofRn {
			tokens = append(tokens, tok(eofToken, "<eof>", s.pos))
			s.eat()
		} else if id, ok := runeToks[curr]; ok {
			tokens = append(tokens, tok(id, string(curr), s.pos))
			s.eat()
		} else if curr == fslashRn && s.peek() == fslashRn {
			s.takeUntil(nlFn)
			tokens = append(tokens, tok(eolToken, "<eol>", s.pos))
		} else if lexeme := s.takeWhile(numFn); len(lexeme) > 0 {
			if lexeme == "0" || lexeme == "1" {
				tokens = append(tokens, tok(bitToken, lexeme, s.pos))
			} else {
				tokens = append(tokens, tok(numToken, lexeme, s.pos))
			}
		} else if lexeme := s.takeWhile(idFn); len(lexeme) > 0 {
			tokens = append(tokens, tok(idToken, lexeme, s.pos))
		} else if unicode.IsSpace(curr) {
			s.takeWhile(unicode.IsSpace)
		} else {
			tokens = append(tokens, tok(errToken, string(curr), s.pos))
			s.eat()
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

func (s *scanner) eat() {
	s.pos += 1
}

func (s *scanner) takeWhile(f func(rune) bool) string {
	var buff []rune

	for !s.done() {
		if !f(s.curr()) {
			break
		}

		buff = append(buff, s.curr())
		s.eat()
	}

	return string(buff)
}

func (s *scanner) takeUntil(f func(rune) bool) string {
	var buff []rune

	for !s.done() {
		if f(s.curr()) {
			s.eat()
			break
		}

		buff = append(buff, s.curr())
		s.eat()
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

func tok(id tokenId, lexeme string, pos int) token {
	return token{
		id:     id,
		lexeme: lexeme,
		pos:    pos,
	}
}
