package parser

import (
	"fmt"
	"strconv"

	"github.com/minond/exercises/vm/lisp/vm/lang"
)

func Parse(code string) ([]lang.Expr, error) {
	tokens := lex(code)
	p := parser{
		tokens: tokens,
		len:    len(tokens),
		pos:    0,
	}

	return p.do()
}

/**
 * main			 = epxr*
 *               ;
 *
 * expr          = "(" expr ")"
 *               | "'" expr
 *               | identifier
 *               | number
 *               | string
 *               | boolean
 *               ;
 *
 * boolean       = "#t"
 *               | "#f"
 *               ;
 *
 * identifier    = ?? identifier ??
 *               ;
 *
 * number        = ?? number ??
 *               ;
 *
 * string        = ?? string ??
 *               ;
 */
type parser struct {
	tokens []Token
	len    int
	pos    int
}

func (p parser) done() bool {
	return p.pos >= p.len
}

func (p *parser) eat() {
	p.pos++
}

func (p parser) curr() Token {
	return p.tokens[p.pos]
}

func (p parser) currEq(o Token) bool {
	return p.tokens[p.pos].Eq(o)
}

func (p parser) currIsA(o tok) bool {
	return p.tokens[p.pos].IsA(o)
}

func (p *parser) expectA(t tok) error {
	if p.done() {
		return fmt.Errorf("expected %v but reached eof", t)
	}

	curr := p.curr()
	if !curr.IsA(t) {
		return fmt.Errorf("expected %v but found %v", t, curr.tok)
	}

	return nil
}

func (p *parser) expectEq(t Token) error {
	if p.done() {
		return fmt.Errorf("expected %v but reached eof", t)
	}

	curr := p.curr()
	if !curr.Eq(t) {
		return fmt.Errorf("expected %v but found %v", t, curr.tok)
	}

	return nil
}

func (p *parser) do() ([]lang.Expr, error) {
	var buff []lang.Expr

	for !p.done() {
		part, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		buff = append(buff, part)
	}

	return buff, nil
}

func (p *parser) parseExpr() (lang.Expr, error) {
	if p.currEq(tokenOpenParen) {
		return p.parseSexpr()
	} else if p.currEq(tokenQuote) {
		return p.parseQuote()
	} else if p.currIsA(tokWord) {
		return p.parseIdentifier()
	} else if p.currIsA(tokNumber) {
		return p.parseNumber()
	} else if p.currIsA(tokString) {
		return p.parseString()
	} else if p.currIsA(tokBoolean) {
		return p.parseBoolean()
	}

	return nil, fmt.Errorf("invalid syntax: %v", p.curr())
}

func (p *parser) parseSexpr() (*lang.Sexpr, error) {
	if err := p.expectEq(tokenOpenParen); err != nil {
		return nil, err
	}

	p.eat() // Eat the open paren

	var values []lang.Expr
	for !p.done() && !p.currEq(tokenCloseParen) {
		val, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		values = append(values, val)
	}

	if err := p.expectEq(tokenCloseParen); err != nil {
		return nil, err
	}

	p.eat() // Eat the closing paren

	return lang.NewSexpr(values...), nil
}

func (p *parser) parseQuote() (*lang.Quote, error) {
	if err := p.expectEq(tokenQuote); err != nil {
		return nil, err
	}

	p.eat() // Eat the quote

	val, err := p.parseExpr()
	if err != nil {
		return nil, err
	}

	return lang.NewQuote(val), nil
}

func (p *parser) parseIdentifier() (*lang.Identifier, error) {
	if err := p.expectA(tokWord); err != nil {
		return nil, err
	}

	curr := p.curr()
	p.eat() // Eat the id

	return lang.NewIdentifier(string(curr.lexeme)), nil
}

func (p *parser) parseNumber() (*lang.Number, error) {
	if err := p.expectA(tokNumber); err != nil {
		return nil, err
	}

	curr := p.curr()
	p.eat() // Eat the number

	val, err := strconv.ParseFloat(string(curr.lexeme), 64)
	if err != nil {
		return nil, fmt.Errorf("invalid number: %v", curr)
	}

	return lang.NewNumber(val), nil
}

func (p *parser) parseString() (*lang.String, error) {
	if err := p.expectA(tokString); err != nil {
		return nil, err
	}

	curr := p.curr()
	p.eat() // Eat the string

	return lang.NewString(string(curr.lexeme)), nil
}

func (p *parser) parseBoolean() (*lang.Boolean, error) {
	if err := p.expectA(tokBoolean); err != nil {
		return nil, err
	}

	curr := p.curr()
	p.eat() // Eat the boolean

	var val bool
	switch string(curr.lexeme) {
	case "#t":
		val = true
	case "#f":
		val = false
	default:
		return nil, fmt.Errorf("invalid boolean: %v", curr)
	}

	return lang.NewBoolean(val), nil
}
