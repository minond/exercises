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
	seg  segment
	val  int
	line int
}

type popStmt struct {
	seg  segment
	val  int
	line int
}

type errStmt struct {
	token token
	error error
	line  int
}

type addStmt struct{ line int }
type andStmt struct{ line int }
type eqStmt struct{ line int }
type gtStmt struct{ line int }
type ltStmt struct{ line int }
type negStmt struct{ line int }
type notStmt struct{ line int }
type orStmt struct{ line int }
type subStmt struct{ line int }

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
	eolToken
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
	currId = 0

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
		localToken,
		staticToken,
		tempToken,
		thatToken,
		thisToken,
	}

	tokensPushMem = []tokenid{
		argumentToken,
		constantToken,
		localToken,
		staticToken,
		tempToken,
		thatToken,
		thisToken,
	}
)

func (t token) String() string {
	if t.id == eolToken {
		return "EOL"
	} else if t.val == nil || *t.val == "" {
		return fmt.Sprintf("%s on line %d", t.id, t.line)
	} else {
		return fmt.Sprintf(`%s ("%s") on line %d`, t.id, *t.val, t.line)
	}
}

func (id tokenid) String() string {
	switch id {
	case errToken:
		return "error"
	case numToken:
		return "number"
	case pushToken:
		return "push"
	case popToken:
		return "pop"
	case addToken:
		return "add"
	case andToken:
		return "and"
	case argumentToken:
		return "argument"
	case constantToken:
		return "constant"
	case eqToken:
		return "eq"
	case gtToken:
		return "gt"
	case localToken:
		return "local"
	case ltToken:
		return "lt"
	case negToken:
		return "neg"
	case notToken:
		return "not"
	case orToken:
		return "or"
	case staticToken:
		return "static"
	case subToken:
		return "sub"
	case tempToken:
		return "temp"
	case thatToken:
		return "that"
	case thisToken:
		return "this"
	case eolToken:
		return "EOL"
	default:
		panic(fmt.Sprintf("invalid token id: %d", id))
	}
}

func (s segment) String() string {
	switch s {
	case argumentMem:
		return "argument"
	case constantMem:
		return "constant"
	case localMem:
		return "local"
	case staticMem:
		return "static"
	case tempMem:
		return "temp"
	case thatMem:
		return "that"
	case thisMem:
		return "this"
	default:
		panic(fmt.Sprintf("invalid segment id: %d", s))
	}
}

// TODO
func (s pushStmt) asm() []string {
	header := comment("line %03d: push %s %d", s.line, s.seg, s.val)
	switch s.seg {
	case argumentMem:
	case constantMem:
		return pushOp(header, []string{
			fmt.Sprintf("@%d", s.val),
			"D=A",
		})
	case localMem:
	case staticMem:
	case tempMem:
	case thatMem:
	case thisMem:
	}

	return []string{comment("error, unimplemented push")}
}

// TODO
func (s popStmt) asm() []string {
	return []string{
		comment("line %03d: pop %s %d", s.line, s.seg, s.val),
	}
}

func (s addStmt) asm() []string {
	return binOp(comment("line %03d: add", s.line),
		[]string{"M=D+M"})
}

func (s andStmt) asm() []string {
	return binOp(comment("line %03d: and", s.line),
		[]string{"M=D&M"})
}

func (s eqStmt) asm() []string {
	id := nextId()
	header := comment("line %03d: eq (%d)", s.line, id)
	return compOp(id, "JEQ", header)
}

func (s gtStmt) asm() []string {
	id := nextId()
	header := comment("line %03d: eq (%d)", s.line, id)
	return compOp(id, "JGT", header)
}

func (s ltStmt) asm() []string {
	id := nextId()
	header := comment("line %03d: eq (%d)", s.line, id)
	return compOp(id, "JLT", header)
}

func (s negStmt) asm() []string {
	return binOp(comment("line %03d: neg", s.line),
		[]string{"M=-M"})
}

func (s notStmt) asm() []string {
	return binOp(comment("line %03d: not", s.line),
		[]string{"M=!M"})
}

func (s orStmt) asm() []string {
	return binOp(comment("line %03d: or", s.line),
		[]string{"M=D|M"})
}

func (s subStmt) asm() []string {
	return binOp(comment("line %03d: sub", s.line),
		[]string{"M=D-M"})
}

func (s errStmt) asm() []string {
	id := nextId()
	return []string{
		comment("Error: %v, %s\n", s.error, s.token),
		fmt.Sprintf("(ERROR.%d)", id),
		fmt.Sprintf("@ERROR.%d", id),
		"0; JMP",
	}
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
			statements = append(statements, p.parsePushPop(true, tokensPushMem))
		case popToken:
			statements = append(statements, p.parsePushPop(false, tokensPopMem))
		case addToken:
			statements = append(statements, addStmt{p.prev().line})
		case andToken:
			statements = append(statements, addStmt{p.prev().line})
		case eqToken:
			statements = append(statements, eqStmt{p.prev().line})
		case gtToken:
			statements = append(statements, gtStmt{p.prev().line})
		case ltToken:
			statements = append(statements, ltStmt{p.prev().line})
		case negToken:
			statements = append(statements, negStmt{p.prev().line})
		case notToken:
			statements = append(statements, notStmt{p.prev().line})
		case orToken:
			statements = append(statements, orStmt{p.prev().line})
		case subToken:
			statements = append(statements, subStmt{p.prev().line})

		case errToken:
			line := p.prev().line
			p.eatLine()
			ok = false
			statements = append(statements, errStmt{
				token: p.prev(),
				error: errors.New("invalid token"),
				line:  line,
			})

		default:
			line := p.prev().line
			p.eatLine()
			ok = false
			statements = append(statements, errStmt{
				token: p.prev(),
				error: errors.New("unexpected token"),
				line:  line,
			})
		}
	}

	return statements, ok
}

func (p *parser) parsePushPop(isPush bool, memTokens []tokenid) statement {
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
			error: errors.New("expecting a number value"),
		}
	}

	num, err := strconv.Atoi(*str.val)

	if err != nil {
		p.eatLine()
		return errStmt{
			token: str,
			error: fmt.Errorf("unable to convert %s to a number", *str.val),
		}
	}

	seg, ok := segmentsMap[segTok.id]

	if !ok {
		p.eatLine()
		return errStmt{
			token: segTok,
			error: fmt.Errorf("expecting %v but found [%s] instead",
				tokensPushMem, segTok.id),
		}
	}

	if isPush {
		return pushStmt{
			seg:  seg,
			val:  num,
			line: segTok.line,
		}
	} else {
		return popStmt{
			seg:  seg,
			val:  num,
			line: segTok.line,
		}
	}
}

func (p parser) done() bool {
	return p.pos >= len(p.tokens)
}

func (p *parser) eat() token {
	if p.done() {
		return token{eolToken, -1, nil}
	}

	next := p.tokens[p.pos]
	p.pos += 1
	return next
}

func (p *parser) eatLine() {
	if p.done() {
		return
	}
	line := p.eat().line
	for line != 0 && !p.done() && p.curr().line == line {
		p.eat()
	}
}

func (p parser) curr() token {
	if p.done() {
		return token{eolToken, -1, nil}
	} else {
		return p.tokens[p.pos]
	}
}

func (p parser) prev() token {
	return p.tokens[p.pos-1]
}

func (p *parser) expect(ids ...tokenid) (token, error) {
	curr := p.curr()
	for _, id := range ids {
		if curr.id == id {
			p.eat()
			return curr, nil
		}
	}

	return token{}, fmt.Errorf("expecting %v but found [%s] instead",
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

func compile(stmts []statement) (code []string) {
	for _, stmt := range stmts {
		code = append(code, stmt.asm()...)
	}
	return
}

func nextId() int {
	currId++
	return currId
}

func pushOp(header string, code []string) []string {
	push := []string{
		"@SP",   // Load top of stack
		"M=M+1", // Increment address by 1
		"A=M-1", // Point address reg to old SP
		"M=D",   // Save the value of D in RAM[M]
	}

	return append(append([]string{header}, code...), push...)
}

func spdecOp() []string {
	return []string{
		"@SP",   // Load top of stack
		"A=M",   // Save the address
		"D=A-1", // Decrement the address by 1
		"@SP",   // Reload top of stack
		"M=D",   // Set memory value to D
	}
}

func binOp(header string, code []string) []string {
	return append(append([]string{
		header,
		"@SP",   // Load top of stack
		"D=M",   // Into D
		"A=A-1", // Point to previous memory cell. We're still pointing to SP
	}, code...), spdecOp()...)
}

func compOp(id int, comp, header string) []string {
	return binOp(header, []string{
		"D=D+M", // Subtract D from value in D
		fmt.Sprintf("@EQ.%d", id),
		fmt.Sprintf("D; %s", comp), // Perform jump

		"@SP",   // Load top of stack
		"A=A+1", // Point to next cell
		"M=0",   // Set it to 0

		fmt.Sprintf("@DONE.%d", id),
		"0; JMP", // Go to DONE

		fmt.Sprintf("(EQ.%d)", id),
		"@SP",   // Load top of stack
		"A=A+1", // Point to next cell
		"M=1",   // Set it to 1

		fmt.Sprintf("(DONE.%d)", id),
	})
}

func comment(str string, args ...interface{}) string {
	return fmt.Sprintf(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; "+str, args...)
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
eq
push temp 6
add`

	statements, _ := parse(tokenize(sample))
	for _, line := range compile(statements) {
		fmt.Println(line)
	}
}
