package main

import (
	"bufio"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

//
// MAIN
//

func show(prog string) {
	lbl := func(str string) {
		fmt.Printf("\n  %s:\n", str)
	}

	toks := Lex(prog)
	expr, errs := Parse(toks)

	lbl("Tokens")
	for _, tok := range toks {
		fmt.Printf("    - %s\n", tok)
	}
	fmt.Print("\n")

	if len(errs) > 0 {
		lbl("Errors")
		for _, err := range errs {
			fmt.Printf("    - %s\n", err)
		}
		fmt.Print("\n")
	}

	lbl("Parsed")
	fmt.Printf("\n    %s\n\n", expr)
}

func repl() {
	reader := bufio.NewReader(os.Stdin)

	for {
		fmt.Print("> ")
		text, _ := reader.ReadString('\n')

		switch strings.TrimSpace(text) {
		case "quit":
			fallthrough
		case "exit":
			return

		default:
			show(strings.TrimSpace(text))
		}
	}
}

func stdio() {
	scanner := bufio.NewScanner(os.Stdin)
	content := ""

	for scanner.Scan() {
		content += "\n" + scanner.Text()
	}

	show(strings.TrimSpace(content))
}

func file(path string) {
	content, err := ioutil.ReadFile(path)

	if err != nil {
		fmt.Printf("Error reading file: %v", err)
		os.Exit(2)
	} else {
		show(strings.TrimSpace(string(content)))
	}
}

func main() {
	if len(os.Args) > 1 {
		file(os.Args[1])
	} else if stat, _ := os.Stdin.Stat(); (stat.Mode() & os.ModeCharDevice) == 0 {
		stdio()
	} else {
		repl()
	}
}

//
// LEXER
//

type TokenKind string

type Token struct {
	kind   TokenKind
	lexeme string
	offset int
}

const (
	EOL = "\n"

	EofToken          TokenKind = "eof"
	EolToken          TokenKind = "eol"
	InvalidToken      TokenKind = "invalid"
	IdentifierToken   TokenKind = "identifier"
	StringToken       TokenKind = "str"
	CharacterToken    TokenKind = "char"
	DecimalToken      TokenKind = "dec"
	IntegerToken      TokenKind = "int"
	HexNumberToken    TokenKind = "hex"
	BinaryNumberToken TokenKind = "bin"
	BooleanToken      TokenKind = "bool"

	LeftParenToken    TokenKind = "lpar"
	RightParenToken   TokenKind = "rpar"
	InvalidParenToken TokenKind = "ivlpar"

	MinusOperatorToken   TokenKind = "minusop"
	PlusOperatorToken    TokenKind = "plusop"
	MultOperatorToken    TokenKind = "multop"
	DivOperatorToken     TokenKind = "divop"
	ExpOperatorToken     TokenKind = "expop"
	GtOperatorToken      TokenKind = "gtop"
	GeOperatorToken      TokenKind = "geop"
	LtOperatorToken      TokenKind = "ltop"
	LeOperatorToken      TokenKind = "leop"
	EqOperatorToken      TokenKind = "eqop"
	NeOperatorToken      TokenKind = "neop"
	InvalidOperatorToken TokenKind = "ivldop"
)

func (tok Token) String() string {
	if tok.kind == EofToken {
		return fmt.Sprintf(`(eof:%d)`, tok.offset)
	} else if tok.kind == EolToken {
		return fmt.Sprintf(`(eol:%d)`, tok.offset)
	} else {
		return fmt.Sprintf(`(%s:%d:%d "%s")`, tok.kind, tok.offset, len(tok.lexeme), tok.lexeme)
	}
}

func token(kind TokenKind, lexeme string, offset int) Token {
	return Token{
		kind:   kind,
		lexeme: lexeme,
		offset: offset,
	}
}

func Lex(raw string) []Token {
	i := 0
	letters := strings.Split(raw+EOL, "")
	lettersLen := len(letters)

	var tokens []Token

	for ; i < lettersLen; i++ {
		letter := letters[i]

		if isQuote(letter) {
			token, len := parseQuoted(letters, i, letter)
			tokens = append(tokens, token)
			i += len
		} else if isDigit(letter) {
			token, len := parseNumeric(letters, i)
			tokens = append(tokens, token)
			i += len - 1
		} else if isEol(letter) {
			// Ignore the last EOL we added
			if i+1 == lettersLen {
				continue
			}

			tokens = append(tokens, token(EolToken, "", i))
		} else if isSpace(letter) {
			continue
		} else if word, len := lookaheadWord(letters, i); isBoolean(word) {
			tokens = append(tokens, token(BooleanToken, word, i))
			i += len - 1
		} else if word := lookahead(letters, i, 2); isOperator(word) {
			tokens = append(tokens, token(getOperatorToken(word), word, i))
			i += 1
		} else if word := lookahead(letters, i, 1); isOperator(word) {
			tokens = append(tokens, token(getOperatorToken(word), word, i))
		} else if word := lookahead(letters, i, 1); isParen(word) {
			tokens = append(tokens, token(getParenToken(word), word, i))
		} else {
			word, len := lookaheadWord(letters, i)
			tokens = append(tokens, token(IdentifierToken, word, i))
			i += len - 1
		}
	}

	return append(tokens, token(EofToken, "", i))
}

func getParenToken(par string) TokenKind {
	toks := map[string]TokenKind{
		"(": LeftParenToken,
		")": RightParenToken,
	}

	tok, ok := toks[par]

	if !ok {
		return InvalidParenToken
	} else {
		return tok
	}
}

func getOperatorToken(op string) TokenKind {
	toks := map[string]TokenKind{
		"+":  PlusOperatorToken,
		"-":  MinusOperatorToken,
		"*":  MultOperatorToken,
		"/":  DivOperatorToken,
		"^":  ExpOperatorToken,
		">":  GtOperatorToken,
		">=": GeOperatorToken,
		"<":  LtOperatorToken,
		"<=": LeOperatorToken,
		"==": EqOperatorToken,
		"!=": NeOperatorToken,
	}

	tok, ok := toks[op]

	if !ok {
		return InvalidOperatorToken
	} else {
		return tok
	}
}

func isBinaryDigit(str string) bool {
	return str == "0" || str == "1"
}

func isHexDigit(str string) bool {
	return isDigit(str) ||
		str == "A" ||
		str == "B" ||
		str == "C" ||
		str == "D" ||
		str == "E" ||
		str == "F"
}

func isDigit(str string) bool {
	return str == "0" ||
		str == "1" ||
		str == "2" ||
		str == "3" ||
		str == "4" ||
		str == "5" ||
		str == "6" ||
		str == "7" ||
		str == "8" ||
		str == "9"
}

func isLetter(str string) bool {
	str = strings.ToLower(str)

	return str == "a" ||
		str == "b" ||
		str == "c" ||
		str == "d" ||
		str == "e" ||
		str == "f" ||
		str == "g" ||
		str == "h" ||
		str == "i" ||
		str == "j" ||
		str == "k" ||
		str == "l" ||
		str == "m" ||
		str == "n" ||
		str == "o" ||
		str == "p" ||
		str == "q" ||
		str == "r" ||
		str == "s" ||
		str == "t" ||
		str == "u" ||
		str == "v" ||
		str == "w" ||
		str == "x" ||
		str == "y" ||
		str == "z"
}

func isAphaNumeric(str string) bool {
	return isDigit(str) || isLetter(str)
}

func isEol(str string) bool {
	return str == "\n" || str == "\r"
}

func isSpace(str string) bool {
	return str == " " || str == "\t" || str == "\n" || str == "\r"
}

func isQuote(str string) bool {
	return str == `"` || str == `'`
}

func isStringQuoteEsc(str string) bool {
	return str == `\`
}

func isBoolean(str string) bool {
	return str == "true" || str == "false"
}

func isParen(str string) bool {
	return str == "[" ||
		str == "]" ||
		str == "(" ||
		str == ")" ||
		str == "{" ||
		str == "}"
}

func isOperator(str string) bool {
	return str == "+" ||
		str == "-" ||
		str == "*" ||
		str == "^" ||
		str == "/" ||
		str == "&" ||
		str == "." ||
		str == "," ||
		str == ";" ||
		str == "<" ||
		str == ">" ||
		str == "|" ||
		str == "@" ||
		str == "!" ||
		str == ":" ||
		str == "=" ||
		str == "\\" ||
		str == "==" ||
		str == "||" ||
		str == "&&" ||
		str == "::" ||
		str == "->" ||
		str == "//"
}

func lookahead(letters []string, start, k int) string {
	buff := ""

	for i := start; i < len(letters) && k != 0; {
		buff = buff + letters[i]
		k--
		i++
	}

	return buff
}

func lookaheadWord(letters []string, start int) (string, int) {
	buff := ""

	for i := start; i < len(letters); i++ {
		curr := letters[i]

		if isSpace(curr) || isParen(curr) || isQuote(curr) {
			break
		}

		if isOperator(lookahead(letters, i, 1)) || isOperator(lookahead(letters, i, 2)) {
			break
		}

		buff = buff + curr
	}

	return buff, len(buff)
}

// Parses:
//   StringToken
//   CharacterToken
func parseQuoted(letters []string, start int, closingQuote string) (Token, int) {
	kind := StringToken
	buff := ""
	lettersLen := len(letters)

	for i := start; i < lettersLen; i++ {
		curr := letters[i]
		prev := ""

		if i != 0 {
			prev = letters[i-1]
		}

		// Add to the buffer anything that's not an escape of the last EOL.
		if !isStringQuoteEsc(curr) && i+1 != lettersLen {
			buff = buff + curr
		}

		// If we're not at the very beginning, we're parsing a quote - our
		// closingQuote - and the previous char wasn't an escape, we're done.
		if i != start && isQuote(curr) && !isStringQuoteEsc(prev) && curr == closingQuote {
			if closingQuote == "'" {
				switch len(buff[1 : len(buff)-1]) {
				case 0:
					fallthrough
				case 1:
					kind = CharacterToken
					break

				default:
					kind = InvalidToken
				}
			}

			return token(kind, buff, start), i - start
		}
	}

	return token(InvalidToken, buff, start), len(buff)
}

// Parses:
//   IntegerToken
//   DecimalToken
//   HexNumberToken
//   BinaryNumberToken
func parseNumeric(letters []string, start int) (Token, int) {
	buff := ""
	peek := lookahead(letters, start, 2)
	kind := IntegerToken
	isInt := true

	// Is this a non-decimal representation of a number?
	if peek == "0x" || peek == "0X" {
		kind = HexNumberToken
		buff = peek
		start += 2
	} else if peek == "0b" || peek == "0B" {
		kind = BinaryNumberToken
		buff = peek
		start += 2
	}

	for i := start; i < len(letters); i++ {
		curr := letters[i]

		isRealChar := isDigit(curr) || curr == "."
		isRealKind := kind == IntegerToken || kind == DecimalToken || kind == InvalidToken

		if kind == HexNumberToken && isHexDigit(curr) {
			buff = buff + curr
		} else if kind == BinaryNumberToken && isBinaryDigit(curr) {
			buff = buff + curr
		} else if isRealKind && isRealChar {
			if isInt == true && curr == "." {
				kind = DecimalToken
				isInt = false
			} else if !isDigit(curr) {
				kind = InvalidToken
			}

			buff = buff + curr
		} else {
			return token(kind, buff, start), len(buff)
		}
	}

	return token(InvalidToken, buff, start), len(buff)
}

//
// PARSER
//

type ExpressionKind string

type Expr struct {
	kind ExpressionKind
	lhs  *Expr
	rhs  *Expr
	opt  Token
	val  Token
}

type Ast struct {
	tokens []Token
	pos    int
	errs   []ParseErr
}

type ParseErr struct {
	err error
	tok Token
}

var (
	ErrMissingRightParen = errors.New("Missing closing parenthesis")
)

const (
	BinaryExpression   ExpressionKind = "binary"
	UnaryExpression    ExpressionKind = "unary"
	GroupingExpression ExpressionKind = "grouping"
	LiteralExpression  ExpressionKind = "literal"
	InvalidExpression  ExpressionKind = "unknown"
)

/*
 * Is able to parse the following grammar
 *
 * expression     = equality ;
 * equality       = comparison ( ( "!=" | "==" ) comparison )* ;
 * comparison     = addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
 * addition       = multiplication ( ( "-" | "+" ) multiplication )* ;
 * multiplication = unary ( ( "/" | "*" ) unary )* ;
 * unary          = ( "!" | "-" ) unary
 *                | primary ;
 * primary        = NUMBER | STRING | "false" | "true" | "nil"
 * 			   | "(" expression ")" ;
 *
 */
func Parse(tokens []Token) (Expr, []ParseErr) {
	ast := NewAst(tokens)
	return ast.parseExpression(), ast.errs
}

func NewAst(tokens []Token) Ast {
	return Ast{tokens: tokens}
}

func (expr Expr) String() string {
	switch expr.kind {
	case BinaryExpression:
		return fmt.Sprintf("(%s %s %s)", expr.opt.lexeme, expr.lhs, expr.rhs)
		break

	case UnaryExpression:
		return fmt.Sprintf("(%s %s)", expr.opt.lexeme, expr.rhs)
		break

	case GroupingExpression:
		return fmt.Sprintf("(group %s)", expr.rhs)
		break

	case LiteralExpression:
		return fmt.Sprintf("<%s>%s", expr.val.kind, expr.val.lexeme)
		break

	case InvalidExpression:
		break

	default:
		break
	}

	return "(invalid)"
}

func (err ParseErr) String() string {
	if err.err == ErrMissingRightParen {
		return fmt.Sprintf(
			"Expecting a closing parentheses but found '%s' intead on position %d.",
			err.tok.kind, err.tok.offset)
	} else {
		return fmt.Sprintf("%s at %s", err.err, err.tok)
	}
}

func (ast *Ast) parseExpression() Expr {
	return ast.parseEquality()
}

func (ast *Ast) parseEquality() Expr {
	expr := ast.parseComparison()

	for matches(ast, EqOperatorToken, NeOperatorToken) {
		opt := prev(ast)
		rhs := ast.parseComparison()
		expr = Expr{
			kind: BinaryExpression,
			opt:  opt,
			rhs:  &rhs,
			lhs: &Expr{
				kind: expr.kind,
				lhs:  expr.lhs,
				rhs:  expr.rhs,
				opt:  expr.opt,
				val:  expr.val,
			},
		}
	}

	return expr
}

func (ast *Ast) parseComparison() Expr {
	expr := ast.parseAddition()
	opts := []TokenKind{
		GtOperatorToken,
		GeOperatorToken,
		LtOperatorToken,
		LeOperatorToken,
	}

	for matches(ast, opts...) {
		opt := prev(ast)
		rhs := ast.parseAddition()
		expr = Expr{
			kind: BinaryExpression,
			opt:  opt,
			rhs:  &rhs,
			lhs: &Expr{
				kind: expr.kind,
				lhs:  expr.lhs,
				rhs:  expr.rhs,
				opt:  expr.opt,
				val:  expr.val,
			},
		}
	}

	return expr
}

func (ast *Ast) parseAddition() Expr {
	expr := ast.parseMultiplication()

	for matches(ast, PlusOperatorToken, MinusOperatorToken) {
		opt := prev(ast)
		rhs := ast.parseMultiplication()
		expr = Expr{
			kind: BinaryExpression,
			opt:  opt,
			rhs:  &rhs,
			lhs: &Expr{
				kind: expr.kind,
				lhs:  expr.lhs,
				rhs:  expr.rhs,
				opt:  expr.opt,
				val:  expr.val,
			},
		}
	}

	return expr
}

func (ast *Ast) parseMultiplication() Expr {
	expr := ast.parseUnary()

	for matches(ast, MultOperatorToken, DivOperatorToken) {
		opt := prev(ast)
		rhs := ast.parseUnary()
		expr = Expr{
			kind: BinaryExpression,
			opt:  opt,
			rhs:  &rhs,
			lhs: &Expr{
				kind: expr.kind,
				lhs:  expr.lhs,
				rhs:  expr.rhs,
				opt:  expr.opt,
				val:  expr.val,
			},
		}
	}

	return expr
}

func (ast *Ast) parseUnary() Expr {
	if matches(ast, MinusOperatorToken) {
		opt := prev(ast)
		rhs := ast.parseUnary()
		return Expr{
			kind: UnaryExpression,
			opt:  opt,
			rhs:  &rhs,
		}
	} else {
		return ast.parsePrimary()
	}
}

func (ast *Ast) parsePrimary() Expr {
	literals := []TokenKind{
		IntegerToken,
		HexNumberToken,
		BinaryNumberToken,
		DecimalToken,
		BooleanToken,
		StringToken,
		CharacterToken,
	}

	if matches(ast, literals...) {
		return Expr{kind: LiteralExpression, val: prev(ast)}
	} else if matches(ast, LeftParenToken) {
		expr := ast.parseExpression()

		if !matches(ast, RightParenToken) {
			ast.errs = append(ast.errs, ParseErr{
				err: ErrMissingRightParen,
				tok: peek(ast),
			})
		}

		return Expr{
			kind: GroupingExpression,
			rhs:  &expr,
		}
	} else {
		return Expr{kind: InvalidExpression}
	}
}

func matches(ast *Ast, kinds ...TokenKind) bool {
	for _, kind := range kinds {
		if check(ast, kind) {
			ast.pos += 1
			return true
		}
	}

	return false
}

func check(ast *Ast, kind TokenKind) bool {
	if eof(ast) {
		return false
	} else {
		return peek(ast).kind == kind
	}
}

func eof(ast *Ast) bool {
	return len(ast.tokens) < ast.pos || peek(ast).kind == EofToken
}

func peek(ast *Ast) Token {
	return ast.tokens[ast.pos]
}

func prev(ast *Ast) Token {
	return ast.tokens[ast.pos-1]
}
