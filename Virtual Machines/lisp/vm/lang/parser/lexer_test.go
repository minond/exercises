package parser

import "testing"

func assertTokensEq(t *testing.T, returned, expected []Token) {
	t.Helper()

	if len(returned) != len(expected) {
		t.Errorf(`missmatched length:\n
returned: (len=%v) %v
expected: (len=%v) %v`, len(returned), returned, len(expected), expected)
		return
	}

	for i := range returned {
		if !returned[i].Eq(expected[i]) || returned[i].offset != expected[i].offset {
			t.Errorf(`missmatched token:\n
returned: (index=%v) %v
expected: (index=%v) %v`, i, returned[i], i, expected[i])
		}
	}
}

func TestLexer(t *testing.T) {
	var tests = []struct {
		label    string
		input    string
		expected []Token
	}{
		{
			label:    "empty string",
			input:    ``,
			expected: nil,
		},
		{
			label: "whitespace",
			input: `					      `,
			expected: nil,
		},
		{
			label:    "open and close paren",
			input:    `()`,
			expected: []Token{buildTokenOpenParen(0), buildTokenCloseParen(1)},
		},
		{
			label: "nested parens",
			input: `((()))`,
			expected: []Token{
				buildTokenOpenParen(0),
				buildTokenOpenParen(1),
				buildTokenOpenParen(2),
				buildTokenCloseParen(3),
				buildTokenCloseParen(4),
				buildTokenCloseParen(5),
			},
		},
		{
			label:    "quoted list",
			input:    `'()`,
			expected: []Token{buildTokenQuote(0), buildTokenOpenParen(1), buildTokenCloseParen(2)},
		},
		{
			label:    "string",
			input:    `"hi there, how are you today?"`,
			expected: []Token{buildTokenString([]rune("hi there, how are you today?"), 0)},
		},
		{
			label: "strings",
			input: `"hi there" "how are you today?"`,
			expected: []Token{
				buildTokenString([]rune("hi there"), 0),
				buildTokenString([]rune("how are you today?"), 11),
			},
		},
		{
			label:    "boolean",
			input:    `#f`,
			expected: []Token{buildTokenBoolean([]rune("#f"), 0)},
		},
		{
			label: "booleans",
			input: `#f #t #t #f`,
			expected: []Token{
				buildTokenBoolean([]rune("#f"), 0),
				buildTokenBoolean([]rune("#t"), 3),
				buildTokenBoolean([]rune("#t"), 6),
				buildTokenBoolean([]rune("#f"), 9),
			},
		},
		{
			label:    "number",
			input:    `123`,
			expected: []Token{buildTokenNumber([]rune("123"), 0)},
		},
		{
			label: "numbers",
			input: `1 2 3 456 0.1 432432.432342432`,
			expected: []Token{
				buildTokenNumber([]rune("1"), 0),
				buildTokenNumber([]rune("2"), 2),
				buildTokenNumber([]rune("3"), 4),
				buildTokenNumber([]rune("456"), 6),
				buildTokenNumber([]rune("0.1"), 10),
				buildTokenNumber([]rune("432432.432342432"), 14),
			},
		},
		{
			label:    "word",
			input:    `one`,
			expected: []Token{buildTokenWord([]rune("one"), 0)},
		},
		{
			label: "words",
			input: `one two? three-four+five*six!`,
			expected: []Token{
				buildTokenWord([]rune("one"), 0),
				buildTokenWord([]rune("two?"), 4),
				buildTokenWord([]rune("three-four+five*six!"), 9),
			},
		},
		{
			label: "everything",
			input: `	 (+1 21 twenty_two #f abc#abc (#t) (one))`,
			expected: []Token{
				buildTokenOpenParen(2),
				buildTokenWord([]rune("+1"), 3),
				buildTokenNumber([]rune("21"), 6),
				buildTokenWord([]rune("twenty_two"), 9),
				buildTokenBoolean([]rune("#f"), 20),
				buildTokenWord([]rune("abc#abc"), 23),
				buildTokenOpenParen(31),
				buildTokenBoolean([]rune("#t"), 32),
				buildTokenCloseParen(34),
				buildTokenOpenParen(36),
				buildTokenWord([]rune("one"), 37),
				buildTokenCloseParen(40),
				buildTokenCloseParen(41),
			},
		},
	}

	for _, test := range tests {
		t.Run(test.label, func(t *testing.T) {
			assertTokensEq(t, lex(test.input), test.expected)
		})
	}
}
