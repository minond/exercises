package lang

import (
	"fmt"
	"strconv"
	"strings"
)

type Expr interface {
	fmt.Stringer
	isExpr()
}

type Value interface {
	fmt.Stringer
	isValue()
}

type Sexpr struct {
	Expr
	values []Expr
}

func NewSexpr(values ...Expr) *Sexpr {
	return &Sexpr{values: values}
}

func (e Sexpr) Size() int {
	return len(e.values)
}

func (e Sexpr) Values() []Expr {
	return e.values
}

func (e Sexpr) Head() Expr {
	return e.values[0]
}

func (e Sexpr) Tail() []Expr {
	return e.values[1:]
}

func (e Sexpr) String() string {
	buff := strings.Builder{}
	buff.WriteString("(")
	for i, val := range e.values {
		if i != 0 {
			buff.WriteRune(' ')
		}
		buff.WriteString(val.String())
	}
	buff.WriteString(")")
	return buff.String()
}

type Quote struct {
	Expr
	Value
	value Expr
}

func NewQuote(value Expr) *Quote {
	return &Quote{value: value}
}

func (e Quote) Unquote() Expr {
	return e.value
}

func (e Quote) String() string {
	return fmt.Sprintf("'%v", e.value.String())
}

type Identifier struct {
	Expr
	Value
	value string
}

func NewIdentifier(value string) *Identifier {
	return &Identifier{value: value}
}

func (e Identifier) Label() string {
	return e.value
}

func (e Identifier) String() string {
	return e.value
}

type Number struct {
	Expr
	Value
	value float64
}

func NewNumber(value float64) *Number {
	return &Number{value: value}
}

func (e Number) Float64() float64 {
	return e.value
}

func (e Number) String() string {
	return strconv.FormatFloat(e.value, 'f', -1, 64)
}

type String struct {
	Expr
	Value
	value string
}

func NewString(value string) *String {
	return &String{value: value}
}

func (e String) String() string {
	return fmt.Sprintf(`"%v"`, e.value)
}

type Boolean struct {
	Expr
	Value
	value bool
}

func NewBoolean(value bool) *Boolean {
	return &Boolean{value: value}
}

func (e Boolean) Bool() bool {
	return e.value
}

func (e Boolean) True() bool {
	return e.value == true
}

func (e Boolean) False() bool {
	return e.value == false
}

func (e Boolean) String() string {
	if e.value {
		return "#t"
	}
	return "#f"
}

type List struct {
	Value
	values []Value
}

func NewList(values []Value) *List {
	return &List{values: values}
}

func (e List) PairQ() bool {
	return len(e.values) > 0
}

func (e List) Size() int {
	return len(e.values)
}

func (e List) Head() Value {
	return e.values[0]
}

func (e List) Tail() []Value {
	return e.values[1:]
}

func (e List) Values() []Value {
	return e.values
}

func (e List) String() string {
	buff := strings.Builder{}
	buff.WriteString("'(")
	for i, val := range e.values {
		if i != 0 {
			buff.WriteRune(' ')
		}
		buff.WriteString(val.String())
	}
	buff.WriteString(")")
	return buff.String()
}
