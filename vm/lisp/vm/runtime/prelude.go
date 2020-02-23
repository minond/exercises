package runtime

import (
	"errors"
	"fmt"

	"github.com/minond/exercises/vm/lisp/vm/lang"
)

var builtins = map[string]lang.Value{
	"cond":   builtinCond,
	"set!":   builtinSetBang,
	"define": builtinDefine,
	"lambda": builtinLambda,
}

var procedures = map[string]procedureFn{
	"+": binaryFloat64Op(func(a, b float64) float64 { return a + b }),
	"-": binaryFloat64Op(func(a, b float64) float64 { return a - b }),
	"*": binaryFloat64Op(func(a, b float64) float64 { return a * b }),
	"/": binaryFloat64Op(func(a, b float64) float64 { return a / b }),

	"not":    unaryBoolOp(func(a bool) bool { return !a }),
	"true?":  unaryBoolOp(func(a bool) bool { return a == true }),
	"false?": unaryBoolOp(func(a bool) bool { return a == false }),

	"car":    procedureCar,
	"cdr":    procedureCdr,
	"cons":   procedureCons,
	"null?":  procedureNullQ,
	"length": procedureLength,
	"pair?":  procedurePairQ,
	"list?":  procedureListQ,

	"type": procedureType,
}

func init() {
	for name, proc := range procedures {
		builtins[name] = NewProcedure(name, proc)
	}
}

var builtinLambda = NewBuiltin(func(exprs []lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	if len(exprs) != 2 {
		return nil, env, errors.New("syntax error: lambda")
	}

	argDef, ok := exprs[0].(*lang.Sexpr)
	if !ok {
		return nil, env, errors.New("syntax error: invalid lambda arguments definition")
	}

	argExprs := argDef.Values()
	args := make([]string, len(argExprs))
	for i, argExpr := range argExprs {
		id, ok := argExpr.(*lang.Identifier)
		if !ok {
			return nil, env, errors.New("syntax error: invalid lambda argument definition")
		}
		args[i] = id.Label()
	}

	return NewLambda(args, exprs[1]), env, nil
})

var builtinDefine = NewBuiltin(func(exprs []lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	if len(exprs) != 2 {
		return nil, env, errors.New("contract error: expected two arguments")
	}

	label, ok := exprs[0].(*lang.Identifier)
	if !ok {
		return nil, env, errors.New("contract error: expected an identifier as the first parameter")
	}

	val, newEnv, err := eval(exprs[1], env)
	env = newEnv
	if err != nil {
		return nil, env, err
	} else if val == nil {
		return nil, env, errors.New("cannot bind a name to an empty value")
	}

	env.Set(label.Label(), val)
	return nil, env, nil
})

var builtinSetBang = NewBuiltin(func(exprs []lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	if len(exprs) != 2 {
		return nil, env, errors.New("contract error: expected two arguments")
	}

	label, ok := exprs[0].(*lang.Identifier)
	if !ok {
		return nil, env, errors.New("contract error: expected an identifier as the first parameter")
	}

	val, newEnv, err := eval(exprs[1], env)
	env = newEnv
	if err != nil {
		return nil, env, err
	} else if val == nil {
		return nil, env, errors.New("cannot bind a name to an empty value")
	}

	env.TopMostParent().Set(label.Label(), val)
	return nil, env, nil
})

// builtinCond expects exprs to be a list of sexprs with at least one item. The
// first item is evaluated. If it is not #f then evaluate the tail and return
// the last item. Otherwise move on to the next item. If no cond is true then
// return an error. An `else` clause is only allowed as the last item in the
// list and it always evaluates to true.
var builtinCond = NewBuiltin(func(exprs []lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	isElse := func(expr lang.Expr) bool {
		id, ok := expr.(*lang.Identifier)
		return ok && id.Label() == "else"
	}

	conds := make([]*lang.Sexpr, len(exprs))
	for i, expr := range exprs {
		switch sexpr := expr.(type) {
		case *lang.Sexpr:
			if sexpr.Size() == 0 {
				return nil, env, errors.New("cond: clause is not a pair")
			} else if isElse(sexpr.Head()) && i != len(exprs)-1 {
				return nil, env, errors.New("cond: `else` clause must be at the end")
			}

			conds[i] = sexpr
		default:
			return nil, env, errors.New("cond: invalid syntax")
		}
	}

	for _, cond := range conds {
		if isElse(cond.Head()) {
			// Else must have subsequent expression to evaluate
			if len(cond.Tail()) == 0 {
				return nil, env, errors.New("cond: missing expresison in `else` clause")
			}
		} else {
			// We're in an "else" clause, move on to tail evaluation
			val, newEnv, err := eval(cond.Head(), env)
			env = newEnv
			if err != nil {
				return nil, env, err
			}

			switch b := val.(type) {
			case *lang.Boolean:
				if b.False() {
					continue
				}
			}

			// Single item list, return the head
			if len(cond.Tail()) == 0 {
				return val, env, nil
			}
		}

		vals, env, err := evalAll(cond.Tail(), env)
		if err != nil {
			return nil, env, err
		}

		return vals[len(vals)-1], env, nil
	}

	return nil, env, errors.New("cond: no return value")
})

func binaryFloat64Op(op func(float64, float64) float64) func([]lang.Value) (lang.Value, error) {
	return func(args []lang.Value) (lang.Value, error) {
		var total float64
		for i, n := range args {
			num, ok := n.(*lang.Number)
			if !ok {
				return nil, fmt.Errorf("contract error: expected a number in position %v", i)
			}

			if i == 0 {
				total = num.Float64()
			} else {
				total = op(total, num.Float64())
			}
		}

		return lang.NewNumber(total), nil
	}
}

func unaryBoolOp(op func(bool) bool) func([]lang.Value) (lang.Value, error) {
	return func(args []lang.Value) (lang.Value, error) {
		if len(args) == 0 {
			return nil, errors.New("contract error: expected an argument")
		}

		arg, ok := args[0].(*lang.Boolean)
		if !ok {
			return nil, errors.New("contract error: expected a boolean")
		}

		return lang.NewBoolean(op(arg.Bool())), nil
	}
}

func procedureCar(args []lang.Value) (lang.Value, error) {
	if len(args) != 1 {
		return nil, errors.New("contract error: expected one argument")
	}

	switch arg := args[0].(type) {
	case *lang.List:
		if arg.PairQ() {
			return arg.Head(), nil
		}
		return nil, errors.New("contract error: expected a pair")
	}
	return nil, errors.New("contract error: expected a list")
}

func procedureCdr(args []lang.Value) (lang.Value, error) {
	if len(args) != 1 {
		return nil, errors.New("contract error: expected one argument")
	}

	switch arg := args[0].(type) {
	case *lang.List:
		if arg.PairQ() {
			return lang.NewList(arg.Tail()), nil
		}
		return nil, errors.New("contract error: expected a pair")
	}
	return nil, errors.New("contract error: expected a pair")
}

func procedureCons(args []lang.Value) (lang.Value, error) {
	if len(args) != 2 {
		return nil, errors.New("contract error: expected two arguments")
	}

	switch tail := args[1].(type) {
	case *lang.List:
		return lang.NewList(append([]lang.Value{args[0]}, tail.Values()...)), nil
	}
	return nil, errors.New("contract error: expected a list as the second argument")
}

func procedureNullQ(args []lang.Value) (lang.Value, error) {
	if len(args) != 1 {
		return nil, errors.New("contract error: expected one argument")
	}

	switch arg := args[0].(type) {
	case *lang.List:
		return lang.NewBoolean(arg.Size() == 0), nil
	}

	return nil, errors.New("contract error: expected a list")
}

func procedureLength(args []lang.Value) (lang.Value, error) {
	if len(args) != 1 {
		return nil, errors.New("contract error: expected one argument")
	}

	switch arg := args[0].(type) {
	case *lang.List:
		return lang.NewNumber(float64(arg.Size())), nil
	}

	return lang.NewBoolean(false), nil
}

func procedurePairQ(args []lang.Value) (lang.Value, error) {
	if len(args) != 1 {
		return nil, errors.New("contract error: expected one argument")
	}

	switch arg := args[0].(type) {
	case *lang.List:
		return lang.NewBoolean(arg.PairQ()), nil
	}

	return lang.NewBoolean(false), nil
}

func procedureListQ(args []lang.Value) (lang.Value, error) {
	if len(args) != 1 {
		return nil, errors.New("contract error: expected one argument")
	}

	switch args[0].(type) {
	case *lang.List:
		return lang.NewBoolean(true), nil
	}

	return lang.NewBoolean(false), nil
}

func procedureType(args []lang.Value) (lang.Value, error) {
	if len(args) != 1 {
		return nil, errors.New("contract error: expected one argument")
	}

	switch args[0].(type) {
	case *lang.List:
		return lang.NewQuote(lang.NewIdentifier("list")), nil
	case *lang.Boolean:
		return lang.NewQuote(lang.NewIdentifier("boolean")), nil
	case *lang.String:
		return lang.NewQuote(lang.NewIdentifier("string")), nil
	case *lang.Number:
		return lang.NewQuote(lang.NewIdentifier("number")), nil
	case *lang.Quote:
		return lang.NewQuote(lang.NewIdentifier("quote")), nil

	// TODO Maybe these three types should be commbined into two (a lazy one
	// and an eager one.)
	case *Lambda:
		return lang.NewQuote(lang.NewIdentifier("lambda")), nil
	case *Builtin:
		return lang.NewQuote(lang.NewIdentifier("builtin")), nil
	case *Procedure:
		return lang.NewQuote(lang.NewIdentifier("procedure")), nil
	}

	return lang.NewQuote(lang.NewIdentifier("unknown")), nil
}
