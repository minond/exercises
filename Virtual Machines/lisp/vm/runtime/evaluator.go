package runtime

import (
	"errors"
	"fmt"

	"github.com/minond/exercises/vm/lisp/vm/lang"
	"github.com/minond/exercises/vm/lisp/vm/lang/parser"
)

func Eval(code string, env *Environment) ([]lang.Value, *Environment, error) {
	exprs, err := parser.Parse(code)
	if err != nil {
		return nil, env, err
	}

	return evalAll(exprs, env)
}

func eval(expr lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	switch e := expr.(type) {
	case *lang.String:
		return e, env, nil
	case *lang.Boolean:
		return e, env, nil
	case *lang.Number:
		return e, env, nil
	case *lang.Identifier:
		val, err := env.Get(e.Label())
		return val, env, err
	case *lang.Sexpr:
		return app(e, env)
	case *lang.Quote:
		val, err := unquote(e)
		return val, env, err
	}

	return nil, env, errors.New("unable to handle expression")
}

func evalAll(exprs []lang.Expr, env *Environment) ([]lang.Value, *Environment, error) {
	vals := make([]lang.Value, len(exprs))
	for i, expr := range exprs {
		val, newEnv, err := eval(expr, env)
		env = newEnv
		if err != nil {
			return nil, env, err
		}

		vals[i] = val
	}

	return vals, env, nil
}

func app(expr *lang.Sexpr, env *Environment) (lang.Value, *Environment, error) {
	if expr.Size() == 0 {
		return nil, env, errors.New("missing procedure expression")
	}

	val, newEnv, err := eval(expr.Head(), env)
	env = newEnv
	if err != nil {
		return nil, env, err
	}

	fn, ok := val.(Applicable)
	if !ok {
		return nil, env, fmt.Errorf("not a procedure: %v", val)
	}

	return fn.Apply(expr.Tail(), env)
}

func unquote(expr *lang.Quote) (lang.Value, error) {
	switch inner := expr.Unquote().(type) {
	case *lang.Identifier:
		return expr, nil
	case *lang.Quote:
		return expr, nil
	case *lang.String:
		return inner, nil
	case *lang.Boolean:
		return inner, nil
	case *lang.Number:
		return inner, nil
	case *lang.Sexpr:
		vals := make([]lang.Value, inner.Size())
		for i, val := range inner.Values() {
			switch inner := val.(type) {
			case *lang.Identifier:
				vals[i] = lang.NewQuote(val)
			case *lang.Sexpr:
				vals[i] = lang.NewQuote(val)
			case *lang.Quote:
				unquoted, err := unquote(inner)
				if err != nil {
					return nil, err
				}
				vals[i] = unquoted
			case *lang.String:
				vals[i] = inner
			case *lang.Boolean:
				vals[i] = inner
			case *lang.Number:
				vals[i] = inner
			default:
				return nil, errors.New("invalid quoted expression")
			}
		}
		return lang.NewList(vals), nil
	}
	return nil, errors.New("invalid quoted expression")
}
