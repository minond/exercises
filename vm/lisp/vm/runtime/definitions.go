package runtime

import (
	"fmt"

	"github.com/minond/exercises/vm/lisp/vm/lang"
)

type Applicable interface {
	Apply([]lang.Expr, *Environment) (lang.Value, *Environment, error)
}

type Lambda struct {
	lang.Value
	Applicable
	args []string
	body lang.Expr
}

func NewLambda(args []string, body lang.Expr) *Lambda {
	return &Lambda{args: args, body: body}
}

func (v Lambda) Apply(exprs []lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	if len(v.args) != len(exprs) {
		return nil, env, fmt.Errorf("expected %v arguments but got %v",
			len(v.args), len(exprs))
	}

	params, newEnv, err := evalAll(exprs, env)
	env = newEnv
	if err != nil {
		return nil, env, err
	}

	scope := env.Scoped()
	for i, val := range params {
		scope.Set(v.args[i], val)
	}

	return eval(v.body, scope)
}

func (v Lambda) String() string {
	return "#<procedure>"
}

type Builtin struct {
	lang.Value
	Applicable
	fn builtinFn
}

type builtinFn func(args []lang.Expr, env *Environment) (lang.Value, *Environment, error)

func NewBuiltin(fn builtinFn) *Builtin {
	return &Builtin{fn: fn}
}

func (v Builtin) String() string {
	return "#<builtin>"
}

func (v Builtin) Apply(args []lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	return v.fn(args, env)
}

type Procedure struct {
	lang.Value
	Applicable
	name string
	fn   procedureFn
}

type procedureFn func(args []lang.Value) (lang.Value, error)

func NewProcedure(name string, fn procedureFn) *Procedure {
	return &Procedure{fn: fn, name: name}
}

func (v Procedure) String() string {
	return fmt.Sprintf("#<procedure:%s>", v.name)
}

func (v Procedure) Apply(exprs []lang.Expr, env *Environment) (lang.Value, *Environment, error) {
	params, newEnv, err := evalAll(exprs, env)
	env = newEnv
	if err != nil {
		return nil, env, err
	}

	val, err := v.fn(params)
	return val, env, err
}
