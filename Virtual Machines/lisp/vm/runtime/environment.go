package runtime

import (
	"fmt"
	"strings"

	"github.com/minond/exercises/vm/lisp/vm/lang"
)

type Environment struct {
	parent   *Environment
	bindings map[string]lang.Value
}

func NewEnvironment() *Environment {
	return &Environment{bindings: make(map[string]lang.Value)}
}

func NewEnvironmentWithPrelude() *Environment {
	uni := NewEnvironment()
	uni.bindings = builtins
	env := NewEnvironment()
	env.parent = uni
	return env
}

func (env *Environment) String() string {
	return env.PrettyPrint(1)
}

func (env *Environment) PrettyPrint(level int) string {
	buff := strings.Builder{}
	padd := strings.Repeat(" ", level*2)
	buff.WriteString(fmt.Sprintf("%s+ level: %d\n", padd, level))
	for key, val := range env.bindings {
		buff.WriteString(fmt.Sprintf("%s  - %s: %s\n", padd, key, val))
	}

	if env.parent != nil {
		return buff.String() + env.parent.PrettyPrint(level+1)
	}
	return buff.String()
}

func (env *Environment) Scoped() *Environment {
	scoped := NewEnvironment()
	scoped.parent = env
	return scoped
}

func (env *Environment) TopMostParent() *Environment {
	if env.parent == nil {
		return env
	}
	return env.parent
}

func (env *Environment) Set(id string, val lang.Value) {
	env.bindings[id] = val
}

func (env Environment) Get(id string) (lang.Value, error) {
	val, ok := env.bindings[id]
	if !ok && env.parent != nil {
		return env.parent.Get(id)
	} else if !ok {
		return nil, fmt.Errorf("undefined: %v", id)
	}
	return val, nil
}
