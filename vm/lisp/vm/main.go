package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/minond/exercises/vm/lisp/vm/lang"
	"github.com/minond/exercises/vm/lisp/vm/runtime"
)

type repl struct {
	running bool
	env     *runtime.Environment
	buff    strings.Builder
	input   io.Reader
	output  io.Writer
}

func (r *repl) prompt() {
	r.print("= ")
}

func (r *repl) read() {
	reader := bufio.NewReader(r.input)
	input, _ := reader.ReadBytes('\n')
	r.buff.WriteString(" ")
	r.buff.Write(input)
}

func (r *repl) printf(f string, args ...interface{}) {
	fmt.Fprintf(r.output, f, args...)
}

func (r *repl) print(str string) {
	fmt.Fprint(r.output, str)
}

func (r *repl) eval() (string, lang.Value, error) {
	orig := r.buff.String()
	code := strings.TrimSpace(orig)
	if code == "" {
		return "", nil, nil
	}

	switch code {
	case "(exit)":
		r.running = false
		return "", nil, nil
	}

	out, env, err := runtime.Eval(orig, r.env)
	r.env = env
	r.buff.Reset()
	if err != nil {
		return "", nil, err
	}

	buff := strings.Builder{}
	for i, val := range out {
		if i != 0 {
			buff.WriteRune('\n')
		}
		if val != nil {
			buff.WriteString(val.String())
			buff.WriteString(" : ")
			buff.WriteString(runtime.TypeNameOf(val))
		}
	}

	if len(out) != 0 {
		return buff.String(), out[len(out)-1], nil
	} else {
		return buff.String(), nil, nil
	}
}

func main() {
	env := runtime.NewEnvironmentWithPrelude()
	r := repl{
		running: true,
		env:     env,
		output:  os.Stdout,
		input:   os.Stdin,
	}

	for r.running {
		r.prompt()
		r.read()
		str, val, err := r.eval()
		env.Set("_", val)
		if err != nil {
			r.printf("%v\n", err)
		} else if str != "" {
			r.printf("%v\n", str)
		}
	}
}
