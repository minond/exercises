package runtime

import "github.com/minond/exercises/vm/lisp/vm/lang"

func TypeNameOf(val lang.Value) string {
	switch val.(type) {
	case *lang.List:
		return "list"
	case *lang.Boolean:
		return "boolean"
	case *lang.String:
		return "string"
	case *lang.Number:
		return "number"
	case *lang.Quote:
		return "quote"

	// TODO Maybe these three types should be commbined into two (a lazy one
	// and an eager one.)
	case *Lambda:
		return "lambda"
	case *Builtin:
		return "builtin"
	case *Procedure:
		return "procedure"
	}

	return "unknown"
}
