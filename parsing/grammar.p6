use v6;

# https://docs.perl6.org/language/grammars
# https://docs.perl6.org/language/grammar_tutorial

grammar Parser {
  rule TOP {
    ^^ <sexp> | <qexp> $$
  }

  rule sexp {
    '(' <stmt>* ')'
  }

  rule qexp {
    '\'(' <stmt>* ')'
  }

  rule stmt {
    [<builtin> | <number> | <sexp> | <boolean> | <string>]+
  }

  token builtin {
    < + - * / = != % >
  }

  token number {
    \d+
  }

  token boolean {
    '#t' | '#f'
  }

  token string {
    '"' [ <str> | \\ <str=.str_escape>  ]* '"'
  }

  token str {
    <-["\\\t\n]>+
  }

  token str_escape {
    <["\\/bfnrt]>
  }
}

class Builtin {
  has $.name;
}

class Bindings {
  method number ($/) {
    say "Got $/ number.";
    make $/.Int;
  }

  method builtin ($/) {
    say "Got $/ builtin.";
    Builtin.new(name => $/);
  }

  method sexp ($/) {
    say "Got $/ sexp.";
  }
}

sub MAIN {
  my $raw = '(+ 12 13 (* 3 5 1) 54 #f #t "123 123 \" 123 123 \" 123 123")';
  my $ffi = Bindings.new;
  my $ast = Parser.parse: $raw, :actions($ffi);

  say $ast;
}
