import std.stdio : writeln;

// By default everything is mutable. A `const` can point to mutable data but an
// `immutable` cannot. `const` is only enforced in the current scope.

void foo(const char[] s)
{
  writeln(s);
}

void main()
{
  int m = 100;
  m = 200;
  writeln("m type: ", typeof(m).stringof);
  writeln("m val: ", m);

  writeln("---------------------------------");

  const int* cm = &m;
  writeln("cm type: ", typeof(cm).stringof);
  writeln("cm val: ", cm);
  writeln("cm * val: ", *cm);

  writeln("---------------------------------");

  m = 300;
  writeln("cm type: ", typeof(cm).stringof);
  writeln("cm val: ", cm);
  writeln("cm * val: ", *cm);

  writeln("---------------------------------");

  immutable int v = 100;
  writeln("v type: ", typeof(v).stringof);
  writeln("v val: ", v);

  writeln("---------------------------------");

  const int* cv = &v;
  writeln("cv type: ", typeof(cv).stringof);
  writeln("cv val: ", cv);
  writeln("cv * val: ", *cv);
}
