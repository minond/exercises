import std.stdio : writeln, writefln;
import core.stdc.stdlib : malloc, free;

// Scope guards allow for execution at certain conditions once the scope (block
// scope) is left. Scope guards are called in reverse order that they were
// defined in, like Go's `defer`. Also, scope guards are to D what RAII is to
// C++. Options include:
//
// - `scope(exit)`: this scope guard will always be called
// - `scope(success)`: called when no exceptions were thrown
// - `scope(failure)`: called when an exception was called before the scope end
void main() {
  writeln("<html>");

  scope (exit)
    writeln("</html>");

  {
    writeln("  <head>");

    scope (exit)
      writeln("  </head>");
  }

  writeln("  <body>");
  writeln("    <h1>Hi</h1>");

  scope (exit)
    writeln("  </body>");

  int* p = cast(int*) malloc(int.sizeof);
  scope (exit)
    free(p);
}
