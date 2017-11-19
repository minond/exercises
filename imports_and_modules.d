import std.stdio : writeln;

// By default, `import my.cat` will import `my/cat.d` and looks in the current
// directory. Moduled can be broken up by creating a `package.d` file at the
// root of the module folder that then imports all other files that make up the
// module. The compiler also exposes a flag for setting the import directories,
// `-I`.
void main() {
  writeln("Hello, World!");
}
