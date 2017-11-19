import std.array : replace;
import std.algorithm : endsWith;
import std.conv : to;
import std.range : walkLength;
import std.stdio : writeln, writefln;
import std.string : format;
import std.uni : byGrapheme;

// The `string` type is defined as such:
//
//   alias string  = immutable(char)[];  // UTF-8 string
//   alias wstring = immutable(wchar)[]; // UTF-16 string
//   alias dstring = immutable(dchar)[]; // UTF-32 string
//
// Since strings are immutable, they are thread-safe. And since they are
// slices, parts can be taken out without allocating more memory. And since
// strings are arrays/slices, any operation (function?) that works on arrays
// works on strings, too.
void main() {
  string str1 = "Hello, World!";
  wstring wstr1 = to!wstring(str1);
  dstring dstr1 = to!dstring(str1);

  writeln("string1  = ", str1);
  writeln("wstring1 = ", wstr1);
  writeln("dstring1 = ", dstr1);

  string s = "\u0041\u0308";

  writeln("\n", s);
  writeln(s, ".length = ", s.length);
  writeln(s, ".walkLength = ", s.walkLength);
  writeln(s, ".byGrapheme.writeln = ", s.byGrapheme.walkLength);

  string ml = "
    The quick brown fox
    jumped over the lazy dog.";

  string raw1 = `The "quick" brown fox jumped over the "lazy" dog.`;
  string raw2 = r"The `quick` brown fox jumped over the `lazy` dog.";

  writeln(ml);
  writeln();
  writeln(raw1);
  writeln(raw2);

  string str2 = format("%s %s", "Hellö", "Wörld");
  writeln();
  writeln("str2 = ", str2);
  writeln("str2.length = ", str2.length);
  writeln("str2.walkLength = ", str2.walkLength);
  writeln("str2.byGrapheme.walkLength = ", str2.byGrapheme.walkLength);

  writeln();
  writeln(str2);
  writeln(replace(str2, "lö", "lo"));
  writefln(`Does "%s" end with "rld"? %s`, str2, endsWith(str2, "rld"));
}
