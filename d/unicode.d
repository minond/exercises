import std.stdio : writeln;

void main() {
  string utf8 = "å ø ∑ 😦";
  wstring utf16 = "å ø ∑ 😦";
  dstring utf32 = "å ø ∑ 😦";

  writeln("utf8 = ", utf8);
  writeln("utf8.length = ", utf8.length);

  writeln("utf16 = ", utf16);
  writeln("utf16.length = ", utf16.length);

  writeln("utf32 = ", utf32);
  writeln("utf32.length = ", utf32.length);
}
