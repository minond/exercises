// Check out `std.range` and `std.algorithm`.
import std.algorithm : filter, map, each, canFind, splitter, sort, uniq,
  chunkBy, joiner;
import std.array : empty, array;
import std.conv : to;
import std.string : format;
import std.range : zip;
import std.stdio : writeln;

void main() {
  string txt = q{This tour will give you an overview of this powerful and
    expressive systems programming language which compiles directly to
    efficient, *native* machine code.};

  writeln(txt ~ "\n\n");

  alias pred = c => canFind(" ,.\n", c);

  auto words = txt.splitter!pred.filter!(a => !a.empty);
  writeln(words);

  auto wordCharCounts = words.map!`a.count`;
  writeln(wordCharCounts);

  zip(wordCharCounts, words) // convert to an array so we can call .sort
  .array().sort().uniq().chunkBy!(a => a[0]).map!(chunk => format("%d -> %s",
      chunk[0], chunk[1].map!(a => a[1]).joiner(", "))).joiner("\n").writeln();

  auto range = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  writeln(range);

  writeln(filter!"a > 5"(range));
  writeln(filter!(a => a > 5)(range));
  writeln(range.filter!"a > 5"());
  writeln(range.filter!(a => a > 5));

  writeln(range.map!(x => to!string(x)));
  each!(x => writeln(x))(range);
  range.each!(x => writeln(x));
}
