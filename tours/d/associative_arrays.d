import std.array : assocArray;
import std.algorithm.iteration : each, filter, group, splitter, sum;
import std.string : toLower, strip;
import std.stdio : writeln, writefln;

// Standard stuff. But there is a neat `in` operator that returns a pointer if
// the key is found or `null` otherwise.
//
//   string[string] arr;
//   arr["name"] = "Marcos";
//   if (auto name = "name" in arr)
//     *name = "Hannah";
//
// - Accessing a key that does not exist yields a `RangeError`. `arr.get(key,
// defaultValue)` should be used instead.
//
// - AA's have a `.length` property, a `.remove(key)` function, and `.byKey`
// and `.byValue` ranges, too.
void main() {
  string text = "The Tuscan Republic was a short-lived state declared on
    February 18, 1849, after Grand Duke Leopold II fled, leading to the
    suspension of the Grand Duchy of Tuscany.[1] The Republic ended and the
    Grand Duchy was reinstated on April 12 that year when the municipal
    council, fearing Austrian invasion, usurped the powers of the assembly and
    invited the Grand Duke to return.";

  int[string] words;
  text.toLower().splitter(" ").filter!(w => w != "").each!(w => words[strip(w)]++);

  foreach (key, val; words)
    writefln("Key: %s\t\tValue: %s", key, val);

  writeln("Words: ", words.keys);
  writeln("Counts: ", words.values);
  writeln("Total: ", words.byValue.sum);
}
