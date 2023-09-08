import std.file : FileException, readText;
import std.stdio : writeln;
import std.string : StringException;

// D has try/catch, but it recommends the use of scope guards instead. D has a
// `nothrow` annotation which the compiler uses for static analysis.
//
// - `std.exception.enforce` is `assert` but kept after production mode
//   compilation.
//
// - `std.exception.collectException` takes a function (called, not as a var)
//   and return the exception if there is one.
class MyException : Exception {
  this(string msg, string file = __FILE__, size_t line = __LINE__) {
    super(msg, file, line);
  }
}

void main() {
  try {
    readText("doesnotexists.txt");
    throw new StringException("Blah");
  }
  catch (FileException err) {
    writeln("Message: ", err.message);
    writeln("\nFile: ", err.file);
    writeln("\nLine: ", err.line);
    writeln("\nStack:\n", err.info);
  }
  catch (StringException err) {
    writeln("This error should not be seen!");
    throw new MyException("Hey");
  }
  catch (MyException err) {
    writeln("This error should not be seen!");
  }
  finally {
    writeln("\n\nEnd.");
  }
}
