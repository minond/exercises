import core.thread : Fiber;
import std.range : iota;
import std.stdio : write;

void myRange(alias Fnc, R, T)(R range, ref T res) {
  for (; !range.empty; range.popFront) {
    res = Fnc(range.front);
    Fiber.yield();
  }
}

void main() {
  int squareResult, cubeResult;

  auto squareFiber = new Fiber({ myRange!(x => x * x)(iota(1, 11), squareResult); });
  auto cubeFiber = new Fiber({ myRange!(x => x * x * x)(iota(1, 9), cubeResult); });

  squareFiber.call();
  cubeFiber.call();

  while (squareFiber.state != Fiber.State.TERM && cubeFiber.state != Fiber.State.TERM) {
    write(squareResult, " ", cubeResult);
    squareFiber.call();
    cubeFiber.call();
    write("\n");
  }
}
