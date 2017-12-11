import std.array : array;
import std.algorithm.iteration : map;
import std.parallelism : parallel, TaskPool, task;
import std.range : iota;
import std.stdio : writeln;

string theTask() {
  import core.thread : dur, Thread;

  Thread.sleep(dur!("seconds")(2));
  return "Hello";
}

void main() {
  auto pool = new TaskPool(2);

  scope (exit)
    pool.stop();

  auto t = task!theTask;
  pool.put(t);

  auto arr = iota(1, 10).array;
  foreach (ref i; pool.parallel(arr)) {
    // foreach(ref i; arr) {
    i = i * i;
  }

  writeln(arr);

  auto result = pool.reduce!"a+b"(0.0, iota(100).map!"a*a");
  writeln("Sum of squares: ", result);
  writeln(t.yieldForce);
}
