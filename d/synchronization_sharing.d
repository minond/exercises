// `shared` marks variables that are shared among threads.
// `std.concurrency.send` lets you send `immutable` or `shared` data (a copy).
// There's a `synchronized` keyword that behaves like Java's. Blocks and
// classes can be `synchronized`.
import core.atomic : atomicOp, atomicLoad;
import std.array : empty;
import std.concurrency : receiveOnly, send, spawn, Tid, thisTid;
import std.range : iota;
import std.stdio : writeln;

synchronized class SafeQueue(T) {
  private T[] elems;

  void push(T val) {
    elems ~= val;
  }

  T pop() {
    T val;

    if (elems.empty)
      return val;

    val = elems[0];
    elems = elems[1 .. $];
    return val;
  }
}

void safePrint(T...)(T args) {
  synchronized {
    writeln(args);
  }
}

void threadProducer(shared(SafeQueue!int) queue, shared(int)* counter) {
  foreach (i; iota(1, 11)) {
    queue.push(i);
    safePrint("Pushed ", i);
    atomicOp!"+="(*counter, 1);
  }
}

void threadConsumer(Tid owner, shared(SafeQueue!int) queue, shared(int)* counter) {
  int popped = 0;

  while (popped != 10) {
    auto i = queue.pop();

    if (i == int.init)
      continue;

    ++popped;
    safePrint("Popped ", i, " (Consumer pushed ", atomicLoad(*counter), ")");
  }

  owner.send(true);
}

void main() {
  auto queue = new shared(SafeQueue!int);
  shared int counter = 0;

  spawn(&threadProducer, queue, &counter);
  auto consumer = spawn(&threadConsumer, thisTid, queue, &counter);
  auto stopped = receiveOnly!bool;
  assert(stopped);
}
