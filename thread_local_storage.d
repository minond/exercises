import std.concurrency : spawn, thisTid;
import std.stdio : writeln;

// The `static` before `int threadState` allows us to declare and initializer
// the variable only once per thread and is not shared between threads. So
// `spawn`ing means we get to declare `static` vars again because of Thread
// Local Storage (TLS). C's `static` is available with `__gshared`.
//
// Should get something like this:
//
//   Thread = Tid(109065000), my state = 0, app state = 0
//   Thread = Tid(109065000), my state = 1, app state = 1
//   Thread = Tid(109065000), my state = 2, app state = 2
//   Thread = Tid(109065200), my state = 0, app state = 3
//   Thread = Tid(109065200), my state = 1, app state = 4
//   Thread = Tid(109065200), my state = 2, app state = 5
//   Thread = Tid(109065300), my state = 0, app state = 6
//   Thread = Tid(109065300), my state = 1, app state = 7
//   Thread = Tid(109065300), my state = 2, app state = 8
//   Thread = Tid(109065400), my state = 0, app state = 9
//   Thread = Tid(109065400), my state = 1, app state = 10
//   Thread = Tid(109065400), my state = 2, app state = 11
//   Thread = Tid(109065500), my state = 0, app state = 12
//   Thread = Tid(109065500), my state = 1, app state = 13
//   Thread = Tid(109065500), my state = 2, app state = 14
void main() {
  for (uint i = 0; i < 5; i++) {
    spawn(&worker, true);
  }
}

void worker(bool firstTime) {
  __gshared int appState = 0;
  static int threadState = 0;

  writeln("Thread = ", thisTid, ", my state = ", threadState++, ", app state = ", appState++);

  if (firstTime) {
    worker(false);
    worker(false);
  }
}
