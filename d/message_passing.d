import std.concurrency : spawn, thisTid, receive, receiveOnly, send, Tid;
import std.string : format;
import std.stdio : writeln;

// Threads communicate with messages. Threads can receive messages from parent
// threads with the `receive` functions, which is  a lot like a switch
// statement that passes messages to delegates (functions) with matching
// signatures. `receive` blocks until a message is received.
//
//   import std.concurrency : spawn, thisTid;
//   auto childTId = spawn(&foo, thisTid);
//
//   void foo(Tid parentTid) {
//     receive(
//       (int i) {
//         writeln("Got ", i, " from parent thread.");
//       }
//     );
//
//     send(parentTid, "Done");
//   }
void main() {
  Tid[] threads;

  for (uint i = 0; i < 10; i++) {
    threads ~= spawn(&worker, thisTid);
  }

  foreach (int i, ref tid; threads) {
    if (i % 2) {
      send(tid, NumberMessage(i));
    }
    else {
      send(tid, format("T=%d", i));
    }
  }

  foreach (ref tid; threads) {
    send(tid, CancelMessage());
  }

  foreach (ref tid; threads) {
    receiveOnly!CancelAckMessage;
    writeln("Got ack from ", tid);
  }
}

struct NumberMessage {
  int n;

  this(int i) {
    this.n = i;
  }
}

struct CancelMessage {
}

struct CancelAckMessage {
}

void worker(Tid parentTid) {
  bool cancelled = false;

  auto receiveNumber = (NumberMessage m) { writeln("Got number ", m.n); };
  auto receiveString = (string text) { writeln("Got the string ", text); };

  auto receiveCancel = (CancelMessage m) {
    writeln("Stopping ", thisTid);
    send(parentTid, CancelAckMessage());
    cancelled = true;
  };

  writeln("Starting ", thisTid, "...");

  while (!cancelled) {
    receive(receiveNumber, receiveString, receiveCancel);
  }
}
