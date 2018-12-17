module helloworld_test;

string hello(string name = "World") {
  return "Hello, " ~ name ~ "!";
}

unittest {
  assert(hello() == "Hello, World!");
  assert(hello("Alice") == "Hello, Alice!");
  assert(hello("Bob") == "Hello, Bob!");
  assert(hello("") == "Hello, !");
}
