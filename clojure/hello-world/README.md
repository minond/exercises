# Hello World

Write a function that greets the user by name, or by saying "Hello, World!" if no name is given.

["Hello, World!"](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program) is
the traditional first program for beginning programming in a new language.

**Note:** You can skip this exercise by running:

    exercism skip $TRACK_ID hello-world

## Specification

Write a `Hello World!` function that can greet someone given their name.  The
function should return the appropriate greeting.

For an input of "Alice", the response should be "Hello, Alice!".

If a name is not given, the response should be "Hello, World!"

## Test-Driven Development

As programmers mature, they eventually want to test their code.

Here at Exercism we simulate [Test-Driven
Development](http://en.wikipedia.org/wiki/Test-driven_development) (TDD), where
you write your tests before writing any functionality. The simulation comes in
the form of a pre-written test suite, which will signal that you have solved
the problem.

It will also provide you with a safety net to explore other solutions without
breaking the functionality.

### A typical TDD workflow on Exercism:

1. Run the test file and pick one test that's failing.
2. Write some code to fix the test you picked.
3. Re-run the tests to confirm the test is now passing.
4. Repeat from step 1.
5. Submit your solution (`exercism submit /path/to/file`)

## Instructions

Submissions are encouraged to be general, within reason. Having said that, it's
also important not to over-engineer a solution.

It's important to remember that the goal is to make code as expressive and
readable as we can. However, solutions to the hello-world exercise will not be
reviewed by a person, but by rikki- the robot, who will offer an encouraging
word.

### Project Structure

Clojure exercises in exercism use [leiningen](http://leiningen.org/) to configure and run your code
and use [leiningen standard directory structure](https://github.com/technomancy/leiningen/blob/master/doc/TUTORIAL.md#directory-layout).

You will find a test file named `hello_world_test.clj` inside `test` directory.
Write your code in `src/hello_world.clj`. It should use the namespace `hello-world` so that tests can pick it up.

### Running tests

Run the tests using `lein test` command and make them pass:

```
$ lein test

lein test hello-world-test

Ran 3 tests containing 3 assertions.
0 failures, 0 errors.
```

Then submit the exercise using:

```
$ exercism submit src/hello_world.clj
```

For more detailed instructions and learning resources refer [exercism's clojure language page](http://exercism.io/languages/clojure).

## Source

This is an exercise to introduce users to using Exercism [http://en.wikipedia.org/wiki/%22Hello,_world!%22_program](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

