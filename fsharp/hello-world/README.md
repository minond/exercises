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

## Hints
For this exercise the following F# features come in handy:
- [Match Expressions](https://fsharpforfunandprofit.com/posts/match-expression/) Also known as pattern matching which is like a switch statement on steriods. It's a very powerful ability in the F# language and will come in handy in many of the exercises. While this example can be trivially solved with an if/else, pattern matching is more idiomatic.
- [sprintf](https://fsharpforfunandprofit.com/posts/printf/) While F# supports string concatenation using sprintf leads to easier to read solutions and is also idiomatic
- [defaultArg](https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/operators.defaultarg%5B't%5D-function-%5Bfsharp%5D) Default arg is useful when extracting a value from an option type and providing a value when None is encountered.

## Source

This is an exercise to introduce users to using Exercism [http://en.wikipedia.org/wiki/%22Hello,_world!%22_program](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

