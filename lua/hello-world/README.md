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

### Getting started
#### MacOS
First install Lua and [Luarocks][2] using [Homebrew][1]:

```shell
$ brew install lua
```

Then install the [Busted][3] testing framework for Lua:

```shell
$ luarocks install busted
```

Then run your tests:

```shell
$ busted .
```

#### Ubuntu
First install Lua and [Luarocks][2] using [Apt][6]:

```shell
$ sudo apt-get install lua5.3 luarocks
```

Then install the [Busted][3] testing framework for Lua:

```shell
$ luarocks install busted
```

If this fails, you may need to use `sudo`:

```shell
$ sudo luarocks install busted
```

Then run your tests:

```shell
$ busted .
```

#### Windows
First install Lua and [Luarocks][2] using [Chocolatey][7]:

```
C:\> choco install lua
```

Then install the [Busted][3] testing framework for Lua:

```
C:\> luarocks install busted
```

Then run your tests:

```
C:\> busted .
```

#### Other resources

  1. [Lua Style Guide][4]
  2. [Learn Lua in 15 minutes][5]

[1]: http://brew.sh/
[2]: http://luarocks.org/
[3]: http://olivinelabs.com/busted/
[4]: https://github.com/Olivine-Labs/lua-style-guide
[5]: http://tylerneylon.com/a/learn-lua/
[6]: https://help.ubuntu.com/lts/serverguide/apt.html
[7]: http://chocolatey.org/

## Source

This is an exercise to introduce users to using Exercism [http://en.wikipedia.org/wiki/%22Hello,_world!%22_program](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

