<!--
$theme: default
page_number: true
footer: Java Class - Week 2
-->

# Week 2 review

---

## What should I know after week 2?

- Variables and the `new` keyword.
- Functions and methods.
- Loops and conditionals.
- Duke Learn to Program website, Duke libraries, documentation, and coding environment.
- Seven step process for solving problems.

---

#### Variables

- Variables are how we give names to values.
- Declaration vs initialization
- `var` says "I am declaring a new variable."

<br>

```js
  var age   =   42   ;
//  ^   ^   ^   ^    ^
//  |   |   |   |    |
//  |   |   |   |    +---> Semicolon at end of statement.
//  |   |   |   |
//  |   |   |   +---> The value.
//  |   |   |
//  |   |   +---> Equal sign.
//  |   |
//  |   +---> The name of the variable
//  |
//  +---> "var" keyword
```

---

#### Variables, a test

What is the value of `x` after the code below is executed?

<br><br>

```js
var x = 2;
var y = 5;

y = y * y;
x = x + y;
```

---

#### `new` keyword

- The `new` keyword says "make an object."


```js
var image_1 = new SimpleImage("smalluniverse.jpg");
var image_2 = new SimpleImage("lion.jpg");
//            ^   ^           ^
//            |   |           |
//            |   |           |
//            |   |           |
//            |   |           +---> Parameters passed
//            |   |                 for the constructor.
//            |   |
//            |   +---> Name of the type to create.
//            |
//            +---> "new" keyword
//
//            \--------------------------------------/
//                                |
//                                |
//                                +---> The value.
```

---

#### Functions

- Functions perform a set of operations.
- Semantics:
  - Execution goes into the function.
  - Run the code in the body of the function.
  - The function returns a value.
  - Function call is replaced with value.
  - Execution resumes.
- Let's write a function that takes a number and adds it three times to itself and returns that result.

---

#### Functions, an example

```js
function triple(num) {
  return num + num + num;
}

var x = 13;
var y = triple(x) + 3;
```

---

#### Functions, an example

```js
function triple(num) {
  return num + num + num;
}

var x = 13;
var y = triple(13) + 3;
```

---

#### Functions, an example

```js
function triple(num) {
  return num + num + num;
}

var x = 13;
var y = 39 + 3;
```

---

#### Functions, an example

```js
function triple(num) {
  return num + num + num;
}

var x = 13;
var y = 42;
```

---

#### Methods

- Methods are functions that are owned by and operate on an object.

```js
var image_1 = new SimpleImage("smalluniverse.jpg");
var image_2 = new SimpleImage("lion.jpg");

print("Width of image_1 is " + image_1.getWidth());
print("Width of image_2 is " + image_2.getWidth());
```

---

#### Why are functions and methods good?

https://www.coursera.org/learn/duke-programming-web/lecture/vzYV0/functions _[minute 5:06]_

---

#### Types

Types give data meaning. Because of types, the computer language (JavaScript) can operate on strings differently from numbers, and number differently from SimpleImages.

- Everything is a Number vide: https://www.coursera.org/learn/duke-programming-web/lecture/gXWwh/everything-is-a-number
- Types video: https://www.coursera.org/learn/duke-programming-web/lecture/1VDy3/types

---

#### Types, an example

```js
var n1 = 26;
var n2 = 16;

var s1 = "a";
var s2 = "b";

var n3 = n1 + n2;
var s3 = s1 + s2;
```

---

##### Dynamic typing vs Static typing

---

##### Dynamic typing

- The type of a variable can change as the program runs

```js
// OK
var x;

x = 123;

x = "abc";
```

---

##### Static typing

- The type of a variable cannot change as the program runs and must be declared before the program can run
- This is how Java can check for mistakes before you run your program

<br>

```java
// OK
String firstName = "Marcos";
```

```java
// ERROR
int age = 83;
age = "123";
```

---

#### Loops

- `var pixel` refers to the current item. The loop will automatically update this variable on every iteration of the loop.
- `of` is a special keyword we use in this type of loop. It tells JavaScript that you will be looping over an array.
- `image.values()` evaluates to the data we will loop over.
- The body of the loop is repeated of every piece of data.

```js
var image = new SimpleImage("lion.jpg");

for (var pixel of image.values()) {
  pixel.setRed(255);
}

print(image);
```

---

Loops video which walks through a four pixel image: https://ww	w.coursera.org/learn/duke-programming-web/lecture/qZ692/for-loops

---

#### Conditionals

```js
var x = 12;
var y = 34;

if (x >= y) {
  print("x is greater than or equal to y");
} else {
  print("x is less than y");
}
```

---

#### Conditionals, spot the problem

```js
// I expect to see an image with equal red, green,
// and blue parts.
var image = new SimpleImage(30, 10);

for (var pixel of image.values()) {
  if (pixel.getX() < 10) {
    pixel.setRed(255);
  }

  if (pixel.getX() < 20) {
    pixel.setGreen(255);
  }

  if (pixel.getX() < 30) {
    pixel.setBlue(255);
  }
}

print(image);
```
---

#### Conditionals, fix the problem

```js
// I now see an image with equal red, green,
// and blue parts.
var image = new SimpleImage(30, 10);

for (var pixel of image.values()) {
  if (pixel.getX() < 10) {
    pixel.setRed(255);
  } else if (pixel.getX() < 20) {
    pixel.setGreen(255);
  } else if (pixel.getX() < 30) {
    pixel.setBlue(255);
  }
}

print(image);
```

---

#### Duke Learn to Program website, Duke libraries, documentation, and coding environment.

###### http://www.dukelearntoprogram.com/

---

#### Seven step process for solving problems.

1. Work example by hand
1. Write down what you did
1. Find patterns
1. Check by hand
1. Translate to code
1. Run test cases
1. Debug failed test cases

---

Part 1 and part 3, https://www.coursera.org/learn/duke-programming-web/supplement/4x9Bk/programming-exercise-advanced-modifying-images
