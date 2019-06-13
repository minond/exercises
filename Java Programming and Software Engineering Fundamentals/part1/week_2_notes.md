# Purpose of this module?

- http://www.dukelearntoprogram.com/

- Computational thinking. How to write a computer program.
- Programming concepts and JavaScript syntax.
- Process for solving programming problems.
- Working with libraries provided by Duke.


# Topics to cover

- Seven step process for solving problems.
- Variables, methods, and functions
- Types and the `new` keyword
- Loops and conditionals
- Duke Learn to Program website, Duke libraries, documentation, and coding environment.
- Talk about rewatching videos


#  Seven step process for solving problems.

- Work example by hand
  - First step to solving a problem with code is to understand how to solve the problem by hand.
  - Solve a small instance by hand
  - Make sure you understand the problem
  - Have domain knowledge
- Write down what you did
  - Do this for just the instance of the problem
- Find patterns
  - Make an algorithm for any instance
  - Make use of repetition and conditions
  - Repeat steps 1 and 2 if this step becomes difficult to do
- Check by hand
  - Repeat the algorithm you created in step 3 on a sample problem
- Translate to code
- Run test cases
  - Execute problem on a test case to while you already know the result
- Debug failed test cases
  - Use the scientific method to solve the problem
    - Step 1: Observe a phenomenon
    - Step 2: Ask a question
    - Step 3: Gather information
      - Print statements
      - Debugging tools
      - Execute code by hand
    - Step 4: Form a hypothesis
      - Good hypotheses are:
        - Testable: we know how to test it
        - Actionable: we know how to fix it
    - Step 5: Test hypothesis, go back to step 3 if need to.
  - Return to step three if there is a problem with the algorithm
  - Return to step three if there is a problem with the implementation


# Content

- Everything is a number
  - Computers do math
- No need to worry about bits because of abstractions, "Everything Is a Number?" video.
  - Interface vs implementation
- How are images represented? How are pixels represented?
  - Zero means none of that color and 255 means as much of that color.
- What is The Green Screen problem?
  - https://www.coursera.org/learn/duke-programming-web/lecture/nopgq/developing-an-algorithm
- What is a variable?
  - "Boxes" that hold values
  - Declared with `var` keyword
  - Assined with `=` operator
- What is the `new` keyword?
  - It tells JavaScript to "make an object."
  - An object is piece of data that also has methods.
- What is a `SimpleImage`?
  - It is a part of the Duke library.
- Methods
  - Methods act on an object
  - Might be good to show start of this video: https://www.coursera.org/learn/duke-programming-web/lecture/G9mTf/methods
  - Ask who read the documentation for the Image Coordinate System? Who read the documentation at all?
- Why are methods and functions good?
  - End of this video covers it: https://www.coursera.org/learn/duke-programming-web/lecture/vzYV0/functions
- Types
  - Why do we need types?
    - So that we know how to interpret and operate on a value
    - A good example is addition vs concatenation
  - Dynamic typing vs Static typing
    - Dynamic typing
      - The type of a variable can change as the program runs
    - Static typing
      - The type of a variable cannot change as the program runs and must be declared before the program can run
      - This is how Java can check for mistakes before you run your program
- Duke Learn to Program website
  - Video that talks about this: https://www.coursera.org/learn/duke-programming-web/lecture/ToB5T/dukelearntoprogram-environment
- Loops
  - Video covering syntax, semantics, and an example: https://www.coursera.org/learn/duke-programming-web/lecture/qZ692/for-loops
- Conditionals
  - Video covering syntax, semantics, and an example: https://www.coursera.org/learn/duke-programming-web/lecture/7A4Ku/conditional-execution


# Exercises

- http://www.dukelearntoprogram.com/course1/example/index.php
- Write a function that adds three numbers together.
- Write a function that adds two strings. Remember that addings strings means concatenating, so “a” + “b” is “ab” and “b” + “a” is “ba”.
- Write a function that prints the width and height of an image.

- https://www.coursera.org/learn/duke-programming-web/supplement/XwS1Y/try-it-using-for-loops
- http://www.dukelearntoprogram.com/course1/doc/#simpleimage
- Make a yellow square that is 200 pixels wide and 200 pixels high
  - Create a new image, specifying that the new image is 200 pixels wide and 200 pixels high.
  - Then, for each pixel in that image: Make the pixel yellow.
    - Remember that yellow pixels have a red value of 255, a green value of 255, and a blue value of 0.

```js
// Write a JavaScript program that modifies an image by putting three vertical
// stripes on it - a red stripe on the left one third, a green stripe in the
// middle, and a blue stripe on the right one third. For example, if your
// program ran on Drew’s picture shown on the left, the resulting image would
// have red, green and blue vertical stripes as shown in the image on the
// right.
var img = new SimpleImage("smallpanda.png");
var thirdWidth = img.getWidth() / 3;

for (var pixel of img.values()) {
  var x = pixel.getX();
  if (x < thirdWidth) {
    pixel.setRed(255);
  } else if (x < thirdWidth * 2) {
    pixel.setGreen(255);
  } else {
    pixel.setBlue(255);
  }
}

print(img);


// Write a JavaScript function named swapRedGreen with one parameter pixel
// (representing a single pixel). This function should swap the red and green
// values of the pixel. For example, if you have a pixel with red = 255, green
// = 100, blue = 150, after calling swapRedGreen on that pixel its new RGB
// values would be red = 100, green = 255, blue = 150.
function swapRedGreen(pixel) {
  var red = pixel.getRed();
  pixel.setRed(pixel.getGreen());
  pixel.setGreen(red);
}

var img = new SimpleImage("drewgreen.png");
for (var pixel of img.values()) {
  swapRedGreen(pixel);
}

print(img);


// Write code to change the Duke blue devil (the image below on the left) to be
// yellow (as in the image below on the right).
var img = new SimpleImage("duke_blue_devil.png");
for (var pixel of img.values()) {
  if (pixel.getRed() + pixel.getGreen() + pixel.getBlue() !== 255 * 3) {
    pixel.setRed(255);
    pixel.setGreen(255);
    pixel.setBlue(0);
  }
}

print(img);


// Write the green screen algorithm you saw in the lecture video yourself. To
// make sure you really understand the code that was written in the video, you
// should write the code yourself without looking at the video unless you get
// stuck and need to refer back to it for a hint.
function greenscreen(foreground, background) {
  var width = foreground.getWidth();
  var height = foreground.getHeight();

  var output = new SimpleImage(width, height);

  for (var pixel of foreground.values()) {
    var x = pixel.getX();
    var y = pixel.getY();

    // Is this a green pixel?
    if (pixel.getGreen() > pixel.getRed() + pixel.getBlue()) {
      // If so then use the corresponding pixel from the background images
      output.setPixel(x, y, background.getPixel(x, y));
    } else {
      // Otherwise use the pixel from the foreground image
      output.setPixel(x, y, pixel);
    }
  }

  return output;
}

var foreground = new SimpleImage("drewRobert.png");
var background = new SimpleImage("dinos.png");

print(greenscreen(foreground, background));


// Your friend is trying to write a program that draws a square 200 pixels by
// 200 pixels and that looks like this square with colors red (red value 255),
// green (green value 255), blue (blue value 255) and magenta (red value 255
// and blue value 255). All other RGB values are set to 0.
var img = new SimpleImage(200,200);
for (var px of img.values()) {
  var x = px.getX();
  var y = px.getY();

  if (x < img.getWidth() / 2) {
    px.setRed(255);
  }

  if (y > img.getHeight() / 2) {
    px.setBlue(255);
  } else if (x >= img.getWidth() / 2) {
    px.setGreen(255);
  }
}

print(img);


// Write a function named setBlack that has one parameter pixel (representing a
// single pixel) and returns pixel with its red, green, and blue components
// changed so that the pixel’s color is black.
function setBlack(pixel) {
  pixel.setRed(0);
  pixel.setGreen(0);
  pixel.setBlue(0);
  return pixel;
}

// Now you will write another function named addBorder. This function will add
// a black border to an image, such as in the following example:
//
// Hints:
//
//  - The function will require two parameters: the image you want to modify,
//  and the thickness of the border. Remember that you can name parameters
//  whatever you want, but it is good to name them something informative. For
//  example, in this case, you may want to use names like image and thickness.
//
//  - Remember that you wrote a setBlack function.
//
//  - Remember that images have a getWidth() method and a getHeight() method.
function addBorder(image, thickness) {
  var width = image.getWidth();
  var height = image.getHeight();

  for (var pixel of image.values()) {
    var x = pixel.getX();
    var y = pixel.getY();

    if (x <= thickness) {
      setBlack(pixel);
    }

    if (x >= width - thickness) {
      setBlack(pixel);
    }

    if (y <= thickness) {
      setBlack(pixel);
    }

    if (y >= height - thickness) {
      setBlack(pixel);
    }
  }

  return image;
}

print(addBorder(new SimpleImage("smallpanda.png"), 10));
```
