# Standard stuff. Also has a `block` statement which enable labels.
import strutils, random

randomize()

let answer = random(10) + 1

while true:
  echo "I have a number from 1 to 10, what is it? "
  let guess = parseInt(stdin.readLine)

  if guess < answer:
    echo "Too low"
  elif guess > answer:
    echo "Too high"
  else:
    echo "Correct!"
    break


block myloop:
  while true:
    while true:
      break myloop
