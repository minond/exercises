# Anagram

Write a program that, given a word and a list of possible anagrams, selects the correct sublist.

Given `"listen"` and a list of candidates like `"enlists" "google" "inlets" "banana"` the program should return a list containing `"inlets"`.

## Making the Test Suite Pass

Execute the tests with:

```bash
$ jasmine-node bob_test.spec.js
```

All but the first test have been skipped.

Once you get a test passing, you can unskip the next one by
changing `xit` to `it`.


## Source

Inspired by the Extreme Startup game [view source](https://github.com/rchatley/extreme_startup)
