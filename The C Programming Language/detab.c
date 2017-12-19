#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

// Write a program `detab` that replaces tabs in the input with the proper
// number of blanks to space to the next tab stop. Assume a fixed set of tab
// stops, say every n columns. Should n be a variable or a symbolic parameter?
int main(int argc, char** argv) {
  int c;
  bool sol = true;

  int n = atoi(argv[1]);
  char spaces[10];

  for (int i = 0; i < n; i++) {
    sprintf(spaces, "%s%s", spaces, " ");
  }

  while ((c = getchar()) != EOF) {
    if (c == '\n') {
      sol = true;
      printf("\n");
    } else if (sol && c == '\t') {
      printf("%s", spaces);
    } else {
      sol = false;
      printf("%c", c);
    }
  }

  return 0;
}
