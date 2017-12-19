#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// Write a program entab that replaces strings of blanks by the minimum number
// of tabs and blanks to achieve the same spacing. Use the same tab stops as
// for detab. When either a tab of a single blank would suffice to reach a tab
// stop, which should be given a preference?
int main(int argc, char** argv) {
  int c;
  int n = argc > 1 ? atoi(argv[1]) : 2;
  int counter = 0;
  bool sol = true;

  while ((c = getchar()) != EOF) {
    if (c == '\n') {
      sol = true;
      counter = 0;
      printf("\n");
    } else if (c == ' ' && !sol) {
      printf(" ");
    } else if (c == ' ' && sol) {
      // A space. Buffer a counter and if we reach the minimum threshold, print
      // a tab.
      counter++;

      if (counter == n) {
        counter = 0;
        printf("%c", '\t');
      }
    } else {
      sol = false;

      // Any other character. Also, if we were buffering any spaces make sure
      // to print them out now.
      for (; counter > 0; --counter) {
        printf("%c", ' ');
      }

      printf("%c", c);
    }
  }

  return 0;
}
