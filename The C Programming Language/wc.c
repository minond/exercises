#include <stdio.h>

#define IN  1 // we are in a word
#define OUT 2 // we are outside a word

int main() {
  int c, state, nl, nw, nc;

  nl = nw = nc = 0;
  state = OUT;

  while ((c = getchar()) != EOF) {
    ++nc;

    if (c == '\n') {
      ++nl;
    } else if (c == ' ' || c == '\n' || c == '\t') {
      state = OUT;
    } else if (state == OUT) {
      state = OUT;
      ++nw;
    }
  }

  printf("Lines: %d\n", nl);
  printf("Words: %d\n", nw);
  printf("Chars: %d\n", nc);
}
