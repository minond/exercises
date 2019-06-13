#include <stdio.h>

#define MAXLINE 1000

void copy(char to[], char from[]) {
  int i = 0;

  while ((to[i] = from[i]) != '\0') {
    ++i;
  }
}

int getline2(char source[], int limit) {
  int c, i;

  for (i = 0; i < limit - 1 && (c = getchar()) != EOF && c != '\n'; ++i) {
    source[i] = c;
  }

  if (c == '\n') {
    source[i] = c;
    ++i;
  }

  source[i] = '\0';
  return i;
}

int main() {
  int len, max;

  char line[MAXLINE];
  char longest[MAXLINE];

  max = 0;

  while ((len = getline2(line, MAXLINE)) > 0) {
    if (len > max) {
      max = len;
      copy(longest, line);
    }
  }

  if (max > 0) {
    printf("%s", longest);
  }

  return 0;
}
