#include <stdio.h>

unsigned long djb2(unsigned char *str) {
  unsigned long hash = 5381;
  int c;

  while (c = *str++) {
    hash = ((hash << 5) + hash) + c;
  }

  return hash;
}

static unsigned long sdbm(unsigned char* str) {
  unsigned long hash = 0;
  int c;

  while (c = *str++) {
    hash = c + (hash << 6) + (hash << 16) - hash;
  }

  return hash;
}

int main(int argc, char** argv) {
  (void) argc;
  (void) argv;

  printf("%lu\n", djb2("123"));
  printf("%lu\n", sdbm("123"));

  return 0;
}
