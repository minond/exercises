#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "memory.h"
#include "debug.h"
#include "vm.h"

static void run_repl() {
  char line[1024];
  for (;;) {
    printf("> ");

    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }

    /* interpret(line); */
  }
}

static char* read_file(const char* path) {
  FILE* file = fopen(path, "rb");
  if (file == NULL) {
    fprintf(stderr, "Coult not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(file, 0L, SEEK_END);
  size_t file_size = ftell(file);
  rewind(file);

  char* buffer = (char*)malloc(file_size + 1);
  if (buffer == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(74);
  }

  size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
  if (bytes_read < file_size) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(74);
  }

  buffer[bytes_read] = '\0';

  fclose(file);
  return buffer;
}

static void run_file(const char* path) {
  char* source = read_file(path);
  /* InterpretResult result = interpret(source); */
  free(source);

  /* switch (result) { */
  /*   case INTEPRET_COMPILE_ERROR: { exit(65); break; } */
  /*   case INTEPRET_RUNTIME_ERROR: { exit(70); break; } */
  /* } */
}

int main(int argc, const char* argv[]) {
  init_vm();

  if (argc == 1) {
    run_repl();
  } else if (argc == 2) {
    run_file(argv[1]);
  } else {
    fprintf(stderr, "Usage: clox [path]\n");
    exit(64);
  }

  free_vm();

  return 0;
}
