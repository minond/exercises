#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
  Chunk* chunk;
  uint8_t* ip;
  Value stack[STACK_MAX];
  Value *stack_top;
} VM;

typedef enum {
  INTEPRET_OK,
  INTEPRET_COMPILE_ERROR,
  INTEPRET_RUNTIME_ERROR
} InterpretResult;

void init_vm();
void free_vm();
InterpretResult interpret(Chunk* chunk);
void push(Value value);
Value pop();

#endif
