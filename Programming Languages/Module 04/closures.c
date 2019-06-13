#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct List {
  int head;
  struct List* tail;
} List;

List* cons(int h, List* t) {
  List* l = (List*) malloc(sizeof(List));
  l->head = h;
  l->tail = t;
  return l;
}

int size(List* xs) {
  if (xs == NULL)
    return 0;

  return 1 + size(xs->tail);
}

List* map(int (*f)(void*, int), void* env, List* xs) {
  if (xs == NULL)
    return NULL;

  return cons(f(env, xs->head), map(f, env, xs->tail));
}

List* filter(bool (*f)(void*, int), void* env, List* xs) {
  if (xs == NULL)
    return NULL;
  else if (f(env, xs->head))
    return cons(xs->head, filter(f, env, xs->tail));
  else
    return filter(f, env, xs->tail);
}

List* range(int max) {
  List* l = NULL;

  do {
    l = cons(max, l);
  } while (max-- != 0);

  return l;
}

void print(List* xs) {
  printf("(");

  if (xs == NULL) {
    printf("nil");
  } else {
    printf("%i ", xs->head);
    print(xs->tail);
  }

  printf(")");
}

void println(List* xs) {
  print(xs);
  printf(" (size = %i)\n", size(xs));
}

int sum(void* x, int y) {
  return (intptr_t) x + y;
}

bool divisible(void* by, int n) {
  return n % (intptr_t) by == 0;
}

int main(void) {
  List* xs = range(20);
  List* ys = map(&sum, (void*) 5, xs);
  List* es = filter(&divisible, (void*) 3, xs);

  println(xs);
  println(ys);
  println(es);

  return 0;
}
