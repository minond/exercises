#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

typedef struct {
  Token current;
  Token previous;
  bool had_error;
  bool panic_mode;
} Parser;

Parser parser;
Chunk* compiling_chunk;


typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;


static void expression();
static void parse_precedence(Precedence precedence);


static void error_at(Token* token, const char* message) {
  if (parser.panic_mode) {
    return;
  }

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.had_error = true;
  parser.panic_mode = true;
}

static void error_at_current(const char* message) {
  error_at(&parser.current, message);
}

static void error(const char* message) {
  error_at(&parser.previous, message);
}


static Chunk* current_chunk() {
  return compiling_chunk;
}

static uint8_t make_constant(Value value) {
  int constant = add_constant(current_chunk(), value);
  if (constant > UINT8_MAX) {
    error("too many constants in one chunk");
    return 0;
  }

  return (uint8_t)constant;
}

static void emit_byte(uint8_t byte) {
  write_chunk(current_chunk(), byte, parser.previous.line);
}

static void emit_bytes(uint8_t byte1, uint8_t byte2) {
  emit_byte(byte1);
  emit_byte(byte2);
}

static void emit_return() {
  emit_byte(OP_RETURN);
}

static void emit_constant(Value value) {
  emit_bytes(OP_CONSTANT, make_constant(value));
}

static void end_compiler() {
  emit_return();
}

static void binary() {
  TokenType operator_type = parser.previous.type;
  ParseRule* rule = get_rule(operator_type);
  parse_precedence((Precedence)(rule->precedence - 1));

  switch (operator_type) {
  case TOKEN_MINUS: emit_byte(OP_SUBTRACT); break;
  case TOKEN_PLUS:  emit_byte(OP_ADD);      break;
  case TOKEN_SLASH: emit_byte(OP_DIVIDE);   break;
  case TOKEN_STAR:  emit_byte(OP_MULTIPLY); break;
  default:          return;
  }
}


static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scan_token();
    if (parser.current.type != TOKEN_ERROR) {
      break;
    }

    error_at_current(parser.current.start);
  }
}

static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  error_at_current(message);
}


static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "expect ')' after expression");
}

static void number() {
  double value = strtod(parser.previous.start, NULL);
  emit_constant(value);
}

static void unary() {
  TokenType operator_type = parser.previous.type;

  parse_precedence(PREC_UNARY);

  switch (operator_type) {
  case TOKEN_MINUS:
    emit_byte(OP_NEGATE);
    return;

  default:
    return;
  }
}

static void parse_precedence(Precedence precedence) {
}

static void expression() {
  parse_precedence(PREC_ASSIGNMENT);
}

bool compile(const char* source, Chunk* chunk) {
  init_scanner(source);

  compiling_chunk = chunk;

  parser.had_error = false;
  parser.panic_mode = false;

  advance();
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");

  end_compiler();

  return !parser.had_error;
}
