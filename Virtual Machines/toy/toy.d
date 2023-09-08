/**

                          TOY REFERENCE CARD
      https://introcs.cs.princeton.edu/java/60machine/reference.txt


      INSTRUCTION FORMATS

                   | . . . . | . . . . | . . . . | . . . .|
        Format RR: | opcode  |    d    |    s    |    t   |  (0-6, A-B)
        Format A:  | opcode  |    d    |       addr       |  (7-9, C-F)


      ARITHMETIC and LOGICAL operations
          1: add              R[d] <- R[s] +  R[t]
          2: subtract         R[d] <- R[s] -  R[t]
          3: and              R[d] <- R[s] &  R[t]
          4: xor              R[d] <- R[s] ^  R[t]
          5: shift left       R[d] <- R[s] << R[t]
          6: shift right      R[d] <- R[s] >> R[t]

      TRANSFER between registers and memory
          7: load address     R[d] <- addr
          8: load             R[d] <- M[addr]
          9: store            M[addr] <- R[d]
          A: load indirect    R[d] <- M[R[t]]
          B: store indirect   M[R[t]] <- R[d]

      CONTROL
          0: halt             halt
          C: branch zero      if (R[d] == 0) PC <- addr
          D: branch positive  if (R[d] >  0) PC <- addr
          E: jump register    PC <- R[d]
          F: jump and link    R[d] <- PC; PC <- addr


      Register 0 always reads 0.
      Loads from M[FF] come from stdin.
      Stores to  M[FF] go to stdout.

      16-bit registers (using two's complement arithmetic)
      16-bit memory locations
       8-bit program counter

*/

import std.array : split;
import std.conv : to;
import std.file : readText;
import std.regex : matchAll, regex;
import std.stdio : readln, writeln, writef;
import std.string : chop;

static const REG_SIZE = 16;
static const MEM_SIZE = 256;
static const VIEW_PAGE_SIZE = 8;

struct Program {
  string[2][] instructions;
}

void dump(int[] register, int[] memory, uint pc) {
  writeln("Registers:");
  writeln("");

  for (int i = 0; i < REG_SIZE; i++) {
    writef(" %016X  ", register[i]);

    if ((i + 1) % VIEW_PAGE_SIZE == 0) {
      writeln("");
    }
  }

  writeln("");
  writeln("");
  writeln("Main memory:");
  writeln("");

  for (int i = 0; i < MEM_SIZE; i++) {
    if ((i + 0) == pc) {
      writef("[%016X] ", memory[i]);
    }
    else {
      writef(" %016X  ", memory[i]);
    }

    if ((i + 1) % VIEW_PAGE_SIZE == 0) {
      writeln("");
    }
  }
}

void load(Program program, int[] memory) {
  for (int i = 0; i < MEM_SIZE; i++) {
    memory[i] = 0x0;
  }

  for (int i = 0; i < program.instructions.length; i++) {
    auto loc = to!int(program.instructions[i][0], 16);
    auto val = to!int(program.instructions[i][1], 16);
    memory[loc] = val;
  }
}

Program read(string filePath) {
  Program program;

  auto contents = readText(filePath);
  auto matches = matchAll(contents, regex("[0-9A-F]{2}: [0-9A-F]{4}"));

  for (; !matches.empty; matches.popFront) {
    auto parts = matches.front.hit.split(": ");
    program.instructions ~= [parts[0], parts[1]];
  }

  return program;
}

void exec(Program program, int[] memory, int[] register, bool debugMode) {
  uint pc = 0x10;

  runner: while (1) {
    int instruction = memory[pc];

    // Instructions are made up of four bytes. Staring with:
    int opcode = instruction >> 12; // 0 - 4
    int destination = (instruction >> 8) & 15; // 5 - 8
    int lhs = (instruction >> 4) & 15; // 9 - 12
    int rhs = instruction & 15; // 13 - 16

    // Format A's addr takes up two bytes
    int address = instruction & 255;

    if (debugMode) {
      writef("%016X - %04X %04X [%04X, %04X | %08X]\n", instruction, opcode,
          destination, lhs, rhs, address);
    }

    pc = pc + 1;

    switch (opcode) {
      // HALT
    case 0x00:
      break runner;

      // ARITHMETIC and LOGICAL operations
    case 0x01:
      register[destination] = register[lhs] + register[rhs];
      break;

    case 0x02:
      register[destination] = register[lhs] - register[rhs];
      break;

    case 0x03:
      register[destination] = register[lhs] & register[rhs];
      break;

    case 0x04:
      register[destination] = register[lhs] ^ register[rhs];
      break;

    case 0x05:
      register[destination] = register[lhs] << register[rhs];
      break;

    case 0x06:
      register[destination] = register[lhs] >> register[rhs];
      break;

      // TRANSFER between registers and memory
    case 0x07:
      register[destination] = address;
      break;

    case 0x08:
      // Loads from M[FF] come from stdin.
      if (address == 0xFF) {
        memory[address] = to!int(chop(readln()), 16);
      }

      register[destination] = memory[address];
      break;

    case 0x09:
      memory[address] = register[destination];

      // Stores to  M[FF] go to stdout.
      if (address == 0xFF) {
        writef("%04X\n", register[destination]);
      }

      break;

    case 0x0A:
      // Loads from M[FF] come from stdin.
      if (register[rhs] == 0xFF) {
        memory[0xFF] = to!int(chop(readln()), 16);
      }

      register[destination] = memory[register[rhs]];
      break;

    case 0x0B:
      memory[register[rhs]] = register[destination];

      // Stores to  M[FF] go to stdout.
      if (register[rhs] == 0xFF) {
        writef("%04X\n", register[destination]);
      }

      break;

      // CONTROL
    case 0x0C:
      if (register[destination] == 0) {
        pc = address;
      }

      break;

    case 0x0D:
      if (register[destination] > 0) {
        pc = address;
      }

      break;

    case 0x0E:
      pc = register[destination];
      break;

    case 0x0F:
      register[destination] = pc;
      pc = address;
      break;

    default:
      assert(0);
    }

    // Register 0 always reads 0.
    register[0] = 0;
  }
}

void main(string[] args) {
  auto debugMode = true;

  if (args.length < 2) {
    writeln("Usage: rdmd toy.d <FILE>");
    return;
  }

  int[] register = new int[REG_SIZE];
  int[] memory = new int[MEM_SIZE];

  Program program = read(args[1]);
  load(program, memory);
  exec(program, memory, register, debugMode);

  if (debugMode) {
    writeln("");
    dump(register, memory, 0x0);
  }
}
