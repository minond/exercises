import std.stdio : writef;

const int IADD = 1; // int add
const int ISUB = 2; // int substract
const int IMUL = 3; // int multiply
const int ILT = 4; // int less than
const int IEQ = 5; // int equal
const int BR = 6; // branch
const int BRT = 7; // branch if true
const int BRF = 8; // branch if true
const int ICONST = 9; // push constant integer
const int LOAD = 10; // load from local context
const int STORE = 11; // store in local context
const int PRINT = 12; // print stack top
const int POP = 13; // throw away top of stack
const int CALL = 14; // jump
const int RET = 15; // return with/without value
const int HALT = 16; // end

void evaluate(int[] prog) {
  int ip = 0;
  int sp = 0;

  int left;
  int right;

  int[] calls = new int[100];
  int[] stack = new int[100];
  int[] regrs = new int[100];

  while (prog.length > ip && prog[ip] != HALT) {
    writef("<= %s\n", getInstructionName(prog[ip]));

    switch (prog[ip]) {
    case IADD:
      right = stack[--sp];
      left = stack[--sp];
      stack[sp++] = left + right;
      break;

    case ISUB:
      right = stack[--sp];
      left = stack[--sp];
      stack[sp++] = left - right;
      break;

    case IMUL:
      right = stack[--sp];
      left = stack[--sp];
      stack[sp++] = left * right;
      break;

    case ILT:
      right = stack[--sp];
      left = stack[--sp];
      stack[sp++] = left < right;
      break;

    case IEQ:
      right = stack[--sp];
      left = stack[--sp];
      stack[sp++] = left == right;
      break;

    case BR:
      ip = prog[++ip] - 1;
      break;

    case BRT:
      left = prog[++ip];
      if (stack[--sp] == true) {
        ip = left - 1;
      }
      break;

    case BRF:
      left = prog[++ip];
      if (stack[--sp] == false) {

        ip = left - 1;
      }
      break;

    case ICONST:
      stack[sp++] = prog[++ip];
      break;

    case LOAD:
      stack[sp++] = regrs[prog[++ip]];
      break;

    case STORE:
      regrs[prog[++ip]] = stack[--sp];
      break;

    case PRINT:
      writef("=> %s\n", stack[--sp]);
      break;

    case POP:
      sp--;
      break;

    case CALL:
      int addr = prog[++ip];
      int args = prog[++ip];

      calls ~= ip;

      ip = addr - 1;
      sp = sp - args;

      break;

    case RET:
      ip = calls[$ - 1];
      calls = calls[0 .. $ - 1];
      break;

    case HALT:
      ip = int.max;
      break;

    default:
      assert(false);
    }

    ip++;
  }
}

string getInstructionName(int instruction) {
  switch (instruction) {
  case IADD:
    return "IADD";
  case ISUB:
    return "ISUB";
  case IMUL:
    return "IMUL";
  case ILT:
    return "ILT";
  case IEQ:
    return "IEQ";
  case BR:
    return "BR";
  case BRT:
    return "BRT";
  case BRF:
    return "BRF";
  case ICONST:
    return "ICONST";
  case LOAD:
    return "LOAD";
  case STORE:
    return "STORE";
  case PRINT:
    return "PRINT";
  case POP:
    return "POP";
  case CALL:
    return "CALL";
  case RET:
    return "RET";
  case HALT:
    return "HALT";
  default:
    return "<ERR>";
  }
}

void main() {
  writef("Program 1:\n");
  evaluate([ICONST, 1, ICONST, 2, IADD, PRINT, ICONST, 1, ICONST, 2, IADD,
      PRINT, ICONST, 1, ICONST, 2, ICONST, 2, ICONST, 2, POP, IADD, IADD, PRINT, HALT]);

  writef("\n\nProgram 2:\n");
  evaluate([BR, 4, ICONST, 1, ICONST, 2, PRINT,]);

  writef("\n\nProgram 3:\n");
  evaluate([CALL, 11, 0, CALL, 14, 0, CALL, 17, 0, PRINT, HALT, ICONST, 1, RET,
      ICONST, 2, RET, IADD, RET,]);
}
