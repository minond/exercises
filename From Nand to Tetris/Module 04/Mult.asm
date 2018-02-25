// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Pseudo code:
//   R2 = 0
//   remaining = R1
//
//   LOOP:
//   if (remaining eq 0) goto END
//
//   remaining = remaining - 1
//   R2 = R2 + R0
//   goto LOOP
//
//   END:
//   exit

// R2 = 0
// remaining = R1
	@R2
	M=0

	@R1
	D=M
	@remaining
	M=D

// LOOP:
// if (remaining eq 0) goto END
(LOOP)
  @remaining
  D=M
  @END
  D;JEQ

// remaining = remaining - 1
// R2 = R2 + R0
// goto LOOP
  @remaining
  M=M-1
  @R0
  D=M
  @R2
  M=M+D
  @LOOP
  0;JMP

// END:
// exit
(END)
  @END
  0;JMP
