// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Pseudo code:
//   LOOP:
//   if KBD eq 0
//   then color = 0
//   else color = -1
//   fillScreen(D)
//   goto LOOP

(LOOP)
	@KBD
	D=M
	@BLACK
	D;JNE
	@color
	M=0
	@START_PAINT
	0;JMP

(BLACK)
	@color
	M=-1
	@START_PAINT
	0;JMP

(START_PAINT)
	@8191
	D=A
	@cells
	M=D

(PAINT)
	@color
	D=M

	@AS_BLACK
	D;JNE

// AS_WHILE
	@cells
	D=M
	@SCREEN
	A=A+D
	M=0
	@CONT_PAINT
	0;JMP

(AS_BLACK)
	@cells
	D=M
	@SCREEN
	A=A+D
	M=-1
	@CONT_PAINT
	0;JMP

(CONT_PAINT)
// if cells eq 0
// then goto LOOP
// else sub one and goto PAINT
	@cells
	D=M
	@LOOP
	D;JEQ
	@cells
	M=M-1
	@PAINT
	0;JMP
