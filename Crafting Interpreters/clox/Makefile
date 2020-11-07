C_FLAGS = -std=c99 -Wall -Wextra -Werror -Wno-unused-parameter -O3 -flto

SRC = $(shell find *.c)

run: clean compile
	./main

compile:
	$(CC) $(SRC) -o main $(C_FLAGS)

clean:
	@-rm main
