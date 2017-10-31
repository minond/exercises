#include <stdio.h>

#define CAN_REPLACE 0
#define REPLACED    1

/**
 * write a program to copy its input to its output, replacing each string of
 * one or more blanks by a single blank
 */
main()
{
    int state, c;

    state = CAN_REPLACE;

    while ((c = getchar()) != EOF) {
        if (c == ' ' || c == '\t') {
            if (state == CAN_REPLACE) {
                state = REPLACED;
                putchar(c);
            }
        } else {
            state = CAN_REPLACE;
            putchar(c);
        }
    }

    return 0;
}
