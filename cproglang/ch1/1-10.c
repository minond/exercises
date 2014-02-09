#include <stdio.h>

/**
 * write a program to copy its input to its output, replacing each tab by \t,
 * each backspace by \b, and each backslash by \\. this makes tabs, and
 * backspaces visible in an unambiguous way
 */
main()
{
    int c;

    while ((c = getchar()) != EOF) {
        switch (c) {
            case '\t':
                printf("\\t");
                break;

            case '\b':
                printf("\\t");
                break;

            case '\\':
                printf("\\\\");
                break;

            default:
                putchar(c);
                break;
        }
    }

    return 0;
}
