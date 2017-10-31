#include <stdio.h>

/**
 * write a program to count blanks, tabs, and newlines
 */
main()
{
    int c;
    long blanks, tabs, newlines;

    blanks = 0;
    tabs = 0;
    newlines = 0;

    while ((c = getchar()) != EOF) {
        switch (c) {
            case '\n':
                newlines++;
                break;

            case '\t':
                tabs++;
                break;

            case ' ':
                blanks++;
                break;

            default:
                break;
        }
    }

    printf("blanks: %6.0ld\n", blanks);
    printf("tabs: %8.0ld\n", tabs);
    printf("newlines: %4.0ld\n", newlines);

    return 0;
}
