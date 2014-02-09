#include <stdio.h>

/**
 * verify that the expression 'getchar() != EOF' is 0 or 1
 */
main()
{
    int c;

    c = getchar() != EOF;
    printf("valud of 'getchar() != EOF' is: %i", c);

    return 0;
}
