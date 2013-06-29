#include <stdio.h>

/**
 * experiment to find out what happens when printf's argument string contains
 * \c, where c is some character not listed above.
 */
main()
{
	printf("slash n: ");
	printf("\n");
	printf(".");

	printf("slash a: ");
	printf("\a");
	printf(".");
}
