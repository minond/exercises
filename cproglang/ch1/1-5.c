#include <stdio.h>

#define LOWER  0
#define UPPER  300
#define STEP   20

/**
 * modify the temperature conversion program to print the table in reverse
 * order, that is, from 300 degrees to 0
 */
main()
{
	float fahr, celsius;

	printf("\n");
	printf("fahrenheit   celsius\n");
	printf("----------   -------\n");

	for (fahr = UPPER; fahr >= LOWER; fahr = fahr - STEP) {
		celsius = (5.0 / 9.0) * (fahr - 32.0);
		printf("%10.0f %9.2f\n", fahr, celsius);
	}

	return 0;
}
