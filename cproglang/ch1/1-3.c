#include <stdio.h>

main()
{
	float fahr, celsius;
	int lower, upper, step;

	lower = 0; // lower limit to temp table
	upper = 300; // upper limit to temp table
	step = 20;
	fahr = lower;

	printf("\n");
	printf("fahrenheit   celsius\n");
	printf("----------   -------\n");

	while (fahr <= upper) {
		celsius = (5.0 / 9.0) * (fahr - 32.0);
		printf("%10.0f %9.2f\n", fahr, celsius);
		fahr = fahr + step;
	}

	return 0;
}
