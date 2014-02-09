#include <stdio.h>

/**
 * write a program to print the corresponding Celsius to Fahrenheit table
 */
main()
{
    float fahr, celsius;
    int lower, upper, step;

    lower = 0; // lower limit to temp table
    upper = 300; // upper limit to temp table
    step = 20;
    celsius = lower;

    printf("\n");
    printf("fahrenheit   celsius\n");
    printf("----------   -------\n");

    while (celsius <= upper) {
        fahr = (celsius * 180.0 / 100.0) + 32.0;
        printf("%10.0f %9.2f\n", fahr, celsius);
        celsius = celsius + step;
    }

    return 0;
}
