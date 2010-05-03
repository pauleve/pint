#include <stdio.h>

#define MATHLIB_STANDALONE
#include <Rmath.h>

double qerlang( double area, int shape, double rate, int lower ) {
	return qgamma(area, (double)shape, 1/rate, lower, 0);
}
double rerlang( int shape, double rate ) {
	return rgamma((double) shape, 1/rate);
}

