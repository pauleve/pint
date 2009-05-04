/* gcc `pkg-config --cflags --libs libRmath` -o dsum dsum.c */

#include <stdio.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>

/* confidence 95% */
#define C 0.025  

void printpts( double r, int k1, int k2 ) {
	double d1 = qgamma(C, (double)k1, 1/r, 1, 0);
	double d2 = qgamma(C, (double)k2, 1/r, 1, 0);
	double d = qgamma(C, (double)(k1+k2), 1/r, 1, 0);
	printf("%.5f\t%.5f\t%.5f\n", d1, d2, d);
}

#define lsa_min 0
#define lsa_max 3.5
#define lsa_step 0.2
#define lr_min -5
#define lr_max 5
#define lr_step 0.1

int main() {
	double lsa1, lsa2;
	int sa1=-1, sa1p, sa2=-1, sa2p;
	double lr;

	for ( lsa1 = lsa_step; lsa1 <= lsa_max; lsa1 += lsa_step ) {
		sa1p = (int)exp(lsa1);
		if ( sa1p == sa1 ) continue;
		sa1 = sa1p;
		for ( lsa2 = lsa1; lsa2 <= lsa_max; lsa2 += lsa_step ) {
			sa2p = (int)exp(lsa2);
			if ( sa2p == sa2 ) continue;
			sa2 = sa2p;
			for ( lr = lr_min; lr <= lr_max; lr += lr_step ) {
				printpts(exp(lr), sa1, sa2);
			}
		}
	}

	return 0;
}
	

