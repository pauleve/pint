/* gcc `pkg-config --cflags --libs libRmath` -o r_plot_erlang r_plot_erlang.c */

#include <stdio.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>

/* confidence 95% */
#define C 0.025  

void printpts( double r, int sa ) {
	double shape = (double) sa;
	double scale = 1/(shape*r);
	double a = qgamma(C, shape, scale, 1, 0);
	double b = qgamma(C, shape, scale, 0, 0);
	printf("%.5f\t%d\t%.5f\t%.5f\n", r, sa, a, b);
}

#define lsa_min 0
#define lsa_max 4.5
#define lsa_step 0.1
#define lr_min -8
#define lr_max 2
#define lr_step 0.1

int main( int argc, char** argv ) {
	double lsa;
	int sa=-1, sa2;
	double lr;
	int sort_rate = 0;
	if ( argc > 1 && strcmp(argv[1], "rate") == 0 ) {
		sort_rate = 1;
	}

	if ( !sort_rate ) {
		for ( lsa = lsa_step; lsa <= lsa_max; lsa += lsa_step ) {
			sa2 = (int)exp(lsa);
			if ( sa2 == sa ) continue;
			sa = sa2;
			for ( lr = lr_min; lr <= lr_max; lr += lr_step ) {
				printpts(exp(lr), sa);
			}
			printf("\n");
		}
	} else {
		for ( lr = lr_min; lr <= lr_max; lr += lr_step ) {
			for ( lsa = lsa_step; lsa <= lsa_max; lsa += lsa_step ) {
				sa2 = (int)exp(lsa);
				if ( sa2 == sa ) continue;
				sa = sa2;
				printpts(exp(lr), sa);
			}
			printf("\n");
		}
	}
	return 0;
}


