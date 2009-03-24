/* gcc -I/usr/share/R/include -L/usr/lib/R/lib/ -lR -o test_qgamma r_gamma.c */

#include <stdio.h>
#include <Rmath.h>
int main() {
	double q, shape, scale, res;
	q = 0.025;
	shape = 50;
	scale = 0.08;
	res = qgamma(q, shape, scale, TRUE, FALSE);
	printf("qgamma(%.5f, shape=%.5f, scale=%.5g, lower.tail=TRUE, log.p=FALSE) = %.5f\n", q, shape, scale, res);
	return 0;
}

#if 0
double _qgamma( double q, double shape, double scale, int lower_tail ) {
	/* double qgamma( double q, double shape, double scale, int lower.tail, int log.p ) */
	printf("qgamma(%.5f,shape=%.5f,scale=%.5f)\n", q, shape, scale);
	return qgamma(q, shape, scale, lower_tail, FALSE);
}
double test_qgamma( double sa, double rate, double q, int lower_tail ) {
	/* double qgamma( double q, double shape, double scale, int lower.tail, int log.p ) */
	double res = _qgamma(q, sa, 1/(rate*sa), lower_tail);
	printf("qerlang(%.5f,rate=%.5f,sa=%.5f,lower=%d) = %.5f\n", q, rate, sa, lower_tail, res);
}

int main() {
	test_qgamma(50, 0.25, 0.025, TRUE);
	test_qgamma(50, 0.25, 0.025, TRUE);
	test_qgamma(50, 0.25, 0.025, TRUE);
	test_qgamma(50, 0.25, 0.025, FALSE);
	test_qgamma(50, 0.25, 0.05, TRUE);
	test_qgamma(50, 0.25, 0.05, FALSE);
	test_qgamma(5, 0.25, 0.025, TRUE);
	test_qgamma(5, 0.25, 0.025, FALSE);
	return 0;
}
#endif


