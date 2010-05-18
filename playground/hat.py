
import math

def sa_hat( u, v, (d, D) ) :
	return math.exp(u*math.pow(D/d,v))
def r_hat( w, x, y, (d, D) ) :
	return (w + x*math.exp(-y*d))/(d+D)

def sa_hat_99( d, D ) :	return sa_hat(6.41, -1.04, (d,D))
def  r_hat_99( d, D ) : return  r_hat(2.03, 1.39, 33.33, (d,D))


