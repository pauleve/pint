
W = [-2, -1, 2, 1]
T = 3

W,T = [-1,5,-1],-1
W,T = [-2,1,-2,-1],-1
W,T = [3,2,1,-2], 0
W,T = [4,1,1,-1], 0

n = len(W)

def int_to_tuple( i ) :
	return tuple(reversed([i >> b & 1 for b in range(0,n)]))

def sum_tuple( t ) :
	return sum([s == 0 and -w or w for (s,w) in zip(t,W)])

sup, less, eq = [],[],[]

for i in range(0,2**n) :
	t = int_to_tuple(i)
	s = sum_tuple(t)
	if s > T :
		sup.append(t)
	elif s < T :
		less.append(t)
	else :
		eq.append(t)

def str_of_tuple( t ) :
	return "[" + ";".join(map(str,t)) + "]"

def str_of_ttuple( t ) :
	return str_of_tuple(map(str_of_tuple, t))

print(str_of_ttuple(sup))
print(str_of_ttuple(less))
print(str_of_ttuple(eq))

