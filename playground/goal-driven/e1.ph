process a 1
process b 1
process c 1
process d 1
process e 1

BRN([
	a 1 -> +  c; 
	b 1 -> + c; 
	b 1 -> + d;
	b 1 -> + a;
	e 1 -> + c])

COOPERATIVITY([e;a] -> c 0 1, [[1;1]])

initial_state b 1

