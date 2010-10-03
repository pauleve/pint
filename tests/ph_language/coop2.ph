
process a 1
process b 1
process c 1

GRN([
	a 1 -> + b;
	c 1 -> - b
])

COOPERATIVITY([a;c] in [[1;0]], b, 1, 0)


