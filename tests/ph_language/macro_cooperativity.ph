
process a 1
process b 1
process c 2
process z 1

GRN([
	a 1 -> + b;
	b 1 -> - a;
	c 1 -> - b
])

COOPERATIVITY([a;c] -> b 0 1, [[1;0]])


