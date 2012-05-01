process a 2
process b 1
process c 1

a 2 -> b 1 0

process bc 3

b 0 -> bc 3 1
b 0 -> bc 2 0
b 1 -> bc 1 3
b 1 -> bc 0 2
c 0 -> bc 3 2
c 0 -> bc 1 0
c 1 -> bc 2 3
c 1 -> bc 0 1

b 0 -> a 2 1
b 1 -> a 0 1
c 0 -> a 2 1
c 1 -> a 0 1
bc 3 -> a 1 2
bc 0 -> a 1 0

