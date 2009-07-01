

def v(l) :
	m = l[-1]/len(l)
	print "m = %s" % m
	p = 0
	s = 0
	for x in l :
		s += (x-p-m)**2
		p = x
	v = s/len(l)
	v2 = s/(len(l)-1)
	print "v = %s (%s)" % (v2,v)

