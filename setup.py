#!/usr/bin/env python

import os
import sys

from optparse import OptionParser

src_basedir = os.path.abspath(os.path.dirname(__file__))

cfg = {
	"version": "2013-09-23",##VERSION##
	"pint_share_path": src_basedir,
}



p = OptionParser()
p.add_option("--share-path", type=str,
					help="Directory containing Pint share files (default: %s)" \
						% cfg["pint_share_path"],
					default=cfg["pint_share_path"],
					dest='share_path')
(args, _) = p.parse_args()

if args.share_path:
	cfg["pint_share_path"] = args.share_path

print(cfg)

#
# generation
# 
if not os.path.isdir('build'):
	os.mkdir('build')

fd = open('build/distenv.ml', 'w')
for var, value in cfg.items():
	fd.write("let %s = \"%s\"\n" % (var, value))

fd = open('build/Makefile.inc', 'w')
for var, value in cfg.items() :
	fd.write("%s=%s\n" % (var.upper(), value))


