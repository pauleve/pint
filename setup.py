#!/usr/bin/env python

import os
import shutil
import sys

from optparse import OptionParser

src_basedir = os.path.abspath(os.path.dirname(__file__))

cfg = {
	"version": "2015-10-19",##VERSION##
	"pint_share_path": src_basedir,
}



p = OptionParser()
p.add_option("--share-path", type=str,
					help="Directory containing Pint share files (default: %s)" \
						% cfg["pint_share_path"],
					default=cfg["pint_share_path"],
					dest='share_path')
p.add_option("--enable-R", action="store_true", dest="enable_R", default=True)
p.add_option("--disable-R", action="store_false", dest="enable_R")
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


if args.enable_R:
    print("enabling R bindings")
    shutil.copyfile("bindings/Makefile.inc.R", "bindings/Makefile.inc")
    for f in ["bindings/r.ml", "bindings/r.mli"]:
        os.path.exists(f) and os.unlink(f)
else:
    print("disabling R bindings")
    shutil.copyfile("bindings/Makefile.inc.noR", "bindings/Makefile.inc")
    shutil.copyfile("bindings/r_empty.ml", "bindings/r.ml")
    shutil.copyfile("bindings/r_empty.mli", "bindings/r.mli")


