#!/usr/bin/env python

import os
import shutil
import stat
import sys

from optparse import OptionParser

src_basedir = os.path.abspath(os.path.dirname(__file__))

cfg = {
    "pint_share_path": src_basedir,
}

p = OptionParser()
p.add_option("--share-path", type=str,
        help="Directory containing Pint share files (default: %s)" \
                                            % cfg["pint_share_path"],
        default=cfg["pint_share_path"], dest='share_path')
p.add_option("--pint-env", action="store_true")
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
if args.pint_env:

    pintenv_file = "bin/pint-env"
    with open(pintenv_file, "w") as envf:
        envf.write("#!/bin/sh\n")
    st = os.stat(pintenv_file)
    os.chmod(pintenv_file, st.st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)

    fd.write("""
let pintenv =
    let fold_env env line =
        let line = String.trim line
        in
        if String.get line 0 = '#' then env else
        match String.split_on_char '=' line with
          var::value::[] ->
            let var = String.trim var
            and value = String.trim value
            in
            (var,value)::env
        | _ -> env
    in
    let rec read_lines pin =
        try
            let line = input_line pin
            in
            line::read_lines pin
        with End_of_file -> []
    in
    let pin = Unix.open_process_in "pint-env"
    in
    let env = List.fold_left fold_env [] (read_lines pin)
    in
    let _ = Unix.close_process_in pin
    in
    env
""")

for var, value in cfg.items():
    if args.pint_env:
        fd.write("""
let {0} = try Sys.getenv "{1}"
    with Not_found -> try List.assoc "{1}" pintenv
    with Not_found -> "{2}"
""".format(var, var.upper(), value))
    else:
        fd.write("""
let {0} = try Sys.getenv "{1}"
    with Not_found -> "{2}"
""".format(var, var.upper(), value))
fd.close()

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


