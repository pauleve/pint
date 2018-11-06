
import os
import sys
from argparse import ArgumentParser

from .model import load, FileModel

from .cfg import CFG, remove_output_files

def import_model():
    CFG["console"] = True
    parser = ArgumentParser(prog=sys.argv[0])
    parser.add_argument("input")
    parser.add_argument("--output", "-o", type=str, default=None)

    sbgnpd_opts = parser.add_argument_group("SBGN-PD import")
    sbgnpd_opts.add_argument("--fancy-names", action="store_true", default=False,
                help="Use EPN labels as automata names")

    args = parser.parse_args()
    opts = vars(args).copy()
    del opts["input"]
    del opts["output"]

    an = load(args.input, **opts)

    if args.output:
        an.save_as(args.output)
    else:
        print(an.source())

    remove_output_files()



