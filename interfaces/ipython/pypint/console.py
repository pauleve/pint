
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
    args = parser.parse_args()

    an = load(args.input)

    if args.output:
        an.save_as(args.output)
    else:
        print(an.source())

    remove_output_files()



