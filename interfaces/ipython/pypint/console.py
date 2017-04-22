
import sys
from argparse import ArgumentParser

from .model import load

def import_model():
    parser = ArgumentParser(prog=sys.argv[0])
    parser.add_argument("input")
    parser.add_argument("--output", "-o", type=str, default=None)
    args = parser.parse_args()

    an = load(args.input)
    if args.output:
        an.save_as(args.output)
    else:
        print(an.source())



