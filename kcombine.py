#! /usr/bin/env python3

import sys

import argparse
import sch
import sexp
from sexp import sym


def kcombine(config):
    output = config.assoc_get(sym('output'))
    print(output)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('config')
    args = parser.parse_args()

    config = sexp.read_sexp(args.config)
    sexp.print_sexp(config)
    kcombine(config)



