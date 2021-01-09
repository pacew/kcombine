#! /usr/bin/env python3

import sys

import argparse
import sch
import sexp
from sexp import sym


def kcombine(config):
    top = sch.make_empty()
    print(top)



    output_filename = config.assoc_get('output')
    print(output_filename)
    try:
        top = sexp.read_sexp(output_filename)
    except FileNotFoundError:
        top = sch.make_empty()
    print(top)
    

    sheet_sym = sym('sheet')
    for item in config:
        if sexp.keyeq(item, 'sheet'):
            pass

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('config')
    args = parser.parse_args()

    config = sexp.read_sexp(args.config)
    print(config)
    kcombine(config)



