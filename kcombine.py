#! /usr/bin/env python3

import sys

import argparse
import sch
import sexp
from sexp import sym


def kcombine(config):
    output_filename = config.get1('output')
    # try:
    #     top = sch.read_sch(output_filename)
    # except FileNotFoundError:
    #     top = sch.make_empty()
    # print(top)
    top = sch.make_empty()

    sheet_sym = sym('sheet')
    for sheet_spec in config:
        if sexp.keyeq(sheet_spec, 'sheet'):
            insts = sheet_spec.get_multiple('inst')
            for inst in insts:
                top.add_sheet(sheet_spec, inst)

    top.generate_sheet_instances()

    with open(output_filename, 'w') as outf:
        top.write(outf)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('config')
    args = parser.parse_args()

    config = sexp.read_sexp(args.config)
    print(config)
    kcombine(config)



