#! /usr/bin/env python3

import os
import sys

import argparse
import sch
import sexp

def kcombine(config):
    output_filename = config.get1('output')
    try:
        top = sch.read_sch(output_filename)
    except FileNotFoundError:
        top = sch.make_empty()

    for sheet_spec in config:
        if sexp.keyeq(sheet_spec, 'sheet'):
            insts = sheet_spec.get_multiple('inst')
            for inst in insts:
                top.add_sheet(sheet_spec, inst)

    top.generate_sheet_instances()
    top.fixup_symbol_instances()
    top.filter_sheets()

    with open(output_filename, 'w') as outf:
        top.write(outf)

    pcb_filename = os.path.splitext(output_filename)[0] + '.kicad_pcb'
    sch.generate_pcb(pcb_filename, top)


def test_sexp():
    with sexp.PeekStream("foo") as inf:
        print(sexp.Sexp().read_exp(inf))
    sys.exit(0)
      

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('config')
    args = parser.parse_args()

    config = sexp.read_sexp(args.config)
    kcombine(config)



