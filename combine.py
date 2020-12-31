#! /usr/bin/env python3

import sys
import os
import random
import subprocess
from uuid import uuid4
import re

import sch

from sch import Val

def find_elt(data, name):
    for elt in data:
        if type(elt) == list:
            op = elt[0]
            if type(op) == Val and op.equal(name):
                return elt
    return None



in_top = 'empty/empty.kicad_sch'
in_mod = 'flat/flat.kicad_sch'

pname = 'combined'
mname = 'mod'

subprocess.run(f'rm -rf {pname}', shell=True)
os.mkdir(pname)
subprocess.run(f'cp empty/empty.kicad_pro {pname}/{pname}.kicad_pro',
               shell=True)

top = sch.Sch().readfile(in_top)
mod = sch.Sch().readfile(in_mod)

mod_symbol_instances = find_elt(mod, 'symbol_instances')

def filter_mod(mod):
    ret = []
    for clause in mod:
        if type(clause) == list:
            op = clause[0]
            if op.equal('sheet_instances'):
                pass
            elif op.equal('symbol_instances'):
                pass
            else:
                ret.append(clause)
        else:
            ret.append(clause)
    return ret

mod = filter_mod(mod)

# millimeters

page_width = 11 * 25.4
page_height = 8.5 * 25.4

posx = random.uniform(page_width * 0.5, page_width * 0.75)
posy = random.uniform(page_height * 0.2, page_height * 0.8)

width = 60
height = 20

sheet_uuid = str(uuid4())

sheet = [ 
    'sheet',
    ['at', posx, posy],
    ['size', width, height],
    ['stroke', 
     ['width', '0.001'], 
     ['type', 'solid'], 
     ['color', 132, 0, 132, 1]],
    ['fill', ['color', 255, 255, 255, 0]],
    ['uuid', sheet_uuid],
    ['property', '"Sheet name"', f'"{mname}"', 
     ['id', 0 ],
     ['at', posx, posy - 1, 0],
     ['effects',
      ['font', ['size', 1.27, 1.27]], 
      ['justify', 'left', 'bottom']]],
    ['property', '"Sheet file"', f'"{mname}.kicad_sch"',
     ['id', 1],
     ['at', posx, posy + height + 1, 0],
     ['effects',
      ['font', ['size', 1.27, 1.27]], 
      ['justify', 'left', 'top']]],
]


top.append(sheet)

def dquote(s):
    return '"' + str(s) + '"'

sheet_instances = find_elt(top, 'sheet_instances')
if sheet_instances is None:
    sheet_instances = ['sheet_instances']
    top.append(sheet_instances)

uuids_seen = set()
max_page = 0
for s in sheet_instances[1:]:
    if type(s) != list:
        raise ValueError('expected list')
    if not s[0].equal('path'):
        raise ValueError('expected path')
    s_uuid = re.sub('["/]', '', str(s[1]))
    s_page = int(re.sub('"', '', str(s[2][1])))

    uuids_seen.add(s_uuid)
    max_page = max(max_page, s_page)

if '' not in uuids_seen:
    max_page += 1
    sheet_instances.append(['path', dquote('/'), ['page', dquote(max_page)]])

max_page += 1
sheet_page = max_page
sheet_instances.append(['path', f'"/{sheet_uuid}/"', 
                        ['page', dquote(sheet_page)]])


top_symbol_instances = find_elt(top, 'symbol_instances')
if top_symbol_instances is None:
    top_symbol_instances = ['symbol_instances']
    top.append(top_symbol_instances)
    
for sym in mod_symbol_instances[1:]:
    uuid = re.sub('"', '', str(sym[1]))
    newpath = f'/{sheet_uuid}{uuid}'
    sym[1] = dquote(newpath)
    top_symbol_instances.append(sym)



with open('combined/combined.kicad_sch', 'w') as outf:
    sch.write_sexp(outf, top)

with open('combined/mod.kicad_sch', 'w') as outf:
    sch.write_sexp(outf, mod)



