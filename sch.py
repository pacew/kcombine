#! /usr/bin/env python3

import sys
from uuid import uuid4
import sexp
from sexp import sym

import re
import random


def print_exp(exp, outf=None):
    sexp.Sexp().print_exp(exp, outf)


def item_type(item):
    if isinstance(item, list) and len(item) > 0:
        return item[0]
    return None


def find_prop(item, pname):
    property = sym('property')
    for clause in item:
        if isinstance(clause, list) and len(clause) >= 3:
            if clause[0] == property and clause[1] == pname:
                return clause
    return None


def get_prop(item, pname):
    prop = find_prop(item, pname)
    if prop is None:
        return None
    return prop[2]


def set_prop(item, pname, pval):
    prop = find_prop(item, pname)
    if prop is None:
        prop = [sym('property'), pname, pval]
        item.append(prop)
    else:
        prop[2] = pval
    return prop


def assoc(key, alist, create=False):
    for item in alist:
        if isinstance(item, list) and len(item) > 0 and item[0] == key:
            return item
    if create:
        item = [key]
        alist.append(item)
        return item
    return None


def assoc_get(key, alist):
    item = assoc(key, alist)
    if isinstance(item, list) and len(item) >= 2:
        return item[1]
    return None


def assoc_set(key, alist, val):
    item = assoc(key, alist)
    if item is None:
        alist.append([key, val])
    else:
        del item[1:]
        item.append(val)


def assoc_set_multiple(key, alist, val):
    item = assoc(key, alist)
    if item is None:
        item = [key]
        alist.append(item)
    del item[1:]
    item.extend(val)


def set_id(alist, val):
    assoc_set(sym('id'), alist, val)
                

def set_at(alist, x, y, rotation):
    assoc_set_multiple(sym('at'), alist, [x, y, rotation])

                
def set_effects(alist, top_bottom):
    val = [[sym('font'), [sym('size'), 1.27, 1.27]],
           [sym('justify'), sym('left'), sym(top_bottom)]]
    assoc_set_multiple(sym('effects'), alist, val)


def set_stroke(alist):
    val = [[sym('width'), 0.001],
           [sym('type'), sym('solid')],
           [sym('color'), 132, 0, 132, 1]]
    assoc_set_multiple(sym('stroke'), alist, val)
  

def set_fill(alist):
    val = [sym('color'), 255, 255, 255, 0]
    assoc_set(sym('fill'), alist, val)


def make_path(path, pagenum):
    return [sym('path'), path, [sym('page'), str(pagenum) if pagenum else '']]


class Sch:
    def readfile(self, filename):
        self.filename = filename
        self.sch = sexp.Sexp().readfile(filename)
        return self

    def print_sch(self, outf=None):
        print_exp(self.sch, outf)

    def make_empty(self):
        self.sch = [sym('kicad_sch'),
                    [sym('version'), 20201015],
                    [sym('paper'), "A4"]]
        return self

    def find_sheet(self, sheet, inst_name):
        sheet_sym = sym('sheet')
        for item in self.sch:
            if (item_type(item) == sheet_sym
                and get_prop(item, 'Sheet name') == inst_name
                and get_prop(item, 'Sheet file') == sheet.filename):
                return item
        return None

    def add_sheet(self, sheet, inst_name):
        if self.find_sheet(sheet, inst_name) is None:
            posx = random.uniform(160, 250)
            posy = random.uniform(20, 150)
            width = 20
            height = 15

            item = list()
            item.append(sym('sheet'))
            item.append([sym('at'), posx, posy])
            item.append([sym('size'), width, height])
            set_stroke(item)
            set_fill(item)

            self.uuid = sym(str(uuid4()))
            item.append([sym('uuid'), self.uuid])

            line_height = 1.6

            prop = set_prop(item, 'Sheet name', inst_name)
            set_id(prop, 0)
            set_at(prop, posx, posy - 0.2, 0)
            set_effects(prop, 'bottom')
        
            prop = set_prop(item, 'Sheet file', sheet.filename)
            set_id(prop, 1)
            set_at(prop, posx, posy + height + 0.2, 0)
            set_effects(prop, 'top')

            self.sch.append(item)

    def fixup_sheet_instances(self):
        for _, sheet in Sheet.sheets.items():
            sheet.sheet_insts = []
            sheet.pagenum = None

        sheet_sym = sym('sheet')
        uuid_sym = sym('uuid')
        sheets_used = list()
        pagenum = 2
        for item in self.sch:
            if item_type(item) == sheet_sym:
                uuid = assoc_get(uuid_sym, item)
                inst_name = get_prop(item, 'Sheet name')
                filename = get_prop(item, 'Sheet file')

                sheet = Sheet.lookup_by_file(filename)
                if sheet:
                    if sheet.pagenum is None:
                        sheet.pagenum = pagenum
                        pagenum += 1
                    SheetInst(sheet, inst_name, uuid)

        sheet_instances_sym = sym('sheet_instances')
        si = [sheet_instances_sym, make_path('/', 1)]

        for _, sheet in Sheet.sheets.items():
            pagenum = sheet.pagenum
            for sheet_inst in sheet.sheet_insts:
                path = f'/{sheet_inst.uuid.name}/'
                si.append(make_path(path, pagenum))
                pagenum = None

        elt = assoc(sheet_instances_sym, self.sch)
        if elt:
            elt.clear()
            elt.append(si)
        else:
            self.sch.append(si)

    def fixup_symbol_instances(self):
        symbol_instances = []

        symbol_sym = sym('symbol')
        for _, sheet in Sheet.sheets.items():
            for sheet_inst in sheet.sheet_insts:
                for item in sheet.sch.sch:
                    if item_type(item) == symbol_sym:
                        uuid = assoc_get(sym('uuid'), item)
                        ref = get_prop(item, 'Reference')
                        unit = get_prop(item, 'unit')
                        value = get_prop(item, 'Value')
                        footprint = get_prop(item, 'Footprint')
                        if unit is None:
                            unit = 1

                        path = f'/{sheet_inst.uuid.name}/{uuid}'

                        core_ref = re.sub('^[^:]*:', '', ref)
                        ref = f'{sheet_inst.inst_name}:{core_ref}'

                        syminst = [sym('path'), path,
                                   [sym('reference'), ref],
                                   [sym('unit'), unit],
                                   [sym('value'), value],
                                   [sym('footprint'), footprint]]
                        symbol_instances.append(syminst)

        symbol_instances.sort(key=lambda item: assoc_get(sym('reference'), item))
        assoc_set_multiple(sym('symbol_instances'), self.sch, 
                           symbol_instances)
        
class SheetInst:
    def __init__(self, sheet, inst_name, uuid):
        self.sheet = sheet
        self.inst_name = inst_name
        self.uuid = uuid

        self.sheet.add_sheet_inst(self)

    def __repr__(self):
        return (f'<sheet-inst {self.sheet.filename}'
                f' {self.inst_name} {self.uuid}>')

class Sheet:
    sheets = dict()

    def add_sheet_inst(self, sheet_inst):
        self.sheet_insts.append(sheet_inst)

    @classmethod
    def lookup_by_file(cls, filename):
        return cls.sheets.get(filename)

    @classmethod
    def from_file(cls, filename):
        if filename in cls.sheets:
            return cls.sheets[filename]

        sch = Sch().readfile(filename)

        sheet = cls()
        sheet.filename = filename
        sheet.sch = sch
        sheet.sheet_insts = list()

        cls.sheets[filename] = sheet
        return sheet


def main():
    top = Sch().make_empty()

    if False:
        led = Sheet.from_file('led-sheet.kicad_sch')
        top.add_sheet(led, "led1")
        top.add_sheet(led, "led2")
    else:
        cap = Sheet.from_file('cap.kicad_sch')
        top.add_sheet(cap, 'cap1')

    top.fixup_sheet_instances()
    top.fixup_symbol_instances()

    top.print_sch()

if __name__ == '__main__':
    main()
