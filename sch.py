#! /usr/bin/env python3

import sys
import os
from uuid import uuid4
import sexp
from sexp import sym, Sexp, keyeq

import re
import random


def print_exp(exp, outf=None):
    Sexp().print_exp(exp, outf)


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


def set_id(prop, val):
    prop.set1('id', val)
                
def set_at(prop, x, y, rotation):
    prop.set_multiple('at', [x, y, rotation])

# (effects
#   (font (size 1.27 1.27))
#   (justify left bottom)
# )
def set_effects(prop, top_bottom):
    elts = Sexp()
    elts.set1('font', Sexp(key='size', elts=[1.27, 1.27]))
    elts.set_multiple('justify', Sexp(elts=[sym('left'), sym(top_bottom)]))
    
    prop.set_multiple('effects', elts)


def make_stroke():
    val = Sexp()
    val.set1('width', 0.001)
    val.set1('type', sym('solid'))
    val.set_multiple('color', [132, 0, 132, 1])
    return val
    

def make_fill():
    val = Sexp()
    val.set_multiple('color', [255, 255, 255, 0])
    return val


def make_path(path, page_num):
    val = Sexp('path')
    val.append(path)
    val.set1('page', str(page_num) if page_num else '')
    return val


def read_sch(filename):
    with sexp.PeekStream(filename) as inf:
        sch = Sch().read_exp(inf)
        sch.local_name = filename
        return sch

def make_empty():
    ret = Sch()
    ret.list.append(sym('kicad_sch'))
    ret.set1('version', 20201015)
    ret.set1('paper', "A4")
    return ret

# items.append(f'u:{str(self.uuid)[0:6]}')

class Sheet:
    def __init__(self):
        self.sch = None
        self.local_name = None
        self.page_num = None
        self.insts = []

    def __repr__(self):
        items = []
        if self.local_name:
            items.append(re.sub('[.].*', '', self.local_name))

        if self.page_num:
            items.append(f'p{self.page_num}')

        items.append(f'insts={len(self.insts)}')

        items.append(f'{id(self) & 0xffff:x}')

        msg = ' '.join(items)

        return f'<sheet {msg}>'


class Sch(Sexp):
    def __init__(self):
        super().__init__()
        self.sheets = {}
        self.local_name = '(unnamed)'

    def __str__(self):
        return f'<Sch {self.local_name} {id(self) & 0xffff:x}>'

    def find_sheet(self, sheet_spec):
        input_name = sheet_spec.get1('sch_file')
        local_name = sheet_spec.get1('local_name')
        if local_name is None:
            local_name = os.path.basename(input_name)

        if not os.path.isfile(local_name):
            with open(local_name, 'w') as outf:
                with open(input_name) as inf:
                    outf.write(inf.read())

        if local_name not in self.sheets:
            sheet = Sheet()
            sheet.sch = read_sch(local_name)
            sheet.local_name = local_name
            self.sheets[local_name] = sheet

        return self.sheets[local_name]

    def find_sheet_item(self, sheet_spec, inst_name):
        sheet_sym = sym('sheet')
        local_name = sheet_spec.get1('local_name')
        for item in self:
            if (item_type(item) == sheet_sym
                and get_prop(item, 'Sheet name') == inst_name
                and get_prop(item, 'Sheet file') == local_name):
                return item
        return None

    def add_sheet(self, sheet_spec, inst_name):
        sheet = self.find_sheet(sheet_spec)
        sheet_item = self.find_sheet_item(sheet_spec, inst_name)
        if sheet_item is None:
            posx = random.uniform(160, 250)
            posy = random.uniform(20, 150)
            width = 20
            height = 15

            item = Sexp('sheet')
            item.set_multiple('at', [posx, posy])
            item.set_multiple('size', [width, height])
            item.set_multiple('stroke', make_stroke())
            item.set_multiple('fill', make_fill())

            uuid = sym(str(uuid4()))
            item.set1('uuid', uuid)

            prop = item.set_prop('Sheet name', inst_name)
            set_id(prop, 0)
            set_at(prop, posx, posy - 0.2, 0)
            set_effects(prop, 'bottom')
        
            prop = item.set_prop('Sheet file', sheet.local_name)
            set_id(prop, 1)
            set_at(prop, posx, posy + height + 0.2, 0)
            set_effects(prop, 'top')

            self.list.append(item)

    def generate_sheet_instances(self):
        for _, sheet in self.sheets.items():
            sheet.insts = []
            sheet.page_num = None

        sheets_used = list()
        page_num = 1
        for item in self.list:
            if keyeq(item, 'sheet'):
                inst_name = item.get_prop('Sheet name')
                local_name = item.get_prop('Sheet file')

                sheet = self.sheets.get(local_name)
                if sheet:
                    if sheet.page_num is None:
                        page_num += 1
                        sheet.page_num = page_num
                    sheet.insts.append(item)

        si = Sexp()
        si.append(make_path('/', 1))

        for _, sheet in self.sheets.items():
            page_num = sheet.page_num
            for inst in sheet.insts:
                uuid = inst.get1('uuid')
                path = f'/{str(uuid)}/'
                si.append(make_path(path, page_num))
                page_num = None

        self.set_multiple('sheet_instances', si)


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

class Sheet_old:
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
