#! /usr/bin/env python3

import sys
from uuid import uuid4
import sexp
from sexp import sym


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


def assoc(key, alist):
    if isinstance(alist, list):
        for item in alist:
            if isinstance(item, list) and len(item) > 0 and item[0] == key:
                return item
    return None


class Sch:
    def readfile(self, filename):
        self.filename = filename
        self.sch = sexp.Sexp().readfile(filename)
        return self

    def print_sch(self, outf=None):
        print_exp(self.sch, outf)

    def make_empty(self):
        self.sch = [sym('kicad_sch'),
                    [sym('version'), 20200310],
                    [sym('page'), "A4"]]
        return self

    def find_sheet(self, sheet, inst_name):
        sheet_sym = sym('sheet')
        for item in self.sch:
            if item_type(item) == sheet_sym:
                if (
                        get_prop(item, 'Sheet name') == inst_name
                        and get_prop(item, 'Sheet file') == sheet.filename):
                    return item
        return None

    def add_sheet(self, sheet, inst_name):
        if self.find_sheet(sheet, inst_name) is None:
            item = list()
            item.append(sym('sheet'))
            item.append([sym('at'), 50, 50])
            item.append([sym('size'), 20, 20])
            self.uuid = sym(str(uuid4()))
            item.append([sym('uuid'), self.uuid])
            set_prop(item, 'Sheet name', inst_name)
            set_prop(item, 'Sheet file', sheet.filename)

            self.sch.append(item)

    def fixup_sheet_instances(self):
        sheet_sym = sym('sheet')
        uuid_sym = sym('uuid')
        sheets_used = list()
        for item in self.sch:
            if item_type(item) == sheet_sym:
                val = assoc(uuid_sym, item)
                inst_name = get_prop(item, 'Sheet name')
                filename = get_prop(item, 'Sheet file')
                uuid = val[1]
                sheets_used.append([inst_name, filename, uuid])
        print(sheets_used)


class Sheet:
    sheets = dict()

    @classmethod
    def from_file(cls, filename):
        if filename in cls.sheets:
            return cls.sheets[filename]

        sch = Sch().readfile(filename)

        sheet = cls()
        sheet.filename = filename
        sheet.sch = sch

        cls.sheets[filename] = sheet
        return sheet


led = Sheet.from_file('byhand/led.kicad_sch')

top = Sch().make_empty()
top.add_sheet(led, "led1")
top.add_sheet(led, "led2")
top.fixup_sheet_instances()

top.print_sch(sys.stdout)
