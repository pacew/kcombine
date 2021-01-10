#! /usr/bin/env python3

import sys
import os
import math
from uuid import uuid4
import sexp
from sexp import sym, Sexp, keyeq
import copy

import re
import random


def print_exp(exp, outf=None):
    Sexp().print_exp(exp, outf)


def item_type(item):
    if isinstance(item, list) and len(item) > 0:
        return item[0]
    return None

def set_id(prop, val):
    prop.put1('id', val)
                
def set_at(prop, x, y, rotation):
    prop.put_multiple('at', [x, y, rotation])

def set_effects(prop, top_bottom):
    elts = Sexp()
    elts.put1('font', Sexp(key='size', elts=[1.27, 1.27]))
    elts.put_multiple('justify', Sexp(elts=[sym('left'), sym(top_bottom)]))
    
    prop.put_multiple('effects', elts)


def make_stroke():
    val = Sexp()
    val.put1('width', 0.001)
    val.put1('type', sym('solid'))
    val.put_multiple('color', [132, 0, 132, 1])
    return val
    

def make_fill():
    val = Sexp()
    val.put_multiple('color', [255, 255, 255, 0])
    return val


def make_path(path, page_num):
    val = Sexp('path')
    val.append(path)
    val.put1('page', str(page_num) if page_num else '')
    return val


def read_sch(filename):
    with sexp.PeekStream(filename) as inf:
        sch = Sch().read_exp(inf)
        sch.local_name = filename
        return sch

def read_pcb(filename):
    with sexp.PeekStream(filename) as inf:
        pcb = Pcb().read_exp(inf)
        return pcb

class Pcb(Sexp):
    def __init__(self):
        pass
    
    def setup(self):
        left = math.inf
        right = -math.inf
        top = math.inf
        bottom = -math.inf

        self.nets = []

        for item in self:
            if keyeq(item, 'gr_line') and item.get1('layer') == 'Edge.Cuts':
                x0, y0 = item.get_multiple('start')
                x1, y1 = item.get_multiple('end')
                left = min(left, x0, x1)
                right = max(right,x0, x1)
                top = min(top, y0, y1)
                bottom = max(bottom, y0, y1 )

            if keyeq(item, 'net'):
                net_id = item.list[1]
                net_name = item.list[2]

                if net_id != len(self.nets):
                    raise ValueError('unexpected net_id', net_id, len(self.nets))
                self.nets.append(net_name)

        self.left = left
        self.top = top
        self.width = right - left
        self.height = bottom - top


def make_empty():
    ret = Sch()
    ret.list.append(sym('kicad_sch'))
    ret.put1('version', 20201015)
    ret.put1('paper', "A4")
    return ret

class Sheet:
    def __init__(self):
        self.sch = None
        self.pcb = None
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



# no sub sheets on top level
def is_simple(sch):
    if sch.get1('version') != 20201015:
        print('unexpected version')
        return False

    safe_clauses = {
        sym('version'), 
        sym('generator'), 
        sym('paper'), 
        sym('lib_symbols'),
        sym('sheet_instances'),
        sym('symbol_instances'),
        sym('junction'),
        sym('wire'),
        sym('hierarchical_label'),
        sym('symbol'),
        sym('no_connect'),
    }

    reject_clauses = {
    }

    for item in sch:
        if item == sym('kicad_sch'):
            continue

        if keyeq(item, 'sheet'):
            return False

        key = sexp.car(item)
        if key in safe_clauses:
            continue

        if key in reject_clauses:
            return False

        raise ValueError('unknown clause', str(item))

    return True


# top level has just a sheet
def is_just_sheet(sch):
    if sch.get1('version') != 20201015:
        print('unexpected version')
        return False

    safe_clauses = {
        sym('version'), 
        sym('generator'), 
        sym('paper'), 
        sym('lib_symbols'),
        sym('sheet_instances'),
        sym('symbol_instances'),
    }

    reject_clauses = {
        sym('no_connect'),
        sym('wire'),
    }

    sheet_count = 0
    for item in sch:
        if item == sym('kicad_sch'):
            continue

        if keyeq(item, 'sheet'):
            sheet_count += 1
            continue

        key = sexp.car(item)
        if key in safe_clauses:
            continue

        if key in reject_clauses:
            return False

        raise ValueError('unknown clause', str(item))

    if sheet_count == 1:
        return True
    
    return False

def round_to_50mil(x):
    return math.floor(x / 25.4 * 200) / 200 * 25.4

def make_sheet_item_prototype(sheet, inst_name):
    posx = round_to_50mil(random.uniform(160, 250))
    posy = round_to_50mil(random.uniform(20, 150))
    width = 0.7 * 25.4
    height = 0.5 * 25.4

    item = Sexp('sheet')
    item.put_multiple('at', [posx, posy])
    item.put_multiple('size', [width, height])
    item.put_multiple('stroke', make_stroke())
    item.put_multiple('fill', make_fill())

    prop = item.set_prop('Sheet name', inst_name)
    set_id(prop, 0)
    set_at(prop, posx, posy - 0.2, 0)
    set_effects(prop, 'bottom')

    prop = item.set_prop('Sheet file', sheet.local_name)
    set_id(prop, 1)
    set_at(prop, posx, posy + height + 0.2, 0)
    set_effects(prop, 'top')

    return item

def move(exp, dx, dy):
    if isinstance(exp, Sexp):
        if keyeq(exp, 'at') and len(exp.list) >= 3:
            exp.list[1] += dx
            exp.list[2] += dy

        for subexp in exp:
            move(subexp, dx, dy)


class Sch(Sexp):
    def __init__(self):
        super().__init__()
        self.sheets = {}
        self.local_name = '(unnamed)'
        self.symbol_ref_to_path = {}

    def __str__(self):
        return f'<Sch {self.local_name} {id(self) & 0xffff:x}>'

    def find_sheet_item(self, sheet, inst_name):
        for item in self:
            if (keyeq(item, 'sheet')
                and item.get_prop('Sheet file') == sheet.local_name
                and item.get_prop('Sheet name') == inst_name):
                return item
        return None


    def get_sheet(self, sheet_spec):
        sheet = Sheet()

        sch_file = sheet_spec.get1('sch_file')
        sheet.sch = read_sch(sch_file)

        pcb_filename = os.path.splitext(sch_file)[0] + '.kicad_pcb'
        if os.path.isfile(pcb_filename):
            sheet.pcb = read_pcb(pcb_filename)

        sheet.local_name = sheet_spec.get1('local_name')
        if sheet.local_name is None:
            sheet.local_name = os.path.basename(sch_file)

        if is_simple(sheet.sch):
            sheet.item_prototype = make_sheet_item_prototype(sheet, 'dummy')
            src_path = sch_file
        elif is_just_sheet(sheet.sch):
            sheet.item_prototype = sheet.sch.assoc('sheet')
            src_path = os.path.join(os.path.dirname(sch_file), 
                                    sheet.item_prototype.get_prop('Sheet file'))
            sheet.sch = read_sch(src_path)
        else:
            print((f'can\'t handle top level {sch_file}\n'
                   f'must have no sheets, or just 1 sheet all by itself'))
            sys.exit(1)

        with open(sheet.local_name, 'w') as outf:
            with open(src_path) as inf:
                outf.write(inf.read())

        self.sheets[sheet.local_name] = sheet
        return sheet


    def add_sheet(self, sheet_spec, inst_name):
        sheet = self.get_sheet(sheet_spec)

        local = self.find_sheet_item(sheet, inst_name)
        if local is None:
            local = make_sheet_item_prototype(sheet, inst_name)
            local.put1('uuid', sym(str(uuid4())))
            self.list.append(local)

        new = copy.copy(sheet.item_prototype)
        new.put1('uuid', local.get1('uuid'))
        
        (old_x, old_y) = local.get_multiple('at')
        (new_x, new_y) = new.get_multiple('at')
        dx = old_x - new_x
        dy = old_y - new_y

        move(new, dx, dy)

        new.set_prop('Sheet file', sheet.local_name)
        new.set_prop('Sheet name', inst_name)

        local.list = new.list


    def generate_sheet_instances(self):
        for _, sheet in self.sheets.items():
            sheet.insts = []
            sheet.page_num = None

        sheets_used = list()
        page_num = 1
        for item in self.list:
            if keyeq(item, 'sheet'):
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

        self.put_multiple('sheet_instances', si)


    def fixup_symbol_instances(self):
        sheet_uuids_used = set()
        for _, sheet in self.sheets.items():
            for sheet_inst in sheet.insts:
                sheet_inst_uuid = sheet_inst.get1('uuid')
                sheet_uuids_used.add(sheet_inst_uuid)

        symbol_instances = []

        for inst in self.get_multiple('symbol_instances'):
            if keyeq(inst, 'path'):
                parts = inst.cadr().split('/')
                if len(parts) <= 2:
                    symbol_instances.append(inst)

        for _, sheet in self.sheets.items():
            for sheet_inst in sheet.insts:
                sheet_inst_uuid = sheet_inst.get1('uuid')
                sheet_inst_name = sheet_inst.get_prop('Sheet name')
                for item in sheet.sch:
                    if keyeq(item, 'symbol'):
                        uuid = item.get1('uuid')
                        unit = item.get1('unit')
                        raw_ref = item.get_prop('Reference')
                        value = item.get_prop('Value')
                        footprint = item.get_prop('Footprint')
                        if unit is None:
                            unit = 1

                        path = f'/{sheet_inst_uuid}/{uuid}'

                        sub_ref = re.sub('^[^:]*:', '', raw_ref)
                        ref = f'{sheet_inst_name}:{sub_ref}'

                        self.symbol_ref_to_path[ref] = path

                        syminst = Sexp('path')
                        syminst.append(path)
                        syminst.put1('reference', ref)
                        syminst.put1('unit', unit)
                        syminst.put1('value', value)
                        syminst.put1('footprint', footprint)
                        symbol_instances.append(syminst)

        symbol_instances.sort(key=lambda item: item.get1('reference'))
        si = Sexp(elts=symbol_instances)
        self.put_multiple('symbol_instances', si)
        
    def filter_sheets(self):
        new = Sexp()
        for item in self:
            if keyeq(item, 'sheet'):
                local_name = item.get_prop('Sheet file')
                if local_name in self.sheets:
                    new.append(item)
            else:
                new.append(item)
        self.list = new


def generate_pcb(outname, sch):
    out = read_pcb('empty.kicad_pcb')

    for _, sheet in sch.sheets.items():
        sheet.pcb.setup()

    net_offset = 0
    for _, sheet in sch.sheets.items():
        pcb = sheet.pcb
        for sheet_inst in sheet.insts:
            nnets = len(pcb.nets)
            sheet_inst.net_offset = net_offset
            net_offset += nnets

            sheet_inst.new_nets = []
            for idx in range(nnets):
                old_name = pcb.nets[idx]
                new_name = old_name
                if re.match('^Net-', old_name):
                    inst_name = sheet_inst.get_prop('Sheet name')
                    parts = re.match('^Net-.(.*).$', old_name)
                    basename = parts.group(1)
                    new_name = f'{inst_name}:{basename}'
                elif re.match('.*/', old_name):
                    inst_name = sheet_inst.get_prop('Sheet name')
                    basename = re.sub('.*/', '', old_name)
                    new_name = f'/{inst_name}/{basename}'
                sheet_inst.new_nets.append(new_name)

    for _, sheet in sch.sheets.items():
        pcb = sheet.pcb
        for sheet_inst in sheet.insts:
            for idx in range(len(pcb.nets)):
                net_num = idx + sheet_inst.net_offset
                net_name = sheet_inst.new_nets[idx]
                item = Sexp('net', elts=[net_num, net_name])
                out.append(item)

    stage_y = 0

    for _, sheet in sch.sheets.items():
        pcb = sheet.pcb
        for sheet_inst in sheet.insts:
            dest_x = -pcb.width - 10
            dest_y = stage_y

            dx = dest_x - pcb.left
            dy = dest_y - pcb.top

            for old_item in pcb:
                if keyeq(old_item, 'gr_line'):
                    item = copy.copy(old_item)
                    if item.get1('layer') == 'Edge.Cuts':
                        item.put1('layer', 'F.Fab')
                    item = move_start_end(item, dx, dy)
                    out.append(item)

                elif keyeq(old_item, 'footprint'):
                    new = []
                    for elt in old_item:
                        if keyeq(elt, 'at'):
                            elt = Sexp('at', elts=[elt.list[1] + dx, elt.list[2] + dy])
                        new.append(elt)
                    item = Sexp(elts=new)
                    item = fix_net(item, sheet, sheet_inst)
                    item = fix_footprint(item, sheet, sheet_inst, sch)
                    out.append(item)

                elif keyeq(old_item, 'segment'):
                    item = copy.copy(old_item)
                    item = move_start_end(item, dx, dy)
                    item.list = fix_net(item, sheet, sheet_inst)
                    out.append(item)

            stage_y += pcb.height + 10
                        

    with open(outname, 'w') as outf:
        out.write(outf)

# item is a footprint.  change:
# (property "Sheet file" "led-sheet.kicad_sch")
# (property "Sheet name" "led")
# (path "/f6f98991-c174-4124-bca3-675ab210d740/a4b061e3-44cf-4957-b795-e75f05b2acf4")
# (attr smd)
# (fp_text reference "Q1" ...

def get_footprint_ref(item):
    reference_sym = sym('reference')
    for elt in item:
        if keyeq(elt, 'fp_text') and elt.list[1] == reference_sym:
            return elt.list[2]
    return None

def fix_footprint(item, sheet, sheet_inst, sch):

    old_ref = get_footprint_ref(item)
    if old_ref is None:
        print('footpint is missing ref', item)
        sys.exit(1)

    sheet_inst_name = sheet_inst.get_prop('Sheet name')

    sub_ref = re.sub('^[^:]*:', '', old_ref)
    new_ref = f'{sheet_inst_name}:{sub_ref}'
            
    new_path = sch.symbol_ref_to_path[new_ref]

    reference_sym = sym('reference')

    ret = []
    for elt in item:
        if keyeq(elt, 'property') and elt.list[1] == 'Sheet file':
            elt = copy.copy(elt)
            elt.list[2] = sheet_inst.get_prop('Sheet file')
        elif keyeq(elt, 'property') and elt.list[1] == 'Sheet name':
            elt = copy.copy(elt)
            elt.list[2] = sheet_inst_name
        elif keyeq(elt, 'path'):
            elt = copy.copy(elt)
            elt.list[1] = new_path
        elif keyeq(elt, 'fp_text') and elt.list[1] == reference_sym:
            elt = copy.copy(elt)
            elt.list[2] = new_ref
            
        ret.append(elt)
    return Sexp(elts=ret)
        


def fix_net(item, sheet, sheet_inst):
    ret = []
    for elt in item:
        if keyeq(elt, 'net'):
            elt = copy.copy(elt)
            old_idx = elt.list[1]
            elt.list[1] = old_idx + sheet_inst.net_offset
            if len(elt.list) > 2:
                elt.list[2] = sheet_inst.new_nets[old_idx]

        if isinstance(elt, Sexp):
            elt = fix_net(elt, sheet, sheet_inst)

        ret.append(elt)
    return Sexp(elts=ret)

def move_start_end(item, dx, dy):
    ret = []
    for elt in item:
        if keyeq(elt, 'start') or keyeq(elt, 'end'):
            elt = Sexp(elt.list[0], elts=[elt.list[1] + dx, elt.list[2] + dy])
        if isinstance(elt, Sexp):
            elt = move_start_end(elt, dx, dy)

        ret.append(elt)

    return Sexp(elts=ret)
