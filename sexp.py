#! /usr/bin/env python3

import sys


class Sym:
    table = dict()

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'|{self.name}|'

    @classmethod
    def lookup(cls, name):
        if name in cls.table:
            return cls.table[name]
        val = Sym(name)
        cls.table[name] = val
        return val


def sym(name):
    return Sym.lookup(name)

def keyeq(item, name):
    return isinstance(item, Sexp) and item.keyeq(name)


class PeekStream:
    def __init__(self, filename):
        self.filename = filename
        self.unget_char = None
        
    def __enter__(self):
        self.file = open(self.filename)
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.file.close()
        self.file = None

    def peek(self):
        if not self.unget_char:
            self.unget_char = self.file.read(1)
        return self.unget_char

    def getc(self):
        c = self.peek()
        self.unget_char = None
        return c

    def unget(self, c):
        self.unget_char = c

def read_sexp(filename):
    with PeekStream(filename) as inf:
        return Sexp().read_exp(inf)


def str_sexp(elt):
    if isinstance(elt, str):
        ret = '"'
        for c in elt:
            if c == '\r':
                ret += '\\r'
            elif c == '\n':
                ret += '\\n'
            elif c == '\t':
                ret += '\\t'
            elif c == '\\':
                ret += '\\\\'
            else:
                ret += c
        ret += '"'
        return ret
    elif isinstance(elt, Sexp):
        ret = '('
        need_space = False
        for subelt in elt.list:
            if need_space:
                ret += ' '
            need_space = True
            
            ret += str_sexp(subelt)
        ret += ')\n'
        return ret
    elif isinstance(elt, Sym):
        return elt.name
    elif isinstance(elt, int):
        return str(elt)
    elif isinstance(elt, float):
        return f'{elt:.4f}'
    else:
        return '<OOPS>'

def print_sexp(elt, outf=None):
    if outf is None:
        outf = sys.stdout
    outf.write(str_sexp(elt))

class Sexp:
    def __init__(self, key=None, elts=None):
        self.list = []
        if key:
            self.list.append(sym(key))
        if elts:
            self.list.extend(elts)

    def append(self, val):
        self.list.append(val)
        
    def __iter__(self):
        for item in self.list:
            yield item

    def read_exp(self, inf):
        while True:
            c = inf.getc()
            if c == ' ' or c == '\t' or c == '\r' or c == '\n':
                pass
            elif c is None or c == '':
                return None
            elif c == ')':
                inf.unget(c)
                return None
            elif c == '(':
                return self.read_list(inf)
            elif c == '"':
                return self.read_string(inf)
            elif c == '-' or c == '.' or ('0' <= c <= '9'):
                inf.unget(c)
                return self.read_possible_number(inf)
            else:
                inf.unget(c)
                return self.read_symbol(inf)

    def read_list(self, inf):
        self.list = []
        while True:
            elt = Sexp().read_exp(inf)
            if elt is None:
                break
            self.list.append(elt)
        inf.getc()  # discard close paren
        return self

    def read_symbol(self, inf):
        return sym(self.read_chars(inf, False))

    def read_string(self, inf):
        # initial quote has already been consumed
        return self.read_chars(inf, True)

    def read_possible_number(self, inf):
        s = self.read_chars(inf, False)
        try:
            return int(s)
        except ValueError:
            try:
                return float(s)
            except ValueError:
                return sym(s)

    def read_chars(self, inf, for_string):
        ret = ''
        while True:
            c = inf.getc()
            if for_string:
                if c == '"':
                    break
            else:
                if c == ' ' or c == '\t' or c == '\r' or c == '\n':
                    break
                if c == '(' or c == ')' or c == '"':
                    inf.unget(c)
                    break

            if c == '\\':
                c = inf.getc()
                if c == 'r':
                    c = '\r'
                elif c == 'n':
                    c = '\n'
                elif c == 't':
                    c = '\t'
            ret += c

        return ret

    def __str__(self):
        return str_sexp(self)

    def write(self, outf=None):
        if outf is None:
            outf = sys.stdout
        outf.write(str_sexp(self))

    def car(self):
        if len(self.list) > 0:
            return self.list[0]
        return None

    def tosym(self, name):
        if isinstance(name, str):
            return sym(name)
        return name

    def keyeq(self, name):
        return len(self.list) > 0 and self.list[0] == self.tosym(name)

    def assoc(self, key, create=False):
        key = self.tosym(key)
        for item in self.list:
            if isinstance(item, Sexp) and item.car() == key:
                return item
        if create:
            item = Sexp()
            item.list = [key]
            self.list.append(item)
            return item
        return None

    def get1(self, key):
        key = self.tosym(key)
        item = self.assoc(key)
        if isinstance(item, Sexp) and len(item.list) >= 2:
            return item.list[1]
        return None

    def put1(self, key, val):
        key = self.tosym(key)
        item = self.assoc(key)
        if item is Sexp:
            del item.list[1:]
            item.list.append(val)
        else:
            item = Sexp()
            item.list = [key, val]
            self.list.append(item)

    def get_multiple(self, key):
        key = self.tosym(key)
        item = self.assoc(key)
        if isinstance(item, Sexp) and len(item.list) > 0:
            return item.list[1:]
        return []

    def put_multiple(self, key, val):
        key = self.tosym(key)
        item = self.assoc(key, True)
        del item.list[1:]
        item.list.extend(val)

    def find_prop(self, pname):
        property_sym = sym('property')
        for clause in self.list:
            if isinstance(clause, Sexp) and len(clause.list) >= 3:
                if clause.list[0] == property_sym and clause.list[1] == pname:
                    return clause
        return None

    def get_prop(self, pname):
        prop = self.find_prop(pname)

        if prop is None:
            return None
        return prop.list[2]

    def set_prop(self, pname, pval):
        prop = self.find_prop(pname)
        if prop is None:
            prop = Sexp('property')
            prop.list.extend([pname, pval])
            self.list.append(prop)
        else:
            prop[2] = pval
        return prop

    def cadr(self):
        if len(self.list) < 2:
            return None
        return self.list[1]
    
