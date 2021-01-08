#! /usr/bin/env python3

import sys


class Sym:
    table = dict()

    def __init__(self, name):
        self.name = name

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


def print_sexp(elt, outf=None):
    if outf is None:
        outf = sys.stdout
    if isinstance(elt, str):
        outf.write('"')
        for c in elt:
            if c == '\r':
                outf.write('\\r')
            elif c == '\n':
                outf.write('\\n')
            elif c == '\t':
                outf.write('\\t')
            elif c == '\\':
                outf.write('\\\\')
            else:
                outf.write(c)
        outf.write('"')
    elif isinstance(elt, Sexp):
        elt.print(outf)
    elif isinstance(elt, Sym):
        outf.write(elt.name)
    elif isinstance(elt, int):
        outf.write(str(elt))
    elif isinstance(elt, float):
        outf.write(f'{exp:.4f}')
    else:
        outf.write('OOPS')


class Sexp:
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
                return self.read_number(inf)
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

    def read_number(self, inf):
        s = self.read_chars(inf, False)
        try:
            val = int(s)
        except ValueError:
            try:
                val = float(s)
            except ValueError:
                print('can\'t parse number', repr(s))
                val = 'oops'
        return val

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

    def print(self, outf=None):
        if outf is None:
            outf = sys.stdout
        outf.write('(')
        need_space = False
        for elt in self.list:
            if need_space:
                outf.write(' ')
            need_space = True
            
            print_sexp(elt, outf)
        outf.write(')\n')
                
    def car(self):
        if len(self.list) > 0:
            return self.list[0]
        return None

    def assoc(self, key, create=False):
        for item in self.list:
            if isinstance(item, Sexp) and item.car() == key:
                return item
        if create:
            item = Sexp()
            item.list = [key]
            self.list.append(item)
            return item
        return None

    def assoc_get(self, key):
        item = self.assoc(key)
        if isinstance(item, Sexp) and len(item.list) >= 2:
            return item.list[1]
        return None

    def assoc_set(self, key, val):
        item = assoc(key)
        if item is Sexp:
            del item.list[1:]
            item.list.append(val)
        else:
            item = Sexp()
            item.list = [key, val]
            self.list.append(item)

    def assoc_set_multiple(self, key, val):
        item = assoc(key)
        if item is Sexp:
            del item.list[1:]
            item.extend(val)
        else:
            item = Sexp()
            item.list = [key] + val
