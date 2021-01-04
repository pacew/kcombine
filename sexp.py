#! /usr/bin/env python3

import sys
import numbers
from uuid import uuid4


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


class Sexp:
    def __init__(self):
        self.peekc = None
        self.inf = None

    def peek(self):
        if self.peekc is None:
            self.peekc = self.inf.read(1)
        return self.peekc

    def getc(self):
        c = self.peek()
        self.peekc = None
        return c

    def unget(self, c):
        self.peekc = c

    def readfile(self, filename):
        self.inf = open(filename)
        return self.read_exp()

    def read_exp(self):
        while True:
            c = self.getc()
            if c == ' ' or c == '\t' or c == '\r' or c == '\n':
                pass
            elif c is None or c == '':
                return None
            elif c == ')':
                self.unget(c)
                return None
            elif c == '(':
                return self.read_list()
            elif c == '"':
                return self.read_string()
            elif c == '-' or c == '.' or ('0' <= c <= '9'):
                self.unget(c)
                return self.read_number()
            else:
                self.unget(c)
                return self.read_symbol()

    def read_list(self):
        ret = list()
        while True:
            elt = self.read_exp()
            if elt is None:
                break
            ret.append(elt)
        self.getc()  # discard close paren
        return ret

    def read_symbol(self):
        return sym(self.read_chars(False))

    def read_string(self):
        return self.read_chars(True)

    def read_number(self):
        s = self.read_chars(False)
        try:
            val = int(s)
        except ValueError:
            try:
                val = float(s)
            except ValueError:
                print('can\'t parse number', repr(s))
                val = 'oops'
        return val

    def read_chars(self, for_string):
        ret = ''
        while True:
            c = self.getc()
            if for_string:
                if c == '"':
                    break
            else:
                if c == ' ' or c == '\t' or c == '\r' or c == '\n':
                    break
                if c == '(' or c == ')' or c == '"':
                    self.unget(c)
                    break

            if c == '\\':
                c = self.getc()
                if c == 'r':
                    c = '\r'
                elif c == 'n':
                    c = '\n'
                elif c == 't':
                    c = '\t'
            ret += c

        return ret

    @classmethod
    def print_exp(cls, exp, outf=None):
        if outf is None:
            outf = sys.stdout
        if isinstance(exp, str):
            outf.write('"')
            outf.write(exp)
            outf.write('"')
        elif isinstance(exp, list):
            outf.write('(')
            need_space = False
            for elt in exp:
                if need_space:
                    outf.write(' ')
                cls.print_exp(elt, outf)
                need_space = True
            outf.write(')\n')
        elif isinstance(exp, Sym):
            outf.write(exp.name)
        elif isinstance(exp, int):
            outf.write(str(exp))
        elif isinstance(exp, float):
            outf.write(f'{exp:.4f}')
        else:
            outf.write('? ')


