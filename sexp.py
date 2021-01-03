#! /usr/bin/env python3

import sys

class Symbol:
    def __init__(self, name):
        self.name = name

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
        c = self.getc()  # discard close paren
        return ret

    def read_symbol(self):
        return Symbol(self.read_chars(False))

    def read_string(self):
        return self.read_chars(True)

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

    def print(self, outf, exp):
        if isinstance(exp, str):
            outf.write('"')
            outf.write(exp)
            outf.write('" ')
        elif isinstance(exp, list):
            outf.write('(')
            for elt in exp:
                self.print(outf, elt)
                outf.write(' ')
            outf.write(')\n')
        elif isinstance(exp, Symbol):
            outf.write(exp.name)
        else:
            outf.write('? ')
            

#exp = Sexp().readfile('foo')
exp = Sexp().readfile('byhand/led.kicad_sch')

Sexp().print(sys.stdout, exp)

