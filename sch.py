import sys
import io
import tokenize
import token

class Val:
    def __init__(self, s=None):
        if s:
            self.override = str(s)

    def token(self, tok, minus_flag=False):
        self.tok = tok
        self.minus_flag = minus_flag
        self.override = None
        return self

    def __str__(self):
        if self.override:
            return self.override
        if self.minus_flag:
            return f'MINUS {self.tok.string}'
        return self.tok.string

    def __repr__(self):
        if self.override:
            return self.override
        if self.minus_flag:
            return f'<{token.tok_name[self.tok.type]} MINUS {self.tok.string}>'
        return f'<{token.tok_name[self.tok.type]} {self.tok.string}>'

    def equal(self, s):
        if self.override:
            return self.override == s
        return self.tok.string == s

    @classmethod
    def make_val(cls, s):
        val = cls(None)
        val.override = s
        return val


class Sch:
    def readfile(self, filename):
        with tokenize.open(filename) as inf:
            return self.read(inf.readline)
        
    def readstring(self, s):
        with io.StringIO(s) as inf:
            return self.read(inf.readline)

    def read(self, readline):
        self.tokens = tokenize.generate_tokens(readline)
        self.peek_tok = None
        return self.parse()
        

    def peek(self):
        while True:
            tok = next(self.tokens)
            if tok.type == tokenize.ENDMARKER:
                return None
            if tok.type == tokenize.NEWLINE:
                continue
            if tok.type == tokenize.NL:
                continue
            break
        self.peek_tok = tok
        return tok

    def next(self):
        if self.peek_tok is not None:
            tok = self.peek_tok
            self.peek_tok = None
            return tok
        return self.peek()

    def parse(self):
        tok = self.next()
        if tok is None:
            return None

        if tok.type == tokenize.NUMBER:
            return Val().token(tok)

        if tok.type == tokenize.NAME:
            return Val().token(tok)

        if tok.type == tokenize.STRING:
            return Val().token(tok)

        if tok.type == tokenize.OP and tok.string == '-':
            return Val().token(self.next(), minus_flag=True)

        if tok.type == tokenize.OP and tok.string == '(':
            return self.parse_list()

        print('LAST', tok)
        sys.exit(0)

    def parse_list(self):
        ret = []
        while True:
            tok = self.peek()
            if (type(tok) == tokenize.TokenInfo
                and tok.type == tokenize.OP
                and tok.string == ')'):
                self.next()
                break

            ret.append(self.parse())

        return ret

def write_sexp(outf, val):
    if type(val) == list:
        outf.write('(')
        for elt in val:
            write_sexp(outf, elt)
            outf.write(' ')
        outf.write(')\n')
    elif type(val) == Val:
        if val.override:
            outf.write(val.override)
        else:
            if val.minus_flag:
                outf.write('-')
            outf.write(val.tok.string)
    elif type(val) == float:
        outf.write(f'{val:.4f}')
    else:
        outf.write(str(val))

def sexp_to_str(val):
    with io.StringIO() as outf:
        write_sexp(outf, val)
        return outf.getvalue()

if __name__ == '__main__':
    # val = Sch().readfile('small.l')
    val = Sch().readstring('(foo "bar" 1 2.2 (-3))')

    print('===')
    print('val0', val[0])
    print('val0', repr(val[0]))
    print('repr', repr(val))

    val[0] = Val('"xyzzy"')

    with io.StringIO() as outf:
        write_sexp(outf, val)
        print(outf.getvalue())
        


        


