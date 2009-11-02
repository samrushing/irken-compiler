# -*- Mode: Python; tab-width: 4 -*-

import string
import sys

is_a = isinstance

# XXX I notice that the <atom> class is used for all the atoms
#     except for symbols (which are simply unadulterated strings).
#     should fix this.

class atom:
    def __init__ (self, kind, value):
        self.kind = kind
        self.value = value
    def __repr__ (self):
        return '<A %s %r>' % (self.kind, self.value)

class reader:

    def __init__ (self, file):
        self.file = file
        self.char = None
        self.line = 1

    def peek (self):
        if self.char is None:
            self.char = self.file.read (1)
        return self.char

    def next (self):
        result, self.char = self.char, self.file.read (1)
        if result == '\n':
            self.line += 1
        return result

    def skip_whitespace (self):
        while 1:
            ch = self.peek()
            if not ch:
                break
            elif ch not in string.whitespace:
                if ch == ';':
                    while self.next() not in '\r\n':
                        pass
                else:
                    break
            else:
                self.next()

    def read (self):
        self.skip_whitespace()
        ch = self.peek()
        if ch == '':
            raise EOFError, "Unexpected end of file"
        elif ch == '(':
            result = self.read_list()
        elif ch == '{':
            result = self.read_record()
        elif ch == '"':
            result = self.read_string()
        elif ch == "'":
            self.next()
            result = ['quote', self.read()]
        elif ch == ",":
            self.next()
            result = ['comma', self.read()]
        # unquote, etc.. can be found in old lumberjack code if needed.
        elif ch == '#':
            self.next()
            ch = self.peek()
            if ch == '\\':
                self.next()
                probe = self.read_atom()
                if probe == 'newline':
                    ch = '\n'
                elif probe == 'space':
                    ch = ' '
                else:
                    ch = probe[0]
                result = atom ('char', ch)
            elif ch in 'Xx':
                self.next()
                result = atom ('int', string.atoi (self.read_atom(), 16))
            elif ch in 'Oo':
                self.next()
                result = atom ('int', string.atoi (self.read_atom(), 8))
            elif ch in 'Bb':
                self.next()
                result = atom ('int', string.atoi (self.read_atom(), 2))
            elif ch in 'Tt':
                self.next()
                result = atom ('bool', 'true')
            elif ch in 'Ff':
                self.next()
                result = atom ('bool', 'false')
            elif ch in 'Uu':
                self.next()
                result = atom ('undefined', 'undefined')
            elif ch == '(':
                result = atom ('vector', self.read_list())
            # it's arguable: "{...}" or "#{...}" - the latter is more scheme-like
            #   but pointlessly noisier.
            #elif ch == '{':
            #    result = atom ('record', self.read_record())
            else:
                raise SyntaxError, 'Illegal #-escape character: "%s"' % ch
        elif ch in '-0123456789':
            a = self.read_atom()
            if a == '-':
                # bad, bad, bad
                result = '-'
            else:
                all_digits = 1
                for ch in a:
                    if ch not in '-0123456789':
                        all_digits = 0
                        break
                if all_digits:
                    result = atom ('int', string.atoi (a))
                else:
                    result = a
        else:
            result = self.read_atom()
        # hack to support postfix array-reference syntax
        self.skip_whitespace()
        ch = self.peek()
        if ch != '' and ch == '[':
            index = self.read_array_index()
            return ['%%array-ref', result, index]
        else:
            return result

    def read_atom (self):
        # read at least one character
        line = self.line
        result = self.next()
        while 1:
            ch = self.peek()
            if ch in string.whitespace or ch in '()[]{}':
                return result
            else:
                result = result + self.next()

    special = {'n':'\n','t':'\t'}

    def read_string (self):
        result = ''
        line = self.line
        # throw away the quote.
        ch = self.next()
        while 1:
            ch = self.peek()
            if ch == '"':
                # throw away the close-quote
                ch = self.next()
                return atom ('string', result)
            elif ch == '\\':
                # ignore this backslash, read the next char
                self.next()
                ch = self.next()
                if ch in 'xX':
                    # ascii escapes introduced only R6RS, *however*, theirs
                    #   is terminated by a semicolon and can be more than two hex
                    #   digits.
                    hex0 = self.next()
                    hex1 = self.next()
                    ch = chr (string.atoi (hex0 + hex1, 16))
                    result += ch
                else:
                    result += self.special.get (ch, ch)
            else:
                result += self.next()

    def read_list (self):
        result = []
        # throw away the paren
        paren = self.next()
        while 1:
            self.skip_whitespace()
            p = self.peek()
            if p == ')':
                # throw away the paren
                ch = self.next()
                return result
            else:
                exp = self.read()
                if is_a (exp, list) and len(exp) and exp[0] == 'include':
                    self.read_include (exp, result)
                else:
                    result.append (exp)

    def read_name (self):
        result = []
        while 1:
            p = self.peek()
            if not (p in string.letters or p in string.digits):
                return ''.join (result)
            else:
                result.append (p)
                self.next()

    def read_record (self):
        # { label=value label=value }
        result = []
        # skip open bracket
        self.next()
        while 1:
            self.skip_whitespace()
            p = self.peek()
            if p == '}':
                self.next()
                return atom ('record', result)
            else:
                name = self.read_name()
                self.skip_whitespace()
                if self.next() != '=':
                    raise SyntaxError ("expected '=' in record literal")
                else:
                    val = self.read()
                    result.append ((name, val))

    def read_array_index (self):
        # throw away open bracket
        self.next()
        exp = self.read()
        if self.read() not in ']}':
            raise SyntaxError ("expected closing ']/}' character")
        return exp

    def read_all (self):
        forms = []
        try:
            while 1:
                form = self.read()
                if is_a (form, list) and form[0] == 'include':
                    self.read_include (form, forms)
                else:
                    forms.append (form)
        except EOFError:
            return forms

    # XXX I'm not happy with this here, but if I put it in the transformer, it
    #   will require an extra pass *before* the transformer, because expand_body()
    #   will not recognize things hidden in an include (e.g., 'define' forms).
    def read_include (self, exp, result):
        filename = exp[1].value
        for sub in reader (open (filename, 'rb')).read_all():
            result.append (sub)

if __name__ == '__main__':
    import pprint
    import sys

    if len (sys.argv) < 2:
        file = sys.stdin
    else:
        file = open (sys.argv[1], 'r')

    p = reader (file)
    pprint.pprint (p.read_all())
