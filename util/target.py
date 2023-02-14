# -*- Mode: Python -*-

import os
import tempfile

# a somewhat desperate technique to get the default target
#   triple out of clang.

sample = """
int main (int argc, char * argv[])
{
  return 42;
}
"""

def get_triple():
    with tempfile.TemporaryDirectory() as td:
        path_c = os.path.join (td, 't0.c')
        path_s = os.path.join (td, 't0.ll')
        f = open (path_c, 'w')
        f.write (sample)
        f.close()
        cmd = 'clang %s -S -emit-llvm -o %s' % (path_c, path_s)
        r = os.system (cmd)
        lines = open (path_s, 'r').readlines()
        for line in lines:
            if line.startswith ('target triple'):
                [_, target] = line.split ('=')
                target = target.strip()[1:-1]
                return target

if __name__ == '__main__':
    print (get_triple())
