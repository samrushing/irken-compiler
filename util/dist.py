# -*- Mode: Python -*-

import os
import time

execfile ('util/clean.py')
os.system ('../svn2cl-0.12/svn2cl.sh')
os.chdir ('..')
tarball = 'irken.%s.tar.gz' % (time.strftime ('%y%m%d'),)
# COPYFILE_DISABLE is a hack to turn off storing resource forks in Leopard.
os.system ('COPYFILE_DISABLE=true tar --exclude .svn -zcvf %s irken' % (tarball,))

# test bootstrap to get official self/compile.c
os.system ('mkdir irken-staging')
os.chdir ('irken-staging')
os.system ('tar -zxvf ../%s' % (tarball,))
os.chdir ('irken')
os.system ('cp self/flags.scm self/flags.backup.scm')
# do the full bootstrap
execfile ('util/bootstrap.py')
# build a 'generic' self/compile.c with default self/flags.scm
os.system ('mv self/flags.backup.scm self/flags.scm')
os.system ('self/compile self/compile.scm')
execfile ('util/clean.py')
os.chdir ('..')
# remake the tarball
os.system ('COPYFILE_DISABLE=true tar --exclude .svn -zcvf %s irken' % (tarball,))

# push it
os.system ('scp %s dark:public_html/irken/' % (tarball,))
os.system ("ssh dark '(cd public_html/irken; tar -zxvf %s)'" % (tarball,))
os.system ('rm -fr irken-staging')
os.chdir ('irken')

