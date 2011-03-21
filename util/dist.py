# -*- Mode: Python -*-

import os
import time

execfile ('util/clean.py')
os.system ('../svn2cl-0.12/svn2cl.sh')
os.chdir ('..')
tarball = 'irken.%s.tar.gz' % (time.strftime ('%y%m%d'),)
# COPYFILE_DISABLE is a hack to turn off storing resource forks in Leopard.
os.system ('COPYFILE_DISABLE=true tar --exclude .svn -zcvf %s irken' % (tarball,))
os.system ('scp %s dark:public_html/irken/' % (tarball,))
os.system ("ssh dark '(cd public_html/irken; tar -zxvf %s)'" % (tarball,))
os.chdir ('irken')
