# -*- Mode: -*-

import os
import time

execfile ('util/clean.py')
os.chdir ('..')
tarball = 'irken.%s.tar.gz' % (time.strftime ('%y%m%d'),)
# COPYFILE_DISABLE is a hack to turn off storing resource forks in Leopard.
os.system ('COPYFILE_DISABLE=true tar --exclude .svn -zcvf %s irken' % (tarball,))
os.system ('scp %s dark:public_html/irken/' % (tarball,))
os.chdir ('irken')
