# -*- Mode: -*-

import os
import time

execfile ('util/clean.py')
os.chdir ('..')
tarball = 'pxll.%s.tar.gz' % (time.strftime ('%y%m%d'),)
# COPYFILE_DISABLE is a hack to turn off storing resource forks in Leopard.
os.system ('COPYFILE_DISABLE=true tar --exclude .svn -zcvf %s pxll' % (tarball,))
os.system ('scp %s dark:public_html/pxll/' % (tarball,))
os.chdir ('pxll')
