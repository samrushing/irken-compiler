Installing Irken
================

Irken can be downloaded from its GitHub page.

Clone the `master` branch of the `irken-compiler` repository

    $ git clone https://github.com/samrushing/irken-compiler.git

Change to the newly created repository directory and run the python bootstrap script

    $ cd irken-compiler
    $ python util/bootstrap.py

Compilation will take a few minutes. If you don't get any error messages, you should be ready for installation. The installation process will install all files under `/usr/local`. To install:

    $ python util/install.py

Once complete, you can confirm that Irken is installed and ready to use by compiling a simple demo program. Assuming `/usr/local/bin` is in your path:

    $ irken demo/hello.scm
    $ demo/hello
    Hello, World!
