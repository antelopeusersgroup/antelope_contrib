antelope_contrib
================

Contributed software for use with the Antelope Environmental Monitoring
System from [BRTT, Inc.][brtt]

Maintained by members of the [Antelope Users Group][aug].

  [brtt]: http://www.brtt.com
  [aug]: http://www.antelopeusersgroup.org

Layout of this repository
-------------------------

Code in this repository is laid out in a few top-level dirctories.

C shared libraries, Perl modules, and Python modules live under the `lib`
directory.

The `bin` directory contains executables. Code that talks to instruments
typically lives under `bin/rt`.

Third-party language bindings for PHP, Java, and Matlab live in `data`.

Usage
-----

All code in this repository requires a working Antelope installation.
Additionally, the Antelope environment must be configured in your shell
environment.

Typically, this repository is checked out in `$ANTELOPE/src`.

Compilation is handled by the UNIX `make` command. Most of the Makefiles in
this repository make use of the antelopemake mechanism, which is a bit of
Antelope-specific syntacic sugar and macros.

Initial setup:
```
# For Bourne shells
. /opt/antelope/5.3/setup.sh
# For C shells:
# source /opt/antelope/5.3/setup.csh
cd $ANTELOPE
mkdir
git clone https://github.com/antelopeusersgroup/antelope_contrib.git src
```

Compilation:
```
cd $ANTELOPE/src
make Include
make
make install
```

Contributing
------------

As a rule, all code in this repository must have at a minimum:

1. A Makefile set up to use the antelopemake rules
2. A man page. This can either be formatted by hand or created with a
   documentation package like Doxygen, sphinx or javadoc. The resultant
   troff-formatted manpage must be committed with the code.
