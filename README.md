antelope_contrib
================

Contributed software for use with the Antelope Environmental Monitoring
System from [BRTT, Inc.][brtt]

Maintained by members of the [Antelope Users Group][aug].

  [brtt]: http://www.brtt.com
  [aug]: http://www.antelopeusersgroup.org

Inclusion in Antelope
---------------------

BRTT includes compiled versions of the software in this repository with every
release of Antelope, subject to some basic quality control guidelines. The
[contributing](#contributing) section below contains some guidelines.


Layout of this repository
-------------------------

Code in this repository is laid out in a few top-level dirctories.

* C shared libraries, Perl modules, and Python modules live under the `lib`
  directory.

* The `bin` directory contains executables. Code that talks to instruments
  typically lives under `bin/rt`.

* Third-party language bindings for PHP, Java, and Matlab live in `data`.

Usage
=====

All code in this repository requires a working Antelope installation.
Additionally, the Antelope environment must be configured in your shell
environment.

Typically, this repository is checked out in `$ANTELOPE/src`.

Compilation is handled by the UNIX `make` command. Most of the `Makefiles` in
this repository make use of the `antelopemake(5)` mechanism, which is a bit of
Antelope-specific syntacic sugar and macros.

Initial setup
-------------

### For Bourne shells:

    . /opt/antelope/5.3/setup.sh
    cd $ANTELOPE
    git clone https://github.com/antelopeusersgroup/antelope_contrib.git src

### For C shells:

    source /opt/antelope/5.3/setup.csh
    cd $ANTELOPE
    git clone https://github.com/antelopeusersgroup/antelope_contrib.git src

### localmake

Some of the code in this repository needs to link against third party software
applications and libraries that may not be present on all systems. In order for
this code to compile, the Makefiles for some code use the localmake mechanism
to read a set of pre-defined paths to libraries and other applications. No
defaults are provided - you must run the `localmake_config` command to set up
these macros. Basic boot-strapping for `localmake` looks like this:

    # Install the localmake_config command from source
    cd $ANTELOPE/src/lib/localmake_config
    make Include

    # Install the localmake command
    cd ../../bin/utility/localmake
    make Include; make; make install
    cd ../../


    # Run localmake_config to define the paths to various third-party software
    localmake_config


Compilation
-----------

    cd $ANTELOPE/src
    make Include
    make
    make install

<h1 id="contributing">Contributing</h1>

As a rule, all code in this repository must at a minimum:

1. compile cleanly on the supported Antelope platforms
2. contain a Makefile set up to use the `antelopemake(5)` rules
3. include a man page describing how to use the program or library. This can either be
   formatted by hand or created with a documentation package like Doxygen,
   sphinx or javadoc. The resultant troff-formatted manpage must be committed
   with the code.

The AUG wiki page on Github contains [instructions on how to get started
contributing][contribute] to this repository.

  [contribute]: https://github.com/antelopeusersgroup/antelope_contrib/wiki/Setting-up-to-modify-Antelope-contrib-via-git
