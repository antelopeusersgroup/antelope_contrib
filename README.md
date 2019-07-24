antelope\_contrib
================

Contributed software for use with the Antelope Environmental Monitoring
System from [BRTT, Inc.][brtt]

Maintained by members of the [Antelope Users Group][aug].

The Antelope Contrib [source code is available][repo] on GitHub

  [brtt]: http://www.brtt.com
  [aug]: http://www.antelopeusersgroup.org
  [repo]: http://github.com/antelopeusersgroup/antelope_contrib

Inclusion in Antelope
---------------------

BRTT includes compiled versions of the software in this repository with every
release of Antelope, subject to some basic quality control guidelines. The
[Contributing](#contributing) section below contains some guidelines.

Layout of the antelope\_contrib Git repository
-------------------------

Each directory containing Antelope code is expected to contain a `Makefile`
written in the `antelopemakefile(5)` format. The Antelope build process will
not build any directory that does not contain a `Makefile`.

Code in this repository is laid out in a few top-level dirctories.

* `first` - Code that is necessary for antelope\_contrib to compile properly.
  Built before anything else. Use sparingly.
* `lib` - C shared libraries, Perl modules, and Python modules
* `bin` - The `bin` directory contains executables.
  * `bin/rt` - Code that talks to instruments typically lives under `bin/rt`.
  * `bin/export` - Code that allows other software packages to use Antelope data lives in
    `bin/export`.
* `data` - Third-party language bindings for `PHP`, and `Java`, plus data-only
  files like travel time databases and instrument response curves.
* `nobuild` - Older code that is abandoned by the author or no longer works with the
  current version of Antelope, but may be interesting to others

Usage
=====

All code in this repository requires a working Antelope installation.
Additionally, the Antelope environment must be configured in your shell
environment.

Historically, this repository was checked out in `$ANTELOPE/src`, but can be
checked out to any location that the user desires.

Compilation is handled by the UNIX `make` command. Most of the `Makefiles` in
this repository make use of the `antelopemake(5)` mechanism, which is a bit of
Antelope-specific syntacic sugar and macros.

Initial setup
-------------

In the instructions below, make sure to substitute the correct version of
Antelope.

You may also choose to check out the contributed code to another location than
`$ANTELOPE/src`.

### For Bourne shells:

    . /opt/antelope/5.6/setup.sh
    cd $ANTELOPE
    git clone https://github.com/antelopeusersgroup/antelope_contrib.git src

### For C shells:

    source /opt/antelope/5.6/setup.csh
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
    cd $ANTELOPE/src/first/localmake_config
    make Include

    # Install the localmake command
    cd ../localmake
    make Include; make; make install
    cd ../../

    # Run localmake_config to define the paths to various third-party software
    localmake_config

Compilation
-----------

    cd $ANTELOPE/src # or where ever you checked out the repository
    make Include
    make
    make install

Contributing
============

Code Contribution Rules
-----------------------

As a rule, all code in this repository MUST at a minimum:

1. Compile cleanly on the supported Antelope platforms (RHEL 6+ and Mac OSX
   10.8+)
2. Contain a Makefile set up to use the `antelopemake(5)` rules, and with the
   `SUBDIR` macro set to `/contrib` (See the [Example
   Makefile](#example-makefile) below)
3. Contain a man page describing how to use the program or library. This can be
   formatted by hand or created with a documentation package like `Doxygen`,
   `sphinx`, `pod2man`, or `javadoc`.
4. Contain a file called `LICENSE` that clearly states the license that program
   is released with. See the [Licensing](#licensing) section below for
   acceptable licenses.

Development Process
-------------------

1. Fork it (https://github.com/antelopeusersgroup/antelope_contrib/fork)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

Example Makefile
----------------

```
BIN = myprog
MAN1 = $(BIN).1

SUBDIR=/contrib
include $(ANTELOPEMAKE)
```

Licensing
=========

All code in this repository is expected to be readily distributed. In order for
pre-compiled versions of your code to be included with the Antelope
distribution, it must be accompanied by a `LICENSE` file, and be of a type that
lends itself to inclusion in commercial packages. *Generally speaking,
[BSD][bsd2clause] and [MIT][mitlicense] style licenses are ok, but GNU GPL
and LGPL are not.*

For more information on [BRTT][brtt]'s rules for code contribution with Antelope, please see

BRTT's [contrib licensing page][contrib-lic].

  [contrib-lic]: http://www.brtt.com/contrib_software.html
  [bsd2clause]: https://opensource.org/licenses/BSD-2-Clause
  [mitlicense]: https://opensource.org/licenses/MIT
