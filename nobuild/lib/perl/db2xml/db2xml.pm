package Datascope::db2xml;

use 5.006;
use strict;
use warnings;
use Carp;

require Exporter;
require DynaLoader;
use vars qw($VERSION @ISA @EXPORT);

our @ISA = qw(Exporter DynaLoader);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Datascope::db2xml ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.

our @EXPORT = qw(
	db2xml
);
our $VERSION = '1.0';

bootstrap Datascope::db2xml $VERSION;

# Preloaded methods go here. 

# Autoload methods go after =cut, and are processed by the autosplit program. 

1;
