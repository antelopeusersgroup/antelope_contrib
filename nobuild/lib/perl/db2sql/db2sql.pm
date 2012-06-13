package Datascope::db2sql;

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

# This allows declaration       use Datascope::db2sql ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.

our @EXPORT = qw(
	dbschema2sqlcreate
	db2sqlinsert
	db2sqldelete
	db2sql_get_syncfield_name
	db2sql_set_syncfield_name
	pfconfig_asknoecho
);
our $VERSION = '1.0';

bootstrap Datascope::db2sql $VERSION;

# Preloaded methods go here. 

# Autoload methods go after =cut, and are processed by the autosplit program. 

1;
