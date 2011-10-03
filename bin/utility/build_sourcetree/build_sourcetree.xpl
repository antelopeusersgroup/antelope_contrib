#
#   Copyright (c) 2008 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   This software is licensed under the New BSD license: 
#
#   Redistribution and use in source and binary forms,
#   with or without modification, are permitted provided
#   that the following conditions are met:
#   
#   * Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the
#   following disclaimer.
#   
#   * Redistributions in binary form must reproduce the
#   above copyright notice, this list of conditions and
#   the following disclaimer in the documentation and/or
#   other materials provided with the distribution.
#   
#   * Neither the name of Lindquist Consulting, Inc. nor
#   the names of its contributors may be used to endorse
#   or promote products derived from this software without
#   specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
#   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
#   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#

use Getopt::Std;

use Datascope;

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( ! getopts( 'p:v' ) || @ARGV != 1 ) {
	
	elog_die( "Usage: build_sourcetree [-p pfname] [-v] project" );

} else {
	
	$project = pop( @ARGV );

	$project = lc( $project );
	$Project = uc( $project );
}

if( $opt_p ) {

	$Pf = $opt_p;

} else {

	$Pf = $Program;
}

$base_dir = pfget( $Pf, "base_dir" );
$source_dir = pfget( $Pf, "source_dir" );
%files = %{pfget( $Pf, "files" )};

if( -e "$source_dir" ) {

	elog_die( "Won't overwrite existing directory '$source_dir'. Bye.\n" );

} elsif( $source_dir eq "" ) {

	elog_die( "Must specify source_dir in parameter file. Bye.\n" );

} else {

	makedir( $source_dir );
}

foreach $file ( keys %files ) {

	$key = $file;

	$file =~ s/__PROJECT__/$Project/g;
	$file =~ s/__project__/$project/g;

	$files{$key} =~ s/__PROJECT__/$Project/g;
	$files{$key} =~ s/__project__/$project/g;
	$files{$key} =~ s/__BASEDIR__/$base_dir/g;

	$path = concatpaths( $source_dir, $file );

	if( $path =~ m@/@ ) {

		$dir = $path;
		$dir =~ s@[^/]*$@@;

		if( $opt_v ) {

			elog_notify( "Creating $dir\n" );
		}

		makedir( $dir );
	}

	if( $opt_v ) {

		elog_notify( "Creating $path\n" );
	}

	open( F, ">$path" );

	print F $files{$key};

	close F;
}
