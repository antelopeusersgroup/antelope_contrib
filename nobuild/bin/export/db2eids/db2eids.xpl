
#
#   Copyright (c) 2007 Lindquist Consulting, Inc.
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

use Getopt::Std ;
 
#Fake the trwfname call since it's not in the perldb interface
sub trwfname {
	my( $pattern ) = pop( @_ );
	my( @db ) = @_;

	my( $orid, $time ) = dbgetv( @db, "orid", "time" );

	my( $path_from_db, $fullpath, $tabledir, $relpath, $dir );

	$path_from_db = epoch2str( $time, $pattern );
	$path_from_db =~ s/{orid}/$orid/;
	$path_from_db =~ s/{evid}/$evid/;

	if( $path_from_db !~ m@^/@ ) {

		$tabledir = dbquery( @db, dbTABLE_DIRNAME );

		$path_from_here = concatpaths( $tabledir, $path_from_db );

	} else {

		$path_from_here = $path_from_db;
	}

	( $dir, $base, $suffix ) = parsepath( $path_from_db );
		
	$dfile = $base;
	if( $suffix ne "" ) {
		$dfile .= "." . $suffix;
	}

	dbputv( @db, "dir", $dir, "dfile", $dfile );

	( $dir_from_here, $base, $suffix ) = parsepath( $path_from_here );

	makedir( "$dir_from_here" );

	return $path_from_here;
}

sub save_eids {
	my( $xml ) = pop( @_ );
	my( $time ) = pop( @_ );
	my( @db ) = @_;

	my( $orid ) = dbgetv( @db, "orid" );

	@db = dblookup( @db, "", "eids", "", "" );

	$db[3] = dbaddv( @db, "orid", $orid, "time", $time );

	my( $filename ) = trwfname( @db, $eids_filename_template );

	if( ! open( F, ">$filename" ) ) {

		elog_complain( "Failed to open '$filename'!\n" );

		dbdelete( @db );

		return;
	}

	print F $stylesheet->output_string( $xml );

	close( F );

	if( $opt_V ) {

		elog_notify( "Added row $db[3] to '$dbname.eids'\n" );
		elog_notify( "Wrote EIDS XML file '$filename'\n" );
	}

	return $filename;
}

sub postprocess {
	my( $filename ) = @_;

	if( ! defined( $filename ) || 
	    ! defined( $postprocess_command ) || 
	    $filename eq "" ||
	    $postprocess_command eq "" ) {

		return;
	}

	my( $cmd ) = $postprocess_command;

	$cmd =~ s/EIDSFILE/$filename/g;

	if( $opt_v ) {

		elog_notify( "Executing: $cmd\n" );
	}

	system( "$cmd" );
}


$Pfname = "db2eids";

$Program = $0 ; 
$Program =~ s".*/"" ;

elog_init( $Program, @ARGV );

if ( ! getopts('1p:vV') || @ARGV != 1 ) { 

	die ( "Usage: $pgm [-1] [-p pfname] [-vV] database\n" ) ; 

} else {
	
	$dbname = pop( @ARGV );
}

if( $opt_V ) {
	
	$opt_v++;
}

if( $opt_p ) {
	
	$Pfname = $opt_p;
}

use XML::LibXML;
use XML::LibXSLT;

use Datascope;
use Datascope::db2xml;

$xslt_stylesheet = pfget( $Pfname, "xslt_stylesheet" );
$query_interval_sec = pfget( $Pfname, "query_interval_sec" );
$eqmessage_source = pfget( $Pfname, "eqmessage_source" );
$sent_time_format = pfget( $Pfname, "sent_time_format" );
$eids_filename_template = pfget( $Pfname, "eids_filename_template" );
$generic_rootnode = pfget( $Pfname, "generic_rootnode" );
$db2xml_rootnode = pfget( $Pfname, "db2xml_rootnode" );
$db2xml_rownode = pfget( $Pfname, "db2xml_rownode" );
$postprocess_command = pfget( $Pfname, "postprocess_command" );
@dbprocess_commands = @{pfget( $Pfname, "dbprocess_commands" )};
%generic_xml_elements = %{pfget( $Pfname, "generic_xml_elements" )};

@fields = keys( %generic_xml_elements );

foreach $field ( @fields ) {

	if( $generic_xml_elements{$field} eq "" ) {

		push( @expressions, $field );

	} else {

		push( @expressions, $generic_xml_elements{$field} );
	}
}

if( ! defined( $xslt_stylesheet ) ||
    $xslt_stylesheet eq "" || 
    ! -e $xslt_stylesheet ) {

	die( "Couldn't find XSLT sylesheet '$xslt_stylesheet'\n" );
}

$parser = XML::LibXML->new();
$xslt = XML::LibXSLT->new();

$style_doc = $parser->parse_file( $xslt_stylesheet );
$stylesheet = $xslt->parse_stylesheet( $style_doc );

@db = dbopen( $dbname, "r+" );

if( $db[0] < 0 ) {
	
	die( "Failed to open database '$dbname'. Bye.\n" );
}

$xml_generation_time = now();

$sent = epoch2str( $xml_generation_time, $sent_time_format );

$header_xml = "<$generic_rootnode>" . 
	      "\n<eqmessage_source>$eqmessage_source</eqmessage_source>" .
	      "\n<sent>$sent</sent>";

$trailer_xml = "</$generic_rootnode>";

for( ;; ) {

	@dbp = dbprocess( @db, @dbprocess_commands );

	$nrecs = dbquery( @dbp, dbRECORD_COUNT );

	if( $opt_v ) {

		elog_notify( "Processing $nrecs hypocenters\n" );
	}

	for( $dbp[3] = 0; $dbp[3] < $nrecs; $dbp[3]++ ) {


		$orid = dbgetv( @dbp, "orid" );

		if( $opt_v ) {

			elog_notify( "Processing orid $orid\n" );
		}

		$dbxml = db2xml( @dbp, 
				 $db2xml_rootnode, 
				 $db2xml_rownode,
				 \@fields, 
				 \@expressions );

		$xml = "$header_xml\n" .
		       "$dbxml\n" .
		       "$trailer_xml\n";

		if( $opt_V ) {

			print "Converting generic XML:\n$xml\n\n";
		}

		$source = $parser->parse_string( $xml );

		$result = $stylesheet->transform( $source );

		if( $opt_V ) {

			print "To resulting EIDS message:\n\n" . 
				$stylesheet->output_string( $result ) . 
				"\n\n";
		}

		$filename = save_eids( @dbp, $xml_generation_time, $result );

		postprocess( $filename );
	}

	if( $opt_1 ) {
		
		exit( 0 );

	} else {

		sleep( $query_interval_sec );
	}
}
