#
# dbinbox 
#
# Copyright (c) 2005 Lindquist Consulting, Inc.
# All rights reserved. 
#                                                                     
# Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
# This software may be used freely in any way as long as 
# the copyright statement above is not removed. 
#

use Mail::Util qw( read_mbox );
use Mail::Internet;

use Datascope ;
use filemail;

use Getopt::Std ;

$schema = "Mail1.2";

if ( ! getopts('1v') || @ARGV != 2 ) { 

    	die ( "Usage: $0 [-v] [-1] mail_file dbname\n" ) ; 

} else {
	
	$mfile = shift( @ARGV );
	$dbname = shift( @ARGV );
}

elog_init( "dbinbox", @ARGV );

if( $opt_v ) {
	
	$filemail::Verbose++;
}

dbcreate( $dbname, $schema );

@db = dbopen( $dbname, "r+" );
@dbsummary = dblookup( @db, "", "summary", "", "" );

dbtruncate( @dbsummary, 0 );

@msgs = read_mbox( $mfile );

$first = 1;

foreach $msg ( @msgs ) {

	if( $first == 1 ) {

		$first = 0;

		next if $opt_1;
	}

	$mailobj = new Mail::Internet( $msg );	
	$mailobj->head->unfold();

	$from = $mailobj->head->get( "From" );

	if( ! defined( $from ) || $from eq "" ) {
			
		$from = $mailobj->head->get( "Mail-From" );
	} 

	( $user, $host, $address ) = parse_address( $from );

	$epoch = get_epoch( $mailobj );

	if( $address eq "" || $epoch == 0  ) {
			
		elog_complain( "Insufficient header info for " .
			       "message! Skipping\n" );
		next;
	}

	$Nmessages{$address}++;

	if( ! defined( $Oldest{$address} ) || 
	    $Oldest{$address} > $epoch ) {

		$Oldest{$address} = $epoch;
	}

	if( ! defined( $Newest{$address} ) || 
	    $Newest{$address} < $epoch ) {

		$Newest{$address} = $epoch;
	}
}

foreach $address ( keys %Nmessages ) {

	dbaddv( @dbsummary, "from", $address,
			    "nmessages", $Nmessages{$address}, 
			    "oldest", $Oldest{$address},
			    "newest", $Newest{$address} );

	if( $opt_v ) {

		printf( "$Nmessages{$address} messages from $address\n" );
	}
}
