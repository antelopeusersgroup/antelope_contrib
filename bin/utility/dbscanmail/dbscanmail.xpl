#
# dbscanmail 
#
# Copyright (c) 2005 Lindquist Consulting, Inc.
# All rights reserved. 
#                                                                     
# Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
# This software may be used freely in any way as long as 
# the copyright statement above is not removed. 
#

use Mail::Internet;
use Datascope ;
use filemail;
require "getopts.pl" ;

@valid_operations = ( "set_messageid", 
		      "list_contenttypes",
		      "build_references", 
		      "fix_bytecount" );

sub get_mailobj_bybytes {
	my( @db ) = @_;
	my( $msg, @msg );

	my( $filename ) = dbextfile( @db );

	my( $foff, $bytes ) = dbgetv( @db, "foff", "bytes" );

	if( ! open( F, $filename ) ) {

		elog_die( "Failed to open $filename for record $db[3]. Bye!\n" );
	}

	seek( F, $foff, 0 );

	read( F, $msg, $bytes );

	close( F );

	@msg = split( /\n/, $msg, -1 );

	$mailobj = new Mail::Internet( \@msg );
}

sub get_msgarray_bylines {
	my( @db ) = @_;

	my( $filename ) = dbextfile( @db );

	my( $foff, $lines ) = dbgetv( @db, "foff", "lines" );

	if( ! open( F, $filename ) ) {

		elog_die( "Failed to open $filename for record $db[3]. Bye!\n" );
	}

	seek( F, $foff, 0 );

	my( $nlines_read ) = 0;
	my( $nbytes_read ) = 0;
	my( @msg ) = ();

	while( ( $line = <F> ) && $nlines_read < $lines ) {

		push( @msg, $line );
		$nbytes_read += length( $line );
		$nlines_read++;
	}

	close( F );

	return( \@msg, $nlines_read, $nbytes_read );
}

sub describe_message {
	my( @db ) = @_;
	my( $descrip, $address );
	
	my( $table ) = dbquery( @db, dbTABLE_NAME );

	if( $table eq "in" ) {
		
		$descrip = "From:";
		$address = dbgetv( @db, "from" );

	} elsif( $table eq "out" ) {

		$descrip = "To:";
		$address = dbgetv( @db, "to" );

	} else {

		elog_complain( "unexpected error in describe_message(): " .
			       "not a message row\n" );
		return "";
	}

	my( $time, $subject, $messageid ) =
		dbgetv( @db, "time", "subject", "messageid" );

	return "$descrip $address at " . strtime( $time ) . 
	       "  Subject: '$subject' Message-ID: '$messageid'";
}

sub fix_bytecount {
	my( @db ) = @_;

	if( scalar( @db ) < 4 ) {
		# finished, do nothing
		return;
	} 

	( $lines, $bytes ) = dbgetv( @db, "lines", "bytes" );

	( $ref, $nlines_read, $nbytes_read ) = get_msgarray_bylines( @db );

	if( $nbytes_read != $bytes ) {

		dbputv( @db, "bytes", $nbytes_read );
	
		elog_complain( "FIXED record $db[3]: change $bytes bytes " .
				"to $nbytes_read bytes for message " . 
				describe_message( @db ) . "\n" );

		return 1;

	} else {

		return 0;
	}
}

sub build_references {
	my( @db ) = @_;

	if( scalar( @db ) < 4 ) {
		# finished, do nothing
		return;
	} 

	my( @dbrefs ) = dblookup( @db, "", "references", "", "" );

	$mailobj = get_mailobj_bybytes( @db );

	$mailobj->head->unfold();

	my( $message_id, $in_reply_to, $references );
	my( @in_reply_to, @references );

	chomp( $message_id = $mailobj->head->get( "Message-ID" ) );
	chomp( $in_reply_to = $mailobj->head->get( "In-Reply-To" ) );
	chomp( $references = $mailobj->head->get( "References" ) );

	@in_reply_to = parse_message_ids( $in_reply_to );
	@references = parse_message_ids( $references );

	# Remove references that are already in the in_reply_to header:

	@references = map { $ref = $_; 
			    grep( m/$ref/, @in_reply_to ) ? 
		            () : $ref } @references;

	if( defined( $message_id ) ) {
		
		$message_id = clean_message_id( $message_id );

		dbputv( @db, "messageid", $message_id );

		foreach $ref ( @in_reply_to ) {
			
			dbaddv( @dbrefs, "messageid", $message_id,
					 "inreplyto", "y", 
					 "reference", $ref );
		}

		foreach $ref ( @references ) {
			
			dbaddv( @dbrefs, "messageid", $message_id,
					 "reference", $ref );
		}

		return 0;

	} else {

		elog_complain( "No message id found: " .
				describe_message( @db ) . "\n" );


		return 1;
	}
}

sub set_messageid {
	my( @db ) = @_;

	if( scalar( @db ) < 4 ) {
		# finished, do nothing
		return;
	} 

	$mailobj = get_mailobj_bybytes( @db );

	$mailobj->head->unfold();

	chomp( $message_id = $mailobj->head->get( "Message-ID" ) );

	if( defined( $message_id ) ) {
		
		$message_id = clean_message_id( $message_id );

		dbputv( @db, "messageid", $message_id );

		return 0;

	} else {

		elog_complain( "No message id found: " .
				describe_message( @db ) . "\n" );

		return 1;
	}

}

sub list_contenttypes {
	my( @db ) = @_;

	if( scalar( @db ) < 4 ) {
		print "Totals:\n";
		foreach $key ( keys %seen ) {
			print "\t\"$key\"\t$seen{$key} instances\n";
		}
		return;
	} 

	$mailobj = get_mailobj_bybytes( @db );

	$mailobj->head->unfold();

	my( $contenttype );
	chomp( $contenttype = $mailobj->head->get( "Content-Type" ) );

	$contenttype =~ s/;.*//;

	$contenttype = lc( $contenttype );

	if( ! defined( $seen{$contenttype} ) ) {

		print( "Found Content-Type: \"$contenttype\"\n" );
	}

	$seen{$contenttype}++;

	return 0;
}

elog_init( "dbscanmail", @ARGV );

if ( ! &Getopts('a:vVo') || @ARGV != 2 ) { 

    	die ( "Usage: $0 [-a after] [-vo] dbname operation\n" ) ; 

} else {
	
	$dbname = shift( @ARGV );
	$operation = shift( @ARGV );
}

if( $opt_V ) {
	$opt_v++;
}

if( ! grep( /^$operation$/, @valid_operations ) ) {

	elog_die( "Operation must be one of: " . 
		  join( " ", @valid_operations ) . ". Bye.\n" );
}

if( $opt_v ) {
	elog_notify( "Starting run of operation '$operation'\n" );
}

@db = dbopen( $dbname, "r+" );

$schema = dbquery( @db, dbSCHEMA_NAME );
$schema =~ s/Mail//;

if( $schema < 1.3 ) {

	elog_die( "dbscanmail requires schema Mail1.3 or later. Bye!\n" );
}

if( $opt_o ) {

	@db = dblookup( @db, "", "out", "", "" );

} else {

	@db = dblookup( @db, "", "in", "", "" );
}

$nrecs = dbquery( @db, dbRECORD_COUNT );

$nproblems = 0;

if( $opt_a ) {

	$start = $opt_a;

} else {

	$start = 0;
}

for( $db[3] = $start; $db[3] < $nrecs; $db[3]++ ) {

	if( $db[3] % 1000 == 0 ) {

		if( $opt_v ) {

			print "Processing record $db[3]\n";
		}
	}

	if( $opt_V ) {
		elog_notify( "processing " . describe_message( @db ) . "\n" );
	}

	$nproblems += &{$operation}( @db );
}

&{$operation}();

printf( "Scanned $nrecs messages and encountered $nproblems problems\n" );
