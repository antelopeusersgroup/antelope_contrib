use Getopt::Std ;
use Datascope;
use orb;
use strict;
use warnings;
 
# Kent Lindquist
# Lindquist Consulting
# orbpf2db
# July, 2003

our( $opt_v, $opt_V, $opt_s, $opt_p, $opt_m, $opt_f, $opt_w );
our( $orbname, $dbname, $Pf, $match, $write_mode );
our( $pfmin, %trans );
our( @db, $orb, $stop );
our( $pktid, $srcname, $time, $packet, $nbytes, $result, $pkt );
our( $key, $schema, %version, %clean, @dbscratch, @dbtable );
our( %fieldmap, @matchfields, $hookname, @records );

sub database_prep {
	my( $wantschema ) = pop( @_ );
	my( $dbname ) = pop( @_ );

	my( $haveschema );

	if( ! defined( @db ) ) { 

		if( -e "$dbname" ) {
			
			@db = dbopen( $dbname, "r+" );

			$haveschema = dbquery( @db, dbSCHEMA_NAME );

			if( $haveschema ne $wantschema ) {

				elog_complain( "Wrong schema in '$dbname' for current packet " .
					       "(have $haveschema, want $wantschema)\n" );
				return 0;

			} else {

				return 1;
			}

		} else {
			
			dbcreate( $dbname, $wantschema );

			@db = dbopen( $dbname, "r+" );
		}
		
	} else {

		$haveschema = dbquery( @db, dbSCHEMA_NAME );

		if( $haveschema ne $wantschema ) {

			elog_complain( "Wrong schema in '$dbname' for current packet " .
				       "(have $haveschema, want $wantschema)\n" );
			return 0;

		} else {

			return 1;
		}
	}
}

sub reopen_database {

	my( $dbname ) = dbquery( @db, dbDATABASE_NAME );

	dbclose( @db );

	system( "dbcrunch $dbname" );

	@db = dbopen( $dbname, "r+" );

	return;
}

$Pf = "orbpf2db.pf";
$match = ".*/pf/orbstat";
$write_mode = "overwrite";
$pktid = 0;
$time = -9999999999.999;
$pfmin = 1233000000.000;

if ( ! getopts('s:w:f:p:m:vV') || @ARGV != 2 ) { 

    	die ( "Usage: orbpf2db [-vV] [-p pffile] [-s statefile] [-m match] [-f from] [-w mode] orb database\n" ) ; 

} else {
	
	$orbname = $ARGV[0];
	$dbname = $ARGV[1];
}

elog_init( $0, @ARGV );

if( $opt_V ) {
	
	$opt_v++;
}

if( $opt_p ) {
	
	$Pf = $opt_p;
}

my( $rc ) = pfrequire( $Pf, $pfmin );

if( $rc == -2 ) {
	
	elog_die( "Parameter-file '$Pf' is too old; please update it \n" );

} elsif( $rc < 0 ) {
	
	elog_die( "Error $rc from pfrequire(). Please see pfrequire(3). Bye.\n" );
}

%trans = %{pfget( $Pf, "translations" )};

if( $opt_m ) {
	
	$match = $opt_m;
}

if( $opt_w ) {

	if( $opt_w =~ /^(overwrite|add)$/ ) {
		
		$write_mode = $opt_w;

	} else {
		
		die( "-w mode must be 'overwrite' or 'add'\n" );
	}
}

$orb = orbopen( $orbname, "r&" );

if( $orb < 0 ) {

	die( "Failed to open orb '$orbname' for reading\n" );
}

orbselect( $orb, $match );

if( $opt_f && ( ! $opt_s || ! -e "$opt_s" ) ) {
	
	$pktid = orbposition( $orb, $opt_f );

	if( $opt_v ) {
		elog_notify( "Positioned to packet $pktid\n" );
	}

} elsif( $opt_f ) {

	elog_complain( "Ignoring -f in favor of existing state-file\n" );
}

$stop = 0;

if( $opt_s ) {

	exhume( $opt_s, \$stop, 15 );
	orbresurrect( $orb, \$pktid, \$time  );
	orbseek( $orb, "$pktid" );
}

for( ; $stop == 0 ; ) {

	($pktid, $srcname, $time, $packet, $nbytes) = orbreap( $orb );

#DEBUGGING CODE:
#	($pktid, $srcname, $time, $packet, $nbytes) = 
#				orbreap_timeout( $orb, $DEBUG_timeout );
#
#	if( ! defined( $pktid ) ) {
#		
#		elog_complain( "FYI orbreap timed out at $DEBUG_timeout seconds\n" );
#		next;
##	}

	if( $opt_s ) {

		bury();
	}

	($result, $pkt) = unstuffPkt( $srcname, $time, $packet, $nbytes ); 

	if( $result ne "Pkt_pf" ) {
		if( $opt_v ) {
			elog_notify( "Received a $result, skipping\n" );
		}
		next;
	}

	if( $opt_v ) {

		print "Received a parameter-file '$srcname' at " .
  			strtime( $time );

		if( $opt_V ) {
			print ":\n" . pf2string( $pkt->pf ) . "\n\n";
		} else {
			print "\n";
		}
	}

	( $key ) = grep( "$srcname", keys( %trans ) );
	
	next if( ! defined( $key ) );

	$schema = $trans{$key}{"schema"};

	next unless database_prep( $dbname, $schema );

	%version = %{$trans{$key}{"version"}};

	if( defined( $version{"version_field"} ) &&
	    $version{"version_field"} ne "" ) {

		my( $version_min ) = $version{"version_min"};

		my( $packet_version ) = pfget( $pkt->pf, $version{"version_field"} );

		if( ! defined( $packet_version ) ) {

			elog_complain( "No field '$version{version_field}' in " .
				"packet '$srcname' timestamped " . strtime( $time ) . 
				" ; Skipping\n\n" );
			next;

		} elsif( $packet_version < $version_min ) {

			elog_complain( "field '$version{version_field}' is less than " .
				       "minimum allowed value '$version_min' in " .
					"packet '$srcname' timestamped " . strtime( $time ) . 
					" ; Skipping\n\n" );
			next;
		}
	}

	%clean = %{$trans{$key}{"clean"}};

	my( $cleantable );

	foreach $cleantable ( keys( %clean ) ) {

		@dbscratch = dblookup( @db, "", "$cleantable", "", "dbSCRATCH" );
		@dbtable = dblookup( @db, "", "$cleantable", "", "dbALL" );

		%fieldmap = %{$trans{$key}{"clean"}{$cleantable}};

		@matchfields = ();

		my( $field, $pattern, $value );

		foreach $field ( keys( %fieldmap ) ) {

			$pattern = $fieldmap{$field};

			if( $pattern =~ /^TIME:/ ) {

				$pattern =~ s/^TIME://;
				$value = pfget( $pkt->pf, $pattern );
				$value =~ s/^\s*([\d\-.]+).*/$1/;

			} elsif( $pattern =~ /^BOOLEAN:/ ) {

				$pattern =~ s/^BOOLEAN://;
				$value = pfget_boolean( $pkt->pf, $pattern );

			} elsif( $pattern =~ /^SRCNAME$/ ) {

				$value = $srcname;

			} else {
		
				$value = pfget( $pkt->pf, $pattern );
			}

			dbputv( @dbscratch, "$field", $value );

			push( @matchfields, $field );
		}

		$hookname = "hook_clean_$dbtable[0]_$cleantable";
		@records = dbmatches( @dbscratch, @dbtable, 
						$hookname, @matchfields  );

		my( $record );

		foreach $record ( @records ) {
			
			$dbtable[3] = $record;

			dbmark( @dbtable );
		}

	}

	reopen_database();

	my( %tables ) = %{$trans{$key}{"tables"}};

	my( $table );

	foreach $table ( keys( %tables ) ) {

		@dbscratch = dblookup( @db, "", "$table", "", "dbNULL" );
		dbget( @dbscratch, 0 );
		@dbscratch = dblookup( @db, "", "$table", "", "dbSCRATCH" );

		@dbtable = dblookup( @db, "", "$table", "", "dbALL" );

		%fieldmap = %{$trans{$key}{"tables"}{$table}};

		if( defined( $fieldmap{FOREACH} ) ) {

			my( $structref ) = pfget( $pkt->pf, $fieldmap{FOREACH} );

			next if( ! defined( $structref ) );

			my( @mykeys );

			if( ref( $structref ) eq "HASH" ) {

				my( %arrays ) = %$structref;
				@mykeys = keys( %arrays );

			} elsif( ref( $structref ) eq "ARRAY" ) {

				@mykeys = 0..$#{$structref};

			} else {
				elog_complain( "Problem with FOREACH " .
					       " for table '$table'\n" );
				next;
			}

			my( $key, $field, $value, $pattern );

			foreach $key ( @mykeys ) {

				foreach $field ( keys( %fieldmap ) ) {

					next if( $field eq "FOREACH" );

					$pattern = $fieldmap{$field};
					
					if( $pattern =~ /^FOREACH$/ ) {
						
						$value = $key;

					} elsif( $pattern =~ /^TIME:/ ) {

						$pattern =~ s/FOREACH/$key/;
						$pattern =~ s/^TIME://;
						$value = pfget( $pkt->pf, $pattern );
						$value =~ s/^\s*([\d\-.]+).*/$1/;

					} elsif( $pattern =~ /^BOOLEAN:/ ) {

						$pattern =~ s/FOREACH/$key/;
						$pattern =~ s/^BOOLEAN://;
						$value = pfget_boolean( $pkt->pf, $pattern );

					} elsif( $pattern =~ /^SRCNAME$/ ) {

						$value = $srcname;

					} else {

						$pattern =~ s/FOREACH/$key/;
						$value = pfget( $pkt->pf, $pattern );
					}

					if( defined( $field ) && 
					    defined( $value ) ) {

						dbputv( @dbscratch, "$field", $value );
					}
				}

				if( $write_mode eq "add" ) {
	
					dbadd( @dbtable );

				} elsif( $write_mode eq "overwrite" ) {
	
					$hookname = "hook__$dbtable[0]_$table";	
					@matchfields = dbquery( @dbtable, dbPRIMARY_KEY );
					@records = dbmatches( @dbscratch, @dbtable, $hookname, @matchfields );	

					my( $recno );

					@records = sort {$a <=> $b} @records;
					if( ! defined( @records ) || 
					    scalar( @records ) < 1 ) {
	
						dbadd( @dbtable );
	
					} else {

						$recno = shift( @records );
	
						my( @dbreplace ) = @dbtable;
						$dbreplace[3] = $recno;
						dbput( @dbreplace );
					}
				}
			}

		} else {
			
			my( $field, $pattern, $value );

			foreach $field ( keys( %fieldmap ) ) {

				$pattern = $fieldmap{$field};

				if( $pattern =~ /^TIME:/ ) {

					$pattern =~ s/^TIME://;
					$value = pfget( $pkt->pf, $pattern );
					$value =~ s/^\s*([\d\-.]+).*/$1/;

				} elsif( $pattern =~ /^BOOLEAN:/ ) {

					$pattern =~ s/^BOOLEAN://;
					$value = pfget_boolean( $pkt->pf, $pattern );

				} elsif( $pattern =~ /^SRCNAME$/ ) {

					$value = $srcname;

				} else {
			
					$value = pfget( $pkt->pf, $pattern );
				}

				dbputv( @dbscratch, "$field", $value );
			}

			if( $write_mode eq "add" ) {
				
				dbadd( @dbtable );
			
			} elsif( $write_mode eq "overwrite" ) {

				$hookname = "hook_$dbtable[0]_$table";
				@matchfields = dbquery( @dbtable, dbPRIMARY_KEY );
				@records = dbmatches( @dbscratch, @dbtable, $hookname, @matchfields );

				if( ! defined( @records ) || scalar( @records ) < 1 ) {
	
					dbadd( @dbtable );
	
				} else {

					@records = sort {$a <=> $b} @records;
					my( @dbreplace ) = @dbtable;
					$dbreplace[3] = shift( @records );
					dbput( @dbreplace );
				}
			}
		}
	}
}
