##############################################################################
# Author: Glenn Thompson (GT) 2007
#         ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#       2007-11-08: Added header
#
# Purpose:
#
#       See man page
#
# To do:
#	* Optionally include netmag, stamag and origerr tables in packets
#       * Optionally replay an old database at a specified rate (-r, where r=1 is real-time, r=100 is 100 times real-time, r=0 means ignore this option)
#	* Optionally add a parameter to restart from the beginning (DONE: 20080207 - just put -l 0)
#	* Optionally allow origins without magnitudes (DONE: 20080207 - just put -a)
#
# Changes by Anna Bulanova (March - June 2009):
#
# 1) Changed the run_event function: 
# I replaced the exception handling from Error.pm with a regular perl's eval functions.
# Check if there are arrival, assoc, event record for the current orid. If not, don't send anything
# 2) I commented out 'max_records_to_process', since we may want to process more than 10 records.
# Now maximal number of records is 9999
# 3) Added an extra 'do -- while' loop to make sure that the program reconnects to the database after processing 10 records.
# This helps with memory leaks.
# It seems that antelope's perl database functions don't release or reuse memory until we reconnect to the database.
# So, it asks for more memory from the system for each new record 
# After reconnecting, the same memory can be used again. I am hoping that by processing no more than 10 records, memory usage will be kept under 100Mb.
# If we try to process 700 records without reconnecting, it eats up more than 1 GB. 
# I think that memory leaks caused the freeze problem -- at least it does not happen anymore.
# 4) Added -z option for use with dbmoment database. Only event, origin, and netmag tables are processed. 
#
#
##############################################################################

use Datascope;
use orb;
require "getopts.pl" ;

use strict;
use warnings;
#use lib "/home/glenn/perlmod/lib/site_perl/5.8.0";
#use Error qw(:try);
our $PROG_NAME;
($PROG_NAME = $0) =~ s(.*/)();  # PROG_NAME becomes $0 minus any path

# Usage - command line options and arguments
our ($opt_a, $opt_d, $opt_i, $opt_l, $opt_m, $opt_n, $opt_o, $opt_r, $opt_s, $opt_t, $opt_v, $opt_z); 
if ( ! &Getopts('ad:il:m:n:o:r:s:t:vz') || $#ARGV < 2 ) {
	print STDERR <<"EOU" ;
	
	Usage: $PROG_NAME [-a] [-d days] [-i] [-l last_lddate] [-m magnitude_threshold] [-n number_of_associations_threshold] [-o oldest_acceptable_origin_in-minutes] [-r rate] [-s sleep] [-t time_to_wait_for_magnitude] [-v] [-z] database last_lddate_file orb [orb2 [orb3 ...]]
	
	Also see manpage.
		
EOU
	exit 1 ;
}
$opt_d = -1  unless defined($opt_d);
$opt_l = -1 unless defined($opt_l);


# End of  GT Antelope Perl header
##############################################################################

#use File::Copy;
use File::stat;
use English;

# cannot handle full path names to orb2dbt.pf file
# so outfile MUST be orb2dbt.pf 

my $database = shift @ARGV;  # database name
my $ldfile  = shift @ARGV; # last_lddate file
my @orb = @ARGV;

# minimum magnitude needed
my $minmag = $opt_m ? $opt_m : 0.0 ;

# number of associated arrivals needed
my $minnass = $opt_n ? $opt_n : 4 ;

my $oldest_origin_in_minutes = $opt_o ? $opt_o : 999999;

# rate - if non-zero it invokes replay mode which is designed to replay an old database at r times the real-time rate
my $rate = $opt_r ? $opt_r : 0;

# update time in seconds
my $sleep = $opt_s ? $opt_s : 10 ; # how long to wait between checks

# time to wait in seconds for a magnitude to arrive
my $timetowait = $opt_t ? $opt_t : 60;
our @dbtables;
if(!defined($opt_z)){
	@dbtables = ("arrival", "assoc", "event", "origin") ;
	push @dbtables, qw(netmag stamag origerr) if ($opt_i);
} else {
	@dbtables = ("event", "origin", "netmag") ;
	#push @dbtables, qw(netmag) if ($opt_i);
}

my $epochnow = str2epoch("now");
my $timestrnow = strydtime( $epochnow ); # epochtime now
print STDERR "\n###########  $PROG_NAME $timestrnow ##############\n\n";

foreach my $dbtable (@dbtables){
	if ( ! -e "$database.$dbtable" ) {
		die("No $dbtable table present for $database !\n");
	}
}

my @db      = dbopen ( $database, "r" ) ;
my @dbor    = dblookup (@db, "", "origin", "", "" ) ;
my $or_file = dbquery (@dbor,"dbTABLE_FILENAME"); # filename of origin table
my $nstart  = dbquery ( @dbor, "dbRECORD_COUNT") ; # number of records in table at program initiation
my $new_lddate = -1;
my $last_lddate  = -1;

# process most recent modified row of $opt_d not specified 
if ($opt_l >= 0) {
	print STDERR "-d switch ignored as -l switch overrides this\n" if ($opt_d > -1);
	eval{ # try to parse time string
		$last_lddate = str2epoch($opt_l);
	};
	if($@){ # must be epoch format
		$last_lddate = $opt_l;
	}
	&write_lastlddate($last_lddate,$ldfile);
	print STDERR "opt_l given, last_lddate = $last_lddate\n" if $opt_v;
}
elsif($opt_d >= 0) {
	my $secsperday = 60 * 60 * 24;
	$last_lddate = now() - ($opt_d * $secsperday);
	&write_lastlddate($last_lddate,$ldfile);
	print STDERR "no opt_l, opt_d given, last_lddate = $last_lddate\n" if $opt_v;
	
} else {
	print "no opt_l, if opt_d given it was set to 0\n" if $opt_v;
	
	if (-e $ldfile) {
		open(FIN,$ldfile);
		$last_lddate = <FIN>;
		close(FIN);
		print STDERR "- ldfile exists, last_lddate = $last_lddate\n" if $opt_v;
		
	} 
	else 
	{	
		# Just go to end of database - same as -d 0
		$last_lddate = now();
		&write_lastlddate($last_lddate,$ldfile);
		print STDERR "- no ldfile, getting now last_lddate = $last_lddate\n" if $opt_v;
		
	} 		
} 
dbclose(@db);

printf STDERR "\n\nStarting with last_lddate = %s ($last_lddate)\n",strtime($last_lddate) ; 
print STDERR "database $database starting with $nstart origin records\n" ; 
my $inode_start      = stat("$or_file");

# force first run at startup after sleeping
my $mtime_start      = 0 ;
my $max_records_to_process = 9999; # first time through only, this is a large number, thereafter it is 10



my $first_origin = 1;
my $prev_date = $last_lddate;
# Sit and wait for new origin db rows
my $last_rec=0;
for (;;) {
	sleep $sleep if ( $mtime_start != 0 );
	my $inode = stat("$or_file");
	my $mtime = $inode->mtime ; # when was origin table last modified?
	my $prev_rec=-1;
	if ($mtime > $mtime_start ) {
		print STDERR "\n\n *************************** \n$or_file has changed\n";
		my $nnew=-1;
		# new do--while loop
		do{
			
			@db = dbopen($database, "r");
			@dbor = dblookup(@db, "", "origin", "", "");
			$nnew = dbquery (@dbor, "dbRECORD_COUNT");
			
			if ($prev_rec==-1) { print STDERR "database $database now has $nnew origin records\n" ; }
			
			
			# start a new log file every time the database gets cut down
			if ($nnew < $nstart) {
				print STDERR "Looks like database has been tailed - starting a new log file\n";
				system("mv logs/$PROG_NAME logs/$PROG_NAME.old");
				$nstart = $nnew;
				print STDERR "Database tailed so starting new log file\n";
				print STDERR "\n\nStarting last_lddate at $last_lddate, opt_l arg is $opt_l \n" ; 
				print STDERR "database $database starting with $nstart origin records\n" ;
				$prev_rec=0;
			}	 
			
			my $start_record = ($nnew - $max_records_to_process);
			$start_record = 0 if ($start_record < 0);
			if($prev_rec>=$start_record) { $start_record=$prev_rec+1; }
			
			#printf STDERR "\n*** Starting at record %d ***\n", $start_record if $opt_v;
			my $new_lddate=0;
			for ( $dbor[3] = $start_record ; $dbor[3] < $nnew &&  $dbor[3]<$start_record+10; $dbor[3]++ ) { # Process only 10 records
				#printf STDERR "\n\nThis is record %d of %d\n", $dbor[3], $nnew if $opt_v;
				
				$prev_rec=$dbor[3];
				#$max_records_to_process = 10 if ($max_records_to_process > 10);
				$new_lddate = dbgetv(@dbor, "lddate") ;
				my $mb = dbgetv(@dbor, "mb");
				my $ml = dbgetv(@dbor, "ml");
				my $nass = dbgetv(@dbor, "nass");
				my $orid = dbgetv(@dbor, "orid");
				my $evid = dbgetv(@dbor, "evid");
				my $auth = dbgetv(@dbor, "auth");
				my $otime = dbgetv(@dbor, "time");
				my $newrec = $dbor[3] ;
				
				#print STDERR "new_lddate = $new_lddate, last_lddate = $last_lddate\n" if $opt_v;
				if ( $new_lddate > $prev_date )  {
					
					# replay mode
					if ($rate > 0) {
						my $time_to_sleep = ($new_lddate - $last_lddate) / $rate;
						printf STDERR "Replay: sleep for %.0f seconds\n", $time_to_sleep; 
						sleep( $time_to_sleep) if ($first_origin == 0);
					}
					$first_origin = 0;
					
					# Time criteria met
					my $timenow = str2epoch("now");
					
					print STDERR "\nNew origin $orid detected for event $evid\n";
					printf STDERR "Origin time: %s\n",strtime($otime);
					print STDERR "Author: $auth, Nass: $nass, mb: $mb, ml: $ml\n";		 		
					printf STDERR "Time now: %s,  new_lddate = %s, last_lddate = %s  ...\n",strtime( $timenow ), strtime($new_lddate), strtime($last_lddate);
					
					# GTHO: Check if origin has at least minimum number of arrivals
					if ($nass >= $minnass) {
						print STDERR "Minimum nass criteria met\n";
						
						# GTHO: If no magnitude data yet, wait if event time is within last hour
						my $origin_age_in_minutes = ($timenow - $otime) / 60;
						printf STDERR "timenow ", $timenow, " otime", $otime, " age ", $origin_age_in_minutes, $oldest_origin_in_minutes;
						if ($origin_age_in_minutes < $oldest_origin_in_minutes) {
							
							my $seconds = 0;
							my $retry_secs = 5;
							while ($ml == -999.00 && $mb == -999.00 && $seconds < $timetowait) {
								printf STDERR "Origin has no magnitude data: will wait up to another %d seconds\n",$timetowait - $seconds;
								sleep($retry_secs); $seconds +=$retry_secs; # wait X seconds between rechecks
								
								# then close, reopen and reread database (because the copy of the database each time is static - magnitude data never update as program sees a snapshot, not a dynamically changing database
								dbclose(@db);
								@db = dbopen($database, "r");
								@dbor = dblookup(@db, "", "origin", "", "");
								my $nnow = dbquery (@dbor, "dbRECORD_COUNT");
								unless ($nnow < $nnew) { # check if database tailed in the last sleep and skip this block if it was
									# should just jump to checking next record from old database, which will skip again if there are no magnitudes
									# but this is OK because last_lddate file wont be updated
									# note that if $nnow = $nnew, or $nnow > $nnew, we just assume that at worst new records have been added, 
									# and those will be picked up next time the modification time is checked.
									
									$dbor[3] = $newrec;
									$mb = dbgetv(@dbor, "mb");
									$ml = dbgetv(@dbor, "ml");
									if ($ml > -999.00 || $mb > -999.00) {	
										print STDERR "Revised ml = $ml, mb = $mb after $seconds seconds\n" ;
									}
								}
							}
						}
						
						
						# Check the minimum magnitude has been met
						if ((($opt_a) ||  ($ml >= $minmag || $mb >= $minmag))) {
							
							
							print STDERR "Magnitude criteria met\n";
							# Check that origin time criteria met
							
							if (($origin_age_in_minutes < $oldest_origin_in_minutes) || ($rate > 0) ) {
								printf STDERR "Origin age is %5.1f minutes\n",$origin_age_in_minutes;
								print STDERR "Create packet\n" ;
								
								# First send the origin, event, assoc, netmag and arrival tables
								# 20071214: added $pfout to return variables, so we can output packets
								my ($pfout,$packet) = run_event($newrec, @dbor) ;
								# if nothing to send, go to next iteration of the loop
								if ($pfout eq "") {
									print STDERR "Not sending\n" ;
									next;
								}
								print STDERR "Sending record $newrec / orid $orid ...\n" ;
								my $t = now();
								# print out the packet if in verbose mode
								#print STDERR "$packet\n" if $opt_v;
								
								# write to multiple orbs (at least one)
								my $orbname;
								foreach $orbname (@orb) {
									#print STDERR $orbname,"  "  ;
									
									my $orbptr = orbopen("$orbname", "w");
									if ( $orbptr < 0 ) { 
										die ( "Can't open $orbname\n" ) ; 
									}	
									my $nby = length($packet);
									my $pktid;
									$pktid = orbputx($orbptr, "/pf/orb2dbt", $t, $packet, $nby);
									print STDERR "packet $pktid written to $orbname with $nby bytes\n";
									orbclose($orbptr);
								}
								
								# Only want to update last_lddate file each time a new record is sent
								&write_lastlddate($new_lddate,$ldfile);
								
							}
							else
							{
								print STDERR "Record $newrec / origin $orid - time was too old\n"; 
							} # end of if origin age
							
							
						}
						else
						{
							print STDERR "Record $newrec / origin $orid did not reach the magnitude threshold (or had none defined)\n"; 
						} # end of if ml
					}
					else
					{
						printf STDERR "Failed minimum nass criteria\n";	
					} # end of if nass > minnass
					
				} # 
				else
				{
#					printf STDERR "Old record %d \n", $dbor[3];	
				} # end of new_ldate > last_lddate
				
			} # end of for $dbor
			
			dbclose(@db);
			$mtime_start = $mtime;
			#STDERR->flush;
		}while($dbor[3]<$nnew);
		print STDERR ".";
		$prev_date = $last_lddate;
		
	}
	else
	{
		#printf STDERR "$or_file has not changed\n" if $opt_v;
	}
	@db = dbopen($database, "r");
	dbclose(@db);
	
}

sub run {
	my $cmd = $_[0];
	print STDERR "$cmd\n";
	system($cmd);
}

sub run_event {
	my ($myrec, @db2) = @_ ;
	my @dborigin = dblookup(@db2, "", "origin", "", "");
	$dborigin[3] = $myrec ;
	
	my $subor = dbgetv(@dborigin, "orid") ;
	
	my $orsub = qq(orid==$subor) ;
	
	print STDERR "$orsub\n" if $opt_v;
	my @dbproc;
	eval{
	if($opt_z){
	@dbproc = dbprocess(@db2, "dbopen origin",
	"dbsubset $orsub", 
	"dbjoin event"); 
	}else{
	@dbproc = dbprocess(@db2, "dbopen origin",
	"dbsubset $orsub", 
	"dbjoin event", 
	"dbjoin assoc", 
	"dbjoin arrival") ;
	}
	};
	# if there are no records in dbproc view, then return empty lines
	if ( $@ ne "" || dbquery(@dbproc, "dbRECORD_COUNT") <= 0){ 
				print STDERR "dbprocess failed: no event, assoc, or arrival data for this origin\n";
				return ("", "");
	}
	
	
	if ($opt_i || $opt_z) {
		my @dbproc2;
		# Add origerr if its there
		@dbproc2 = dbprocess(@dbproc, 
		"dbjoin origerr");
		if (dbquery(@dbproc2, "dbRECORD_COUNT") > 0){
			(@dbproc = @dbproc2);
		}else{
		}
		
		# Add netmag if its there
		@dbproc2 = dbprocess(@dbproc, 
		"dbjoin netmag");
		if (dbquery(@dbproc2, "dbRECORD_COUNT") > 0){
			@dbproc = @dbproc2; 
		}else{
		}
		
		# Add stamag if its there
		@dbproc2 = dbprocess(@dbproc, 
		"dbjoin stamag");
		if (dbquery(@dbproc2, "dbRECORD_COUNT") > 0) {
			@dbproc = @dbproc2;
		}else{
		}
	} 
	
	my $ncrap = dbquery(@dbproc, "dbRECORD_COUNT") ;
	print STDERR " $ncrap records in dbproc\n" if $opt_v;
	
	my $pfout = "";
	foreach my $tbl (@dbtables) {
			eval {
				
				my @dbsep = dbseparate(@dbproc, "$tbl");
				if ($tbl eq "arrival") {
					$pfout .= "arrivals  &Literal{\n" ;
						@dbsep = dbsort(@dbsep, "sta", "iphase");
				} elsif ($tbl eq "assoc") {
					$pfout .= "assocs  &Literal{\n" ;
						@dbsep = dbsort(@dbsep, "sta", "phase");
				} elsif ($tbl eq "event" ) {
					$pfout .= "event  &Literal{\n";
				} elsif ($tbl eq "origin") {
					$pfout .= "origin  &Literal{\n";
				} elsif ($tbl eq "netmag") {
					$pfout .= "magnitude_update        yes\nnetmags  &Literal{\n";
				} elsif ($tbl eq "stamag") {
					$pfout .= "stamags  &Literal{\n";
				} elsif ($tbl eq "origerr") {
					$pfout .= "origerr  &Literal{\n";
				}
				my $num_sep = dbquery(@dbsep, "dbRECORD_COUNT");
				my @sep_fields = dbquery(@dbsep, "dbTABLE_FIELDS");
				my $nfields =  $#sep_fields;
				print STDERR "$tbl TABLE with $num_sep records and $nfields Fields\n" if $opt_v;
				for ($dbsep[3] = 0; $dbsep[3] < $num_sep; $dbsep[3]++) {
					my $row = "";
					foreach  my $fld (@sep_fields) {
						
						my @dbfld = dblookup(@dbsep, "", "", "$fld", "");
						my $fmt = dbquery(@dbfld, "dbFIELD_FORMAT");
						
						my $nul = dbquery(@dbfld, "dbNULL");
						my $fldval = dbgetv(@dbfld, "$fld");
						if ( ($fldval eq "") && ( (substr($fmt, -1) eq "f") || (substr($fmt, -1) eq "d") ) ) {
							$fldval = $nul;
						}	
						
						# must have space after fmt string for orb2dbt to read pf file
						eval {
							$row .= sprintf("$fmt ", $fldval);
						};
						if ( $@ ne "" ) {
							print STDERR "Error: fld = $fld, fmt = $fmt, fldval = $fldval, nul = $nul\n" if $opt_v;
						}
						
					} # end foreach
					
					chop $row ;
					print STDERR "$row\n" if $opt_v;
					$pfout .= "$row\n";
					
				} # end for
				
					$pfout .= "}\n";
					
			}; # end try
			if ( $@ ne "" ){
				print STDERR "dbseparate failed: No $tbl table for this origin\n";
			}
		
	} # end foreach
	
	my $packet = s2pfpkt($pfout);
	# 20071214: added $pfout to return variables, so we can output packets
	
	return ($pfout,$packet) ;
}


# This is the trick to get a string into packet format (see rtorbcmd)
sub s2pfpkt { 
	my ( $s ) = @_ ; 
	my $packet = chr(0) ;
	$packet .= chr(1) ;
	$packet .= $s ;
	$packet .= chr(0) ;
	return $packet ;
}

sub write_lastlddate {
	($last_lddate,$ldfile) = @_;
	print STDERR "write_lastlddate: writing $last_lddate to $ldfile\n" if $opt_v;
	if (open(FOUT,">$ldfile")) { 
		print FOUT $last_lddate;
		#print STDERR "Writing $last_lddate to $ldfile\n";
		close FOUT;
	}
	else
	{
		print STDERR "Could not write to $ldfile";
	}
	
}

