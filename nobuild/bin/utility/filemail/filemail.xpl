#
# filemail
# Kent Lindquist
# Lindquist Consulting
# 2003-2004
# 
# Based on an original by Dan Quinlan, BRTT Inc
#

use Mail::Util qw( read_mbox );
use Mail::Internet;

use Datascope ;
use filemail;

use Getopt::Std ;

sub bycopy {

	$a =~ m@.*/[0-9.]+-(.*[^+])(\+*)$@;
	$aname = $1;
	$aplusses = $2;
	
	$b =~ m@.*/[0-9.]+-(.*[^+])(\+*)$@;
	$bname = $1;
	$bplusses = $2;

	if( ( $sorthash{$a} <=> $sorthash{$b} ) != 0 ) {

		return $sorthash{$a} <=> $sorthash{$b};

	} elsif( ( $aname cmp $bname ) != 0 ) { 

		return $aname cmp $bname;

	} else {

		# Intentionally put the ones with more plusses first:
		return length( $bplusses ) <=> length( $aplusses );
	}
}

sub redo_database {
	my( $mfile ) = @_;

	( $dir, $base, $suffix ) = parsepath( $mfile );

	$dir = abspath( $dir );

	if( ( ! defined( $suffix ) ) || $suffix eq "" ) {
		$dfile = $base;
	} else {
		$dfile = "$base.$suffix";
	} 

	open( MFILE, "< $mfile" );

	$startmsg_foff = $foff = 0;
	$bytes = 0;
	$lines = 0;
	@message = ();

	while( <MFILE> ) {

		if( /\AFrom .*\d{4}/ ) {

			if( @message ) {

				message_to_database( @db, $mfile, $dir, $dfile, $startmsg_foff,
						     $lines, $bytes, @message );
			} 

			@message = ();
			$bytes = 0;
			$lines = 0;
			$startmsg_foff = $foff;

		}

		push( @message, $_ );

		$bytes += length( $_ );
		$lines++;

		$foff = tell MFILE;	
	}

	if( @message ) {

		message_to_database( @db, $mfile, $dir, $dfile, $startmsg_foff, $lines, $bytes, @message );
	}

	close( MFILE );
}

sub splitmail {
	my( $mfile ) = @_;

	my @msgs = read_mbox( $mfile );

	MAILMSG: 
	foreach $msg ( @msgs ) {
		
		@saved = @{$msg};
		grep( s/^From /Mail-From: /, @saved );

		$mailobj = new Mail::Internet( $msg );	
		$mailobj->head->unfold();

		$from = $mailobj->head->get( "From" );

		if( ! defined( $from ) || $from eq "" ) {
			
			$from = $mailobj->head->get( "Mail-From" );
		} 

		( $user, $host, $address ) = parse_address( $from );

		$epoch = get_epoch( $mailobj );

		if( $address eq "" || $epoch == 0  ) {
			
			$filename = sprintf( "%s/PROBLEM_%03d", 
					$Splitdir, ++$Problem );
		} else {
			$filename = "$Splitdir/$epoch-$address";
		}

		if( $opt_v ) {

			if( $opt_n ) {

				printf( "Would split " );

			} else {

				printf( "Splitting " );
			}

			printf( "mail from $from at $epoch to $filename\n" );
		}

		if( $opt_n ) {
			
			next MAILMSG;
		}
		
		unless( $opt_u || $filename =~ /^PROBLEM.*/ ) {
			while( -e $filename ) {
				$filename .= "+";
			} 
		}
		open( MSG, ">$filename" );
		$mailobj->print( \*MSG );
		close( MSG ); 
	}
}

sub bytime {

	$a =~ m@.*/(\d+)-[^/]*$@ && ( $atime = $1 );
	$b =~ m@.*/(\d+)-[^/]*$@ && ( $btime = $1 );

	return $atime <=> $btime;
}

sub sorted_bytime {

	return sort bytime glob( "$Splitdir/*" );
}

sub sorted_bytime_unique {

	my( $timestamp, $possible_match, %sorthash, @files );

	foreach $file ( sorted_bytime() ) {
		
		$file =~ m@.*/([0-9.]+)-.*@;
		$timestamp = $1;
		$sorthash{$file} = $timestamp;
	}

	MESSAGE:
	foreach $file ( sort bycopy keys( %sorthash ) ) {
		
		if( $file =~ /\+$/ ) {
			$possible_match = $file;
			while( $possible_match =~ s/\+$// ) {
				if( -e "$possible_match" && 
				    (0 == system( "diff $file $possible_match > /dev/null 2>&1" ) ) ) {
					
					if( $opt_v ) {
						elog_notify( "Skipping $file (matches $possible_match)\n" );
					}
					next MESSAGE;
				}
			}
		}

		push( @files, $file );
	}

	return @files;
}


$do_split = 0;
$do_file = 0;
$do_sort = 0;
$do_database = 0;

if ( ! getopts('aunfvsS:d:l:') ) { 

    	die ( "Usage: $0 [-anufsv] [-S sortedfile] [-l error_logfile] [-d new_database] [mail_file ...]\n" ) ; 

} 

unless( $opt_a ) {

	$reply = askyn( "\n\n\tDo you take full and unconditional responsibility\n" .
		      "\tfor what happens to your email, and assent fully\n" .
		      "\tand unconditionally to the terms of the filemail\n" .
		      "\tman page? " );

	if( ! $reply ) {
		
		die( "\n\nYou do not have permission to execute " .
		     "the filemail program. Bye.\n\n" );
	}
}

if( $opt_l ) {

	$ENV{ELOG_DELIVER} = "stderr " . abspath( $opt_l ) . "\@ncdf";
}

elog_init( "filemail", @ARGV );

if( $opt_v ) {
	
	if( $opt_l ) {

		elog_notify( "Logging errors to $opt_l\n" );
	}

	$filemail::Verbose++;
}

if( $opt_n ) {

	$filemail::Dryrun++;
}

if( ( $opt_f && $opt_s ) ||
    ( $opt_f && $opt_S ) || 
    ( $opt_s && $opt_S ) ) {

	die( "filemail: -f, -s, and -S are mutually exclusive\n" );

} elsif( $opt_f ) {

	$do_file++;

} elsif( $opt_s ) {
	
	$do_split++;

} elsif( $opt_S ) {
	
	$do_split++;
	$do_sort++;

} elsif( $opt_d ) {

	$do_database++;

}  else {
	
	$do_split++;
	$do_file++;
}

if( $opt_d && $#ARGV < 0 ) {

	die( "filemail: Must specify an input filename when using -d\n" );
}

if( $opt_S && $#ARGV < 0 ) {

	die( "filemail: Must specify an input filename when using -S\n" );

} elsif( $opt_S && -e "$opt_S" ) {

	die( "filemail: $opt_S already exists; will not overwrite\n" );

} else {

	$sortedfile = $opt_S;
}

$Pf = "filemail";
$Splitdir = pfget( $Pf, "splitdir" );
$Fileddir = pfget( $Pf, "fileddir" );
$Archivedir = pfget( $Pf, "archivedir" );

$schema = $filemail::Schema;

$filemail::Nullhost = pfget( $Pf, "Hosts{NULL}" );
@filemail::PreserveHosts = @{pfget( $Pf, "Hosts{Preserve}" )};
@filemail::Me = @{pfget( $Pf, "Me" )};
%filemail::Subjects = %{pfget( $Pf, "Subjects" )};
$filemail::Sent_archive_pattern = pfget( $Pf, "sent_archive_pattern" );

if( ! $opt_n && ! $opt_d ) {
	mkdir $Splitdir, 0755 || die( "Can't make directory $Splitdir\n" );
}

if( ! $opt_s && ! $opt_n && ! $opt_d ) {
	mkdir $Fileddir, 0755 || die( "Can't make directory $Fileddir\n" );
}

if( $do_database ) {

	$mail_dbname = $opt_d;

	if( ! -e "$mail_dbname" ) {
		open( D, ">$mail_dbname" );
		print D "#\nschema $schema\n";
		close( D );
	}

	@db = dbopen( $mail_dbname, "r+" );

	if( dbquery( @db, dbSCHEMA_NAME ) ne $schema ) {

		elog_die( "Please upgrade $mail_dbname to schema " .
			  "version $schema\n" );
	}

	foreach $file ( @ARGV ) {

		redo_database( $file );
	}

} else {

	$mail_dbname = pfget( $Pf, "mail_database" );
	chomp( $dbdir = `dirname $mail_dbname` );
	if( ! -e "$dbdir" ) {
		system( "mkdir -p $dbdir" );
	}
	if( ! -e "$mail_dbname" ) {
		open( D, ">$mail_dbname" );
		print D "#\nschema $schema\n";
		close( D );
	}

	@db = dbopen( $mail_dbname, "r+" );

	if( dbquery( @db, dbSCHEMA_NAME ) ne $schema ) {

		elog_die( "Please upgrade $mail_dbname to schema " .
			  "version $schema\n" );
	}
}

@contents = sorted_bytime();

if( $do_split && scalar( @contents ) > 0 ) {
	
	$dir_descrip = $Splitdir;
	if( $dir_descrip !~ m@^[/.]@ ) {
		$dir_descrip = "./$dir_descrip";
	}

	die( "Please clean out the directory $dir_descrip before running\n" );
}

if( $do_split ) {

	foreach $file ( @ARGV ) {

		splitmail( $file );
	}
}

if( $do_file ) {

	foreach $file ( sorted_bytime_unique() ) {

		filemail( $file, $Archivedir, @db );

		system( "/bin/mv $file $Fileddir" );
	}
}

if( $do_sort ) {

	foreach $file ( sorted_bytime_unique() ) {
		
		if( $opt_v ) {
			print "Adding $file\n";
		}

		system( "sed -e 's/^Mail-From: /From /' $file >> $sortedfile" );
	}

	chmod 0444, "$sortedfile";
}
