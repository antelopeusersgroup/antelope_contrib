use Datascope ;
use Mail::Util qw( read_mbox );
use Mail::Internet;

require "getopts.pl" ;
 
sub parse_address {

	my ($address) = @_ ; 
	my( $user, $host );

	if ( $address =~ /<(\S+@\S+)>/ ) { 
		$address = $1 ;
	} elsif ( $address =~ /(\S+@\S+)/ ) { 
		$address = $1 ;
	} elsif ( $address =~ /<(\S+)>/ ) {
		$address = $1 ;
	} elsif ( $address =~ /(\S+)/ ) { 
		$address = $1 ;
	} else { 
		print STDERR "Can't parse address '$address'\n" ;
		return( "", "", "" );
	}

	$address =~ s/,.*// ;
	$address =~ s/\>$// ;

	if( $address =~ m/@/ ) {

    		($user, $host) = split ( '@', $address, 2 ) ;

		if( map { $host =~ m/$_$/ } @PreserveHosts ) {

			map { $host =~ s/.*$_$/$_/ } @PreserveHosts;

		} else {
			
    			my @parts = split ( '\.', $host ) ;
    			while ( @parts > 3 ) { 
				shift @parts ; 
    			}
    			if ( @parts == 3
				&& ( $parts[2] eq "edu" 
	    			|| $parts[2] eq "com" 
	    			|| $parts[2] eq "org"
	    			|| $parts[2] eq "gov"
	    			|| $parts[2] eq "net"
				)) { 
				shift @parts ; 
    			}

    			$host = join ('.', @parts ) ; 
		}

	} else {

		$user = $address;
		$host = $nullhost;
		$address = "$user\@$host";
	}

	$user =~ tr/[A-Z]/[a-z]/;
	$host =~ tr/[A-Z]/[a-z]/;
	$address =~ tr/[A-Z]/[a-z]/;

	return ( $user, $host, $address ) ;
}

sub get_epoch {
	my( $mailobj ) = @_;
	
	my $date = $mailobj->head->get( "Date" );
	my $mailfrom = $mailobj->head->get( "Mail-From" );

	if( $mailfrom =~ /^\s*(\S+)\s+([A-Z][a-z][a-z])\s+([A-Z][a-z][a-z])\s+(\d+)\s+(\d\d:\d\d:\d\d)\s+(\d\d\d\d)/ ) { 

		$from = $1;
		$dow = $2 ; 
		$month = $3 ; 
		$day = $4 ;
		$time = $5 ; 
		$year = $6 ; 
		$epoch = str2epoch ( "$month $day, $year $time" ) ; 

	} elsif( $date =~ m/^(?:\w{3},\s+)?(.*\d+:\d+(:\d\d)?).*/ ) {

		$date = $1;
		$epoch = str2epoch( $date );

	} else {
		print STDERR "Bad time in get_epoch: $date, $mailfrom\n";
		$epoch = 0;
	}
	
	return $epoch;
}

sub realname { 
	my ( $address ) = @_ ; 

	my( $real ) = "" ;

	if ( $address =~ /\((.*)\)/ ) { 

		$real = $1 ; 

	} elsif ( $address =~ /\s*([^<>]*)\s*<.*>/ ) { 

		$real = $1 ; 

	} else { 
		;
	}

	$real =~ s/"//g ;

	if( $real eq "" ) {
		$real = "-";
	}

	return $real ;
}

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

sub message_to_database {
	my( $dir, $dfile, $foff, $lines, $bytes, @message ) = @_;

	if( ref( $message[0] ) eq "Mail::Internet" ) {
		
		$mailobj = $message[0];

	} else {

	 	$mailobj = new Mail::Internet( \@message );	
	}

 	$mailobj->head->unfold();
 
 	$from = $mailobj->head->get( "From" );
	$real = realname( $from );
 
 	( $user, $host, $address ) = parse_address( $from );
 
	$to = $mailobj->head->get( "To" );
	if( defined( $to ) && $to ne "" ) {
		$to = ( parse_address( "$to" ) )[2];
	}

	if( map { $address =~ m/$_/ } ( @Me ) ) {

		$sent++;
	} else { 
		$sent = 0;
	} 

	chomp( $subject = $mailobj->head->get( "Subject" ) );

 	$epoch = get_epoch( $mailobj );
 
 	if( $address eq "" || $epoch == 0  ) {
 		
 		die( "Problem parsing message $from at $epoch, goodbye\n" ); 
	}

 	if( $opt_v ) {
 		printf( "Adding to database mail from $from at $epoch: $foff $lines $bytes $dir $dfile\n" );
	}

	if( $sent ) {
		$dbout[3] = dbaddnull( @dbout );
		dbputv( @dbout,  "to", $to, 
				"subject", $subject, 
				"time", $epoch, 
				"lines", $lines,
				"bytes", $bytes,
				"foff", $foff,
				"dir", $dir, 
				"dfile", $dfile );
	} else {
		$dbin[3] = dbaddnull( @dbin );
		dbputv( @dbin,  "from", $address, 
				"subject", $subject, 
				"time", $epoch, 
				"lines", $lines,
				"bytes", $bytes,
				"foff", $foff,
				"dir", $dir, 
				"dfile", $dfile );

		if( ( $dbcorr[3] = dbfind( @dbcorr, "from == \"$address\"" ) ) < 0 ) {
			dbaddv( @dbcorr, "from", $address,
			 	"descrip", $real, 
				"realname", $real );
		} else {
			$oldreal = dbgetv( @dbcorr, "descrip" );
			if( length( $real ) > length( $oldreal ) ) {
				dbputv( @dbcorr, "descrip", $real );
			}
		}
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

				message_to_database( $dir, $dfile, $startmsg_foff, $lines, $bytes, @message );
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

		message_to_database( $dir, $dfile, $startmsg_foff, $lines, $bytes, @message );
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

		( $user, $host, $address ) = parse_address( $from );

		$epoch = get_epoch( $mailobj );

		if( $opt_v ) {
			printf( "Splitting mail from $from at $epoch\n" );
		}

		if( $address eq "" || $epoch == 0  ) {
			
			$filename = sprintf( "%s/PROBLEM_%03d", 
					$Splitdir, ++$Problem );
		} else {
			$filename = "$Splitdir/$epoch-$address";
		}

		if( $opt_n ) {
			print "$filename\n";
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

sub filemail {
	my( $file ) = @_;

	my( $sent ) = 0;

	open( IN, "$file" );
	$mailobj = new Mail::Internet( \*IN );
	close( IN );
	
	chomp( $subject = $mailobj->head->get( "Subject" ) );
	$epoch = get_epoch( $mailobj );
	$year = epoch2str( $epoch, "%Y" );
	$sent_relpath = epoch2str( $epoch, $sent_archive_pattern );

	$from = $mailobj->head->get( "From" );
	( $user, $host, $address ) = parse_address( $from );

	if( $address eq "" || $epoch == 0 ) {

		$dir = "$Archivedir";
		$dfile = "FormatProblems";

	} elsif( map { $subject =~ m/$_/ } keys( %Subjects ) ) {

		$dir = "$Archivedir/$year/$host";
		foreach $exp ( keys( %Subjects ) ) { 
			if( $subject =~ m/$exp/ ) {
				$dfile = $Subjects{$exp};
				last;
			}
		}

	} elsif( map { $address =~ m/$_/ } ( @Me ) ) {

		( $dir, $base, $suffix ) = 
			parsepath( "$Archivedir/$sent_relpath" );
		
		if( ( ! defined( $suffix ) ) || $suffix eq "" ) {
			$dfile = $base;
		} else {
			$dfile = "$base.$suffix";
		} 

	} else {

		$dir = "$Archivedir/$year/$host";
		$dfile = $user;
	}

	if( ! -e "$dir" ) {
		$status = system( "mkdir -p $dir" );
		if( $status ) {
			fprintf( STDERR, "Problem with $file\n" );
		}
	}

	if( ( ! -e "$dir/$dfile" ) || -z "$dir/$dfile" ) {

		$foff = 0;
		$startline = 0;

	} else {

		chomp( $parts = `wc $dir/$dfile` );
		$parts =~ s/^\s*//;
		( $startline, $foff ) = ( split( /\s+/, $parts ) )[0,2];
	}

	chmod 0644, "$dir/$dfile";
	# Put the old-style 'From' header back in
	open( OUT, "|sed -e 's/^Mail-From: /From /' >>$dir/$dfile" );
	$mailobj->print( \*OUT );
	close( OUT );
	chmod 0444, "$dir/$dfile";

	chomp( $new_parts = `wc $dir/$dfile` );
	$new_parts =~ s/^\s*//;
	($new_startline, $new_foff ) = ( split( /\s+/, $new_parts ) )[0,2];

	$bytes = $new_foff - $foff;
	$lines = $new_startline - $startline;

	$dir = abspath( $dir );

	message_to_database( $dir, $dfile, $foff, $lines, $bytes, $mailobj );
}

$do_split = 0;
$do_file = 0;
$do_sort = 0;
$do_database = 0;

elog_init( "filemail", $ARGV );

if ( ! &Getopts('aunfvsS:d:') ) { 

    	die ( "Usage: $0 [-anufsv] [-S sortedfile] [-d new_database] [mail_file ...]\n" ) ; 

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
$Archivedir = pfget( $Pf, "archivedir" );

$nullhost = pfget( $Pf, "Hosts{NULL}" );
@PreserveHosts = @{pfget( $Pf, "Hosts{Preserve}" )};
@Me = @{pfget( $Pf, "Me" )};
%Subjects = %{pfget( $Pf, "Subjects" )};
$sent_archive_pattern = pfget( $Pf, "sent_archive_pattern" );

#Keep the `From ' fields to use that date if the Date tag is bad:
Mail::Header->mail_from( COERCE );

if( ! $opt_n && ! $opt_d ) {
	mkdir $Splitdir, 0755 || die( "Can't make directory $Splitdir\n" );
}

if( $do_database ) {

	$mail_dbname = $opt_d;

	if( ! -e "$mail_dbname" ) {
		open( D, ">$mail_dbname" );
		print D "#\nschema Mail1.1\n";
		close( D );
	}

	@db = dbopen( $mail_dbname, "r+" );
	@dbcorr = dblookup( @db, "", "correspondents", "", "" );
	@dbin = dblookup( @db, "", "in", "", "" );
	@dbout = dblookup( @db, "", "out", "", "" );

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
		print D "#\nschema Mail1.1\n";
		close( D );
	}

	@db = dbopen( $mail_dbname, "r+" );
	@dbcorr = dblookup( @db, "", "correspondents", "", "" );
	@dbin = dblookup( @db, "", "in", "", "" );
	@dbout = dblookup( @db, "", "out", "", "" );
}

@contents = sort bytime glob( "$Splitdir/*" );

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

	foreach $file ( sort bytime glob( "$Splitdir/*" ) ) {

		if( $opt_v ) { 
			printf( "Filing $file\n" );
		}

		filemail( $file );
	}

}

if( $do_sort ) {

	foreach $file ( sort bytime glob( "$Splitdir/*" ) ) {
		
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
						print "Skipping $file (matches $possible_match)\n";
					}
					next MESSAGE;
				}
			}
		}

		if( $opt_v ) {
			print "Adding $file\n";
		}

		system( "sed -e 's/^Mail-From: /From /' $file >> $sortedfile" );
	}

	chmod 0444, "$sortedfile";
}
