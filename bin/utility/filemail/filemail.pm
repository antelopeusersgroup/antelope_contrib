package filemail;

require Exporter;
@ISA = ('Exporter');

@EXPORT = qw( parse_address message_to_database get_epoch realname filemail );
@EXPORT_OK = qw( $Verbose $FormatProblems $Nullhost );

use Datascope ;
use Mail::Internet;

#Keep the `From ' fields to use that date if the Date tag is bad:
Mail::Header->mail_from( COERCE );

$Verbose = 0;
$Dryrun = 0;
$FormatProblems = "FormatProblems";
$Nullhost = "localhost";
@PreserveHosts = ();
@Me = ();
%Subjects = ();
$Sent_archive_pattern = "%Y/SENT/%B";

sub parse_address {
	my( $address ) = @_ ; 

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
		elog_complain("Can't parse address '$address'\n" );
		return( "", "", "" );
	}

	$address =~ s/ /_/g ;
	$address =~ s/[()'"]//g ;
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
		$host = $Nullhost;
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

	my( $from, $dow, $month, $day, $time, $year, $epoch );

	if( $mailfrom =~ /^\s*(\S+)\s+([A-Z][a-z][a-z])\s+([A-Z][a-z][a-z])\s+(\d+)\s+(\d\d:\d\d:\d\d)\s+(\d\d\d\d)/ ) { 

		$from = $1;
		$dow = $2 ; 
		$month = $3 ; 
		$day = $4 ;
		$time = $5 ; 
		$year = $6 ; 
		$epoch = str2epoch ( "$month $day, $year $time" ) ; 

	} elsif( $mailfrom =~ /^\s*(\S+)\s+([A-Z][a-z][a-z])\s+([A-Z][a-z][a-z])\s+(\d+)\s+(\d\d:\d\d)\s+[A-Z]{3,4}\s+(\d\d\d\d)/ ) { 

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

		elog_complain( "Bad time in get_epoch: $date, $mailfrom\n" );

		$epoch = 0;
	}

	if( $epoch < 0 ) {

		elog_complain( "Bad time in get_epoch: $date, $mailfrom\n" );

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

sub message_to_database {
	my( @db ) = splice( @_, 0, 4 );
	my( $name, $dir, $dfile, $foff, $lines, $bytes, @message ) = @_;

	my( @dbin ) = dblookup( @db, "", "in", "", "" );
	my( @dbout ) = dblookup( @db, "", "out", "", "" );
	my( @dbcorr ) = dblookup( @db, "", "correspondents", "", "" );

	if( ref( $message[0] ) eq "Mail::Internet" ) {
		
		$mailobj = $message[0];

	} else {

	 	$mailobj = new Mail::Internet( \@message );	
	}

 	$mailobj->head->unfold();
 
 	my( $from ) = $mailobj->head->get( "From" );
	
	if( ! defined( $from ) || $from eq "" ) {
		
		$from = $mailobj->head->get( "Mail-From" );
	}

	my( $real ) = realname( $from );
 
 	my( $user, $host, $address ) = parse_address( $from );

	if( ! defined( $address ) || $address eq "" ) {
		
		elog_complain( "Blank address in $name. " . 
			       "Not recording in database!\n" );

		return;
	}
 
	my( $to ) = $mailobj->head->get( "To" );
	if( defined( $to ) && $to ne "" ) {
		$to = ( parse_address( "$to" ) )[2];
	}

	my( $sent );
	if( map { $address =~ m/$_/ } ( @Me ) ) {

		$sent++;
	} else { 
		$sent = 0;
	} 

	chomp( $subject = $mailobj->head->get( "Subject" ) );

 	my( $epoch ) = get_epoch( $mailobj );
 
 	if( $address eq "" || $epoch == 0  ) {
 		
		elog_complain( "Problem parsing message $from at $epoch. " .
			       "Not recording in database!\n" ); 

		return;
	}

 	if( $Verbose ) {
 		elog_notify( "Adding to database mail from $from at $epoch: foff $foff lines $lines bytes $bytes $dir $dfile\n" );
	}

	my( @dbtemp ) = dblookup( @dbcorr, "", "correspondents", "from", "" );
	my( $address_size ) = dbquery( @dbtemp, dbFIELD_SIZE );

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

		my( $expr ) = "from == substr(\"$address\",0,$address_size)";

		if( ( $dbcorr[3] = dbfind( @dbcorr, $expr ) ) < 0 ) {

			dbaddv( @dbcorr, "from", $address,
			 	"descrip", $real, 
				"realname", $real );

		} else {

			my( $oldreal ) = dbgetv( @dbcorr, "descrip" );

			if( length( $real ) > length( $oldreal ) ) {

				dbputv( @dbcorr, "descrip", $real );
			}
		}
	}
}

sub filemail {
	my( $in, $archive_dir, @db ) = @_;

	my( $name );

	if( ref( $in ) eq "Mail::Internet" ) {

		$mailobj = $in;

		$name = "<Mail-object>";

	} else {

		open( IN, "$in" );
		$mailobj = new Mail::Internet( \*IN );
		close( IN );

		$name = $in;
	}
	
	my( $sent ) = 0;
	my( $epoch, $year, $sent_relpath, $from, $dir, $dfile, $subject );
	my( $bytes, $lines, $startline, $foff );

	chomp( $subject = $mailobj->head->get( "Subject" ) );
	$epoch = get_epoch( $mailobj );
	$year = epoch2str( $epoch, "%Y" );
	$sent_relpath = epoch2str( $epoch, $Sent_archive_pattern );

	$from = $mailobj->head->get( "From" );

	if( ! defined( $from ) || $from eq "" ) {
	
		$from = $mailobj->head->get( "Mail-From" );
	} 

	my( $user, $host, $address ) = parse_address( $from );

	if( $address eq "" || $epoch == 0 ) {
		
		$dir = "$archive_dir";
		$dfile = $FormatProblems;

		elog_complain( "Blank address in $name! " .
			       "Filing in $dir/$dfile.\n" );
 
	} elsif( map { $subject =~ m/$_/ } keys( %Subjects ) ) {

		$dir = "$archive_dir/$year/$host";
		foreach $exp ( keys( %Subjects ) ) { 
			if( $subject =~ m/$exp/ ) {
				$dfile = $Subjects{$exp};
				last;
			}
		}

	} elsif( map { $address =~ m/$_/ } ( @Me ) ) {

		( $dir, $base, $suffix ) = 
			parsepath( "$archive_dir/$sent_relpath" );
		
		if( ( ! defined( $suffix ) ) || $suffix eq "" ) {
			$dfile = $base;
		} else {
			$dfile = "$base.$suffix";
		} 

	} else {

		$dir = "$archive_dir/$year/$host";
		$dfile = $user;
	}

	if( $Verbose ) { 

		if( $Dryrun != 0 ) {

			printf( "Would file " );

		} else {

			printf( "Filing " );
		}

		printf( "$name in $dir/$dfile\n" );
	}

	if( $Dryrun != 0 ) {

		return;
	}

	if( ! -e "$dir" ) {
		$status = system( "mkdir -p $dir" );
		if( $status ) {
			elog_complain( "Problem with $name\n" );
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

	my( $new_parts );
	chomp( $new_parts = `wc $dir/$dfile` );
	$new_parts =~ s/^\s*//;
	($new_startline, $new_foff ) = ( split( /\s+/, $new_parts ) )[0,2];

	$bytes = $new_foff - $foff;
	$lines = $new_startline - $startline;

	$dir = abspath( $dir );

	message_to_database( @db, $name, $dir, $dfile, $foff, $lines, $bytes, $mailobj );

}
1;
