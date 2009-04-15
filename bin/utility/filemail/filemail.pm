#   Copyright (c) 2005 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

package filemail;

require Exporter;
@ISA = ('Exporter');

@EXPORT = qw( parse_address message_to_database get_epoch realname filemail
	      parse_message_ids clean_message_id );
@EXPORT_OK = qw( $Verbose $FormatProblems $Nullhost $Dirmode $Filemode 
		 $Relpath $Schema );

use Datascope ;
use Mail::Internet;

#Keep the `From ' fields to use that date if the Date tag is bad:
Mail::Header->mail_from( COERCE );

$Verbose = 0;
$Dryrun = 0;
$Relpath = 0;
$FormatProblems = "FormatProblems";
$Nullhost = "localhost";
$Dirmode = "0755";
$Filemode = "0444";
$Schema = "Mail1.5";
@PreserveHosts = ();
@Me = ();
%Subjects = ();
$Sent_archive_pattern = "%Y/SENT/%B";

sub parse_message_ids {
	my( $line ) = @_;
	my( @ids );

	my( $n ) = length( $line );

	my( $state ) = "searching";

	my( $message_id ) = "";

	for( $i = 0; $i < $n; $i++ ) {

		$c = substr( $line, $i, 1 );

		if( $state eq "searching" && $c eq '<' ) {

			$state = "acquiring";

			$message_id .= $c;

		} elsif( $state eq "acquiring" && $c eq '>' ) {

			$state = "searching";

			$message_id .= $c;

			$message_id = clean_message_id( $message_id );

			push( @ids, $message_id );

		} elsif( $state eq "acquiring" ) {

			$message_id .= $c;
		}
	}

	return @ids;
}

sub clean_message_id {
	my( $message_id ) = @_;

	$message_id =~ s/\s+//g;

	$message_id =~ m/<(.*)>/;

	return $1;
}

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

sub reset_dir {
	my( @db ) = splice( @_, 0, 4 );
	my( $dir ) = pop( @_ );

	@db = dblookup( @db, "", "", "dir", "" );

	my( $dir_size ) = dbquery( @db, dbFIELD_SIZE );

	if( $Relpath == 0 && length( $dir ) > $dir_size ) {

		elog_complain( "Dir '$dir' exceeds $dir_size-byte size of dir field." .
				" Switching to relative-path mode\n" );

		$Relpath++;
	}

	if( $Relpath ) {
		
		my( $table_path ) = dbquery( @db, dbTABLE_DIRNAME );

		$dir = relpath( $table_path, $dir );
	}

	if( length( $dir ) > $dir_size ) {

		elog_die( "Dir '$dir' exceeds $dir_size-byte size of dir field " .
			  "Even with relative path. Can't continue, bye.\n" );
	}

	return $dir;
}

sub message_to_database {
	my( @db ) = splice( @_, 0, 4 );
	my( $name, $dir, $dfile, $foff, $lines, $bytes, @message ) = @_;

	if( dbquery( @db, dbSCHEMA_NAME ) ne $Schema ) {
		
		elog_complain( "Please upgrade your database to schema $Schema\n" );

		return;
	}

	my( @dbin ) = dblookup( @db, "", "in", "", "" );
	my( @dbout ) = dblookup( @db, "", "out", "", "" );
	my( @dbcorr ) = dblookup( @db, "", "correspondents", "", "" );
	my( @dbrefs ) = dblookup( @db, "", "references", "", "" );

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

	my( $subject );
	chomp( $subject = $mailobj->head->get( "Subject" ) );

 	my( $epoch ) = get_epoch( $mailobj );
 
 	if( $address eq "" || $epoch == 0  ) {
 		
		elog_complain( "Problem parsing message $from at $epoch. " .
			       "Not recording in database!\n" ); 

		return;
	}

	my( $message_id, $in_reply_to, $references, @in_reply_to, @references );

	chomp( $message_id = $mailobj->head->get( "Message-ID" ) );
	chomp( $in_reply_to = $mailobj->head->get( "In-Reply-To" ) );
	chomp( $references = $mailobj->head->get( "References" ) );

	@in_reply_to = parse_message_ids( $in_reply_to );
	@references = parse_message_ids( $references );

	# Remove references that are already in the in_reply_to header:

	@references = 
		map { $ref = $_; grep( m/$ref/, @in_reply_to ) ? () : $ref }
			@references;

 	if( $Verbose ) {
 		elog_notify( "Adding to database mail from $from at $epoch: " .
			"foff $foff lines $lines bytes $bytes $dir $dfile\n" );
	}

	if( defined( $message_id ) ) {
		
		$message_id = clean_message_id( $message_id );

		foreach $ref ( @in_reply_to ) {
			
			dbaddv( @dbrefs, "messageid", $message_id,
					 "inreplyto", "y", 
					 "reference", $ref );
		}

		foreach $ref ( @references ) {
			
			dbaddv( @dbrefs, "messageid", $message_id,
					 "reference", $ref );
		}

	} else {

		$message_id = "-";
	}

	my( @dbtemp ) = dblookup( @dbcorr, "", "correspondents", "from", "" );
	my( $address_size ) = dbquery( @dbtemp, dbFIELD_SIZE );

	if( $sent ) {

		$dir = reset_dir( @dbout, $dir );
		$dbout[3] = dbaddnull( @dbout );
		dbputv( @dbout,  "to", $to, 
				"subject", $subject, 
				"time", $epoch, 
				"lines", $lines,
				"bytes", $bytes,
				"foff", $foff,
				"dir", $dir, 
				"dfile", $dfile,
				"messageid", $message_id );
	} else {

		$dir = reset_dir( @dbout, $dir );
		$dbin[3] = dbaddnull( @dbin );
		dbputv( @dbin,  "from", $address, 
				"subject", $subject, 
				"time", $epoch, 
				"lines", $lines,
				"bytes", $bytes,
				"foff", $foff,
				"dir", $dir, 
				"dfile", $dfile,
				"messageid", $message_id );

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

sub chmod_dirs {
	my( $dir ) = @_;

	my( $base, $suffix );

	system( "chmod $Dirmode $dir" );

	($dir, $base, $suffix) = parsepath( $dir );

	while( $dir ne "." && -w "$dir" ) {

		system( "chmod $Dirmode $dir" );
	
		($dir, $base, $suffix) = parsepath( $dir );
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
		$status = makedir( $dir );
		if( $status ) {
			elog_complain( "Problem with $name\n" );
		}
		chmod_dirs( $dir );
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
	system( "chmod $Filemode $dir/$dfile" );

	my( $new_parts );
	chomp( $new_parts = `wc $dir/$dfile` );
	$new_parts =~ s/^\s*//;
	($new_startline, $new_foff ) = ( split( /\s+/, $new_parts ) )[0,2];

	$bytes = $new_foff - $foff;
	$lines = $new_startline - $startline;

	$dir = abspath( $dir );

	message_to_database( @db, $name, $dir, $dfile, $foff, 
			     $lines, $bytes, $mailobj );

}
1;
