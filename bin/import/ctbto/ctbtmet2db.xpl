# met2db for CTBT met Bulletins
#
# Nikolaus Horn
# ZAMG / Vienna
# 2013-01-22

use Datascope ;


if ( @ARGV < 2 ) {

	die( "Usage: $0 filename database\n" );

}
our @Db;

sub init_globals {

	$State = "startup";

	use vars qw(
			$sta, $time, $endtime, 
			$temperature_NULL, $winddirection_NULL, $windspeed_NULL, $pressure_NULL,
			$humidity_NULL, $rainfall_NULL,
			@Db, @db_met
			);
}

sub stateswitch {
	( $line ) = @_;

	if( $line =~ /^\s*BEGIN\s+(\S+)\s+/i ) {
		$format = uc( $1 );
		if( $format ne "IMS1.0" &&
		    $format ne "IMS2.0" ) {
			die( "File $ARGV Not in IMS1.0, " .
			     "or IMS2.0 format\n" ); 
		}
		$State = "startup";
		return 1;
	}
	if( $line =~ /^\s*DATA_TYPE\s+(\S+)\s+/i ) {
		$type = uc( $1 );
		if( $type eq "MET" ) {
			$State = "sta";
		}
		return 1;
	}
	if( $line =~ /^\s*STOP\s+/i ) {
		$State = "startup";
		return 1;
	}
	return 0;
}

sub searching {
	( $line ) = @_;
	# intentional null routine
}

sub startup {
	( $line ) = @_;
	# intentional null routine
}

sub sta {
	( $line ) = @_;
	if ( $line =~ /^\s*(.*)/i ) {
		$sta=$1;
		$State = "addline";
	}
}
sub addline {
	( $line ) = @_;
        ($y1, $m1, $d1, $h1, $min1, $sec1,
            $y2, $m2, $d2, $h2, $min2, $sec2,
             $temperature, $winddirection, $windspeed, $pressure, $humidity, $rainfall)=
        unpack( "A4 x A2 x A2 x A2 x A2 x A4 x A4 x A2 x A2 x A2 x A2 x A4 x A5 x A3 x A5 x A7 x A3 x A5",$line);
        my ($time,$endtime);
        my $ts=sprintf("%04d-%02d-%02d %02d:%02d:%04.1f",$y1,$m1,$d1,$h1,$min1,$sec1);
        eval {
            $time=str2epoch($ts);
        };
        $ts=sprintf("%04d-%02d-%02d %02d:%02d:%04.1f",$y2,$m2,$d2,$h2,$min2,$sec2);
        eval {
            $endtime=str2epoch($ts);
        };
		$endtime -= 0.001;
		# whow, they are using DIFFERENT null values.
		# This code here ignores everything starting with -99
		# physically, this should be ok. Only winddirection could be wrong when ignoring values of -99 degrees...
		$temperature = $temperature_NULL if ($temperature =~ /-99/ );
		$winddirection = $winddirection_NULL if ($winddirection =~ /-99/ );
		$windspeed = $windspeed_NULL if ($windspeed =~ /-99/ );
		$pressure = $pressure_NULL if ($pressure =~ /-99/ );
		$humidity = $humidity_NULL if ($humidity =~ /-99/ );
		$rainfall = $rainfall_NULL if ($rainfall =~ /-99/ );
        eval{
		$recno=dbaddv(@db_met,"sta",$sta,"time",$time,"endtime",$endtime,
				"temperature",$temperature,"winddirection",$winddirection,
				"windspeed",$windspeed,"pressure",$pressure,
				"humidity",$humidity,"rainfall",$rainfall
			  );
        };
        if ($@) {
            $recno=undef;
            if ($@=~ /.*record #(\d+).*/) {
                $row=$1;
                $db_met[3]=$row;
            dbputv(@db_met,"sta",$sta,"time",$time,"endtime",$endtime,
				"temperature",$temperature,"winddirection",$winddirection,
				"windspeed",$windspeed,"pressure",$pressure,
				"humidity",$humidity,"rainfall",$rainfall
                );
            }
        }
}
if( $#ARGV < 1 ) {
	die "Usage: ctbtrb2db filename [filename ...] dbname\n";
} else {
	$dbname = pop( @ARGV );
	@Db = dbopen( $dbname, "r+" );
	$dbfilename=dbquery(@Db,"dbDATABASE_FILENAME");
	if (! -f $dbfilename) {
		dbclose(@Db);
		dbcreate($dbname,"ctbto1.2");
		@Db = dbopen( $dbname, "r+" );
	}
	@db_met=dblookup(@Db,"","met","","");
	@db_NULL=dblookup(@Db,"","met","","dbNULL");
	( $temperature_NULL, $winddirection_NULL, $windspeed_NULL, 
		$pressure_NULL, $humidity_NULL, $rainfall_NULL )=
		dbgetv(@db_NULL,  qw(temperature  winddirection  windspeed  pressure  humidity  rainfall ));

}

init_globals;

while( <ARGV> ) {
	chop;
	next if( /^\s*$/ );
	next if( /^\s*\(.*\)\s*$/ ); # comment
	last if ( /^STOP$/i ); #last line to be processed, avoid confusing error with ISC data	
	next if stateswitch( $_ );
	&$State( $_ );
} 

#everythin already done above
dbclose (@Db);
