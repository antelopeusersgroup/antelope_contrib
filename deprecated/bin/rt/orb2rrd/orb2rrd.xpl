#
#   Copyright (c) 2006 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
#   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
#   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
#   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
#   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
#   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
#   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
#   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Getopt::Std ;
use Datascope;
use orb;
use RRDs;
 
sub inform {
	my( $msg ) = @_;
	
	if( $opt_v ) {
		
		elog_notify( "$msg\n" );
	}

	return;
}

#Fake the trwfname call since it's not in the perldb interface
sub trwfname {
	my( $pattern ) = pop( @_ );
	my( @db ) = @_;

	my( $net, $sta, $chan, $rrdvar ) = 
		dbgetv( @db, "net", "sta", "chan", "rrdvar" );

	my( $path_from_db, $fullpath, $tabledir, $relpath, $dir );

	$path_from_db = epoch2str( $time, $pattern );
	$path_from_db =~ s/{net}/$net/;
	$path_from_db =~ s/{sta}/$sta/;
	$path_from_db =~ s/{chan}/$chan/;
	$path_from_db =~ s/{rrdvar}/$rrdvar/;

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

	system( "mkdir -p $dir_from_here" );

	return $path_from_here;
}

sub archive_chanvar {
	my( $achan ) = @_;

	my( $rrd, @data, $nsamp, $samprate, $time,  $val );
	my( $net, $sta, $chan, $start_time );

	$net = $achan->net();
	$sta = $achan->sta();
	$chan = $achan->chan();
	$start_time = $achan->chan();

	if( ! defined $Chan_vars{$chan} ) {

		if( $opt_V ) {
			
			inform( "Skipping channel $chan because it's not in 'chan_vars' table of parameter file $Pf\n" );
		}

		return;
	}

	my( $key ) = $net . ":" . $sta . ":" . $chan;

	if( ! defined( $Rrd_files{$key} ) || ! -e "$Rrd_files{$key}" ) {

		$start_time = $time - $Waveform_stepsize_sec;

		my( @dbt ) = @Db;
		$dbt[3] = dbaddnull( @Db );

		dbputv( @dbt,
			"net", $net,
			"sta", $sta,
			"rrdvar", $chan,
			"time", $start_time );

		$rrd = trwfname( @dbt, $Rrdfile_pattern );

		my( $datasource ) = 
			"DS:$chan:$Chan_vars{$chan}{'dsparams'}";

		inform( "Creating rrdfile $rrd\n" ); 

		RRDs::create( "$rrd", 
				"-b", "$start_time", 
				"-s", "$Waveform_stepsize_sec",
				"$datasource", @{$Chan_vars{$chan}{'rras'}} ); 

		my $ERR = RRDs::error;

		if( $ERR ) {

			elog_complain( "ERROR while creating '$rrd': $ERR\n" ) 
		}

		$Rrd_files{$key} = $rrd;

	} else {
		
		$rrd = $Rrd_files{$key};
	}

	@data = $achan->data;
	$nsamp = $achan->nsamp;
	$samprate = $achan->samprate;

	for( $isamp = 0; $isamp < $nsamp; $isamp++ ) {

		$time = $achan->time + $isamp / $samprate;

		$val = $data[$isamp];

		if( $opt_V ) {

			inform( "Recording time '$time' value '$val' from " . 
				"'$chan' in $rrd\n" );
		}

		RRDs::update( $rrd, "$time:$val" );

		my $ERR = RRDs::error;

		if( $ERR ) {

			elog_complain( "ERROR while updating '$rrd': $ERR\n" ) 
		}
	}

	return;
}

sub archive_dlsvar {
	my( $net, $sta, $dls_var, $time, $val ) = @_;

	my( $key ) = "$net:$sta:$dls_var";

	my( $rrd );

	if( ! defined( $Rrd_files{$key} ) || ! -e "$Rrd_files{$key}" ) {

		my( $start_time ) = $time - $Status_stepsize_sec;

		my( @dbt ) = @Db;
		$dbt[3] = dbaddnull( @Db );

		dbputv( @dbt,
			"net", $net,
			"sta", $sta,
			"rrdvar", $dls_var,
			"time", $start_time );

		$rrd = trwfname( @dbt, $Rrdfile_pattern );

		my( $datasource ) = 
			"DS:$dls_var:$Dls_vars{$dls_var}{'dsparams'}";

		inform( "Creating rrdfile $rrd\n" ); 

		RRDs::create( "$rrd", 
				"-b", "$start_time", 
				"-s", "$Status_stepsize_sec",
				"$datasource", @{$Dls_vars{$dls_var}{'rras'}} ); 
		my $ERR = RRDs::error;

		if( $ERR ) {

			elog_complain( "ERROR while creating '$rrd': $ERR\n" ) 
		}

		$Rrd_files{$key} = $rrd;

	} else {
		
		$rrd = $Rrd_files{$key};
	}

	if( $opt_V ) {

		inform( "Recording time '$time' value '$val' from " . 
			"'$dls_var' in $rrd\n" );
	}

	if( $val eq "-" ) {

		if( $opt_V ) {

			inform( "Recording null value 'U' for variable " .
			   "'$dls_var' at time '$time' in $rrd\n" );
		}

		$val = "U";
	}

	RRDs::update( $rrd, "$time:$val" );

	my $ERR = RRDs::error;

	if( $ERR ) {

		elog_complain( "ERROR while updating '$rrd': $ERR\n" ) 
	}

	return;
}

$Pf = "orb2rrd.pf";
$match = ".*/pf/st";
$pktid = 0;
$time = -9999999999.999;

if ( ! getopts('s:f:p:m:vV') || @ARGV != 2 ) { 

    	die ( "Usage: orb2rrd [-vV] [-s statefile] [-p pffile] " .
	      "[-m match] [-f from] orb dbcache\n" ) ; 

} else {
	
	$orbname = $ARGV[0];
	$dbcache = $ARGV[1];
}

elog_init( $0, @ARGV );

if( $opt_V ) {
	
	$opt_v++;
}

if( $opt_v ) {

        $now = str2epoch( "now" );

        elog_notify( "Starting at " . epoch2str( $now, "%D %T %Z", "" ) .
                     " (orb2rrd \$Revision$\ " .
                     "\$Date$\)\n" );
}

if( $opt_p ) {
	
	$Pf = $opt_p;
}

if( $opt_m ) {
	
	$match = $opt_m;
}

$orb = orbopen( $orbname, "r&" );

if( $orb < 0 ) {

	die( "Failed to open orb '$orbname' for reading\n" );
}

orbselect( $orb, $match );

if( $opt_f && ( ! $opt_s || ! -e "$opt_s" ) ) {
	
	$pktid = orbposition( $orb, $opt_f );

	inform( "Positioned to packet $pktid" );

} elsif( $opt_f ) {

	elog_complain( "Ignoring -f in favor of existing state-file\n" );
}

if( $opt_s ) {

	$stop = 0;
	exhume( $opt_s, \$stop, 15 );
	orbresurrect( $orb, \$pktid, \$time  );
	orbseek( $orb, "$pktid" );
}

@Db = dbopen( $dbcache, "r+" );

if( $Db[0] < 0 ) {

	die( "Failed to open cache database '$dbcache'. Bye.\n" );

} else {

	@Db = dblookup( @Db, "", "rrdcache", "", "" );

	if( $Db[1] < 0 ) {
		
		die( "Failed to lookup 'rrdcache' table in '$dbcache'. Bye.\n" );
	}
}

@dbt = dbsubset( @Db, "endtime == NULL" );

for( $dbt[3] = 0; $dbt[3] < dbquery( @dbt, dbRECORD_COUNT ); $dbt[3]++ ) {
	
	( $net, $sta, $rrdvar ) = dbgetv( @dbt, "net", "sta", "rrdvar" );

	$path = dbextfile( @dbt );

	$Rrd_files{"$net:$sta:$rrdvar"} = $path;
}

$Rrdfile_pattern = pfget( $Pf, "rrdfile_pattern" );
$Status_stepsize_sec = pfget( $Pf, "status_stepsize_sec" );
$Waveform_stepsize_sec = pfget( $Pf, "waveform_stepsize_sec" );
$Default_network = pfget( $Pf, "default_network" );
@dlslines = @{pfget( $Pf, "dls_vars" )};
@chanlines = @{pfget( $Pf, "chan_vars" )};

foreach $line ( @dlslines ) {

	my( $dls_var, $dsparams, @myrras ) = split( /\s+/, $line );

	$Dls_vars{$dls_var}{'dsparams'} = $dsparams;
	$Dls_vars{$dls_var}{'rras'} = \@myrras;
}

foreach $line ( @chanlines ) {

	my( $chan_var, $dsparams, @myrras ) = split( /\s+/, $line );

	$Chan_vars{$chan_var}{'dsparams'} = $dsparams;
	$Chan_vars{$chan_var}{'rras'} = \@myrras;
}

for( ; $stop == 0 ; ) {

	($pktid, $srcname, $time, $packet, $nbytes) = orbreap( $orb );

	if( $opt_s ) {

		eval( bury() );
	
		if( $@ ) {
		
			elog_complain( "Unexpected failure of bury command! " .
				"(are there two orb2rrd's running with the same" .
				"state file??)\n" );
		}
	}

	($result, $pkt) = unstuffPkt( $srcname, $time, $packet, $nbytes ); 

	if( $result eq "Pkt_pf" ) {

		$msg = "Received a parameter-file '$srcname' at " . 
			strtime( $time );

		if( $opt_V ) {
			$msg .= ":\n" . pf2string( $pkt->pf ) . "\n\n";
		} else {
			$msg .= "\n";
		}

		inform( $msg );

		%mypktpf = %{pfget( $pkt->pf(), "dls" )};
	
		$time = int( $time );
	
		foreach $element ( keys %mypktpf ) {
	  	   foreach $dls_var ( keys %Dls_vars ) {
	
			if( $element =~ /_/ ) {

				( $net, $sta ) = split( '_', $element );

			} else {

				$net = $Default_network;

				$sta = $element;
			}
	
			$val =  $mypktpf{$element}{$dls_var};
	
			archive_dlsvar( $net, $sta, $dls_var, $time, $val );
	   	    }
		}

	} elsif( $result eq "Pkt_wf" ) {

		if( $opt_V ) {
	
			showPkt( $pktid, $srcname, $time, $packet, 
				 $nbytes, PKT_NOSAMPLES );

		} else {

			$msg = "Received a waveform packet '$srcname' at " . 
				strtime( $time ) . "\n";
		}

		inform( $msg );

		for( $ichan = 0; $ichan < $pkt->nchannels; $ichan++ ) {

			$achan = $pkt->channels( $ichan );

			archive_chanvar( $achan );
		}

	} else {

		inform( "Received a packet that's not a parameter file " .
			"or waveform (type '$result' from unstuffPkt); " .
			"skipping\n" );

		next;
	}
}
