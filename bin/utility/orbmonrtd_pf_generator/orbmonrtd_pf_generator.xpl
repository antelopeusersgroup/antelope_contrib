use strict ;
use warnings ;

#
# $Id$
#
# Dynamically create parameter files for orbmonrtd
# from dbmaster deployment table
#
# @author     Rob Newman <rlnewman@ucsd.edu>
# @copyright  University of California, San Diego
# @version    $Revision$
#

require "getopts.pl" ;
use Datascope ;
use IO ;

my( $Program ) = `basename $0` ;
chomp( $Program ) ;

elog_init( $Program, @ARGV ) ;

our( $dbname, $pfdir, %State, @db, $this_pf_file, $revision_time ) ;
our( $opt_v, $opt_d, $opt_p ) ;

if( ! &Getopts( 'vd:p:' ) || @ARGV != 1 ) {
    die( "Usage: orbmonrtdpfgenerator [-v] [-d pfdirectory] [-p pffile] dbmaster\n" ) ;
} else {
    $dbname = $ARGV[0] ;
    $pfdir = $opt_d ;
    $State{pf} = $opt_p ;
}

sub trim($) {
	my $string = shift;
	$string =~ s/^\s+//;
	$string =~ s/\s+$//;
	return $string;
}

sub setup_state {
    $State{pf} =~ s/\.pf$// ;
    if( system( "pfecho $State{pf} > /dev/null 2>&1" ) ) {
        die( "Couldn't find $State{pf}.pf. Goodbye.\n" ) ;
    }
    my $pf_change_time = "1199145600" ;
 
    if( pfrequire( $State{pf}, $pf_change_time ) < 0 ) {
        elog_flush( 1, 0 ) ;
        die( "The parameter file '$State{pf}.pf' is out of date. " .
        "Please upgrade to the latest version. Goodbye.\n" ) ;
    }
}

sub init_globals {
    # Get variables from parameter file
    my( @params ) = (
        "dump_cmd",
        "orb",
        "sources",
        "exceptions",
        "detections",
        "arrivals",
        "pf_revision_time"
    ) ;
    foreach my $param ( @params ) {
        $State{$param} = pfget( $State{pf}, $param ) ;
    }
}

sub check_dbmaster {
    @db = dblookup( @db, "", "deployment", "", "" ) ;
    if( dbquery( @db, "dbRECORD_COUNT" ) < 1 ) {
        dbfree( @db ) ;
        dbclose( @db ) ;
        die( "The dbmaster database deployment table has no records! " .
        "Please check the path defined in the -d option. Goodbye.\n" ) ;
    } else {
        if( $opt_v ) {
            print( "Successfully connected to dbmaster $dbname and opened deployment table.\n" ) ;
        }
    }
}

sub pf_sources {
    my( $pf, $hashname ) = @_ ;
    print "$hashname\n" ;
    my( $sourcespec ) ;
    my( $hash ) = pfget( $pf, $hashname ) ;
    print $hash ;
}

setup_state() ;
init_globals() ;

@db = dbopen( $dbname, "r" ) ;

check_dbmaster() ;

@db = dbsubset( @db, "endtime > now()" ) ;
@db = dbsort( @db, "snet","sta" ) ;

while( my( $key, @value ) = each ( %{ $State{sources} } ) ) {

    my( $expr ) = $State{sources}->{$key}->{'subset'} ;
    my( $modulus ) = $State{sources}->{$key}->{'modulus'} ;

    if( $expr ne '' ) {
        @db = dbprocess( @db, @{$expr} ) ;
    }

    if( dbquery( @db, "dbRECORD_COUNT" ) < 1 ) {
        die( 'No records returned from your subsets' ) ;
    }

    if( $opt_v ) {
        print( "Retrieving snet_station names for: $key\n" ) ;
    }

    if( defined $modulus && $modulus ne '' ) {
        print( "Subsetting using modulus value of $modulus for: $key\n" ) ;
    }

    my( @netstas ) ;
    for( $db[3]=0; $db[3] < dbquery( @db, "dbRECORD_COUNT" ); $db[3]++ ) {
        my( $snet, $staname ) = dbgetv( @db, "snet", "sta" ) ;
        my( $snet_sta ) = $snet."_".$staname ;
        if( defined $modulus && $modulus ne '' ) {
            if( $db[3] % $modulus == 0 ) {
                push @netstas, trim($snet_sta) ;
            }
        } else {
            push @netstas, $snet_sta ;
        }
    }

    if( $opt_v ) {
        print "Creating parameter file for: $key\n" ;
    }

    $this_pf_file = $pfdir."/".$key.".pf" ;
    open( DYNAPF, ">".$this_pf_file ) || 
        die( "Cannot create parameter file ".$key.".pf. Check permissions.\n" ) ;
    # The following are directly mapped from the generator parameter file
    if ( defined $State{filter} ) { pfput( "filter", $State{filter}, 'pfobj' ) } ;
    if ( defined $State{dump_cmd} ) { pfput( "dump", $State{dump_cmd}, 'pfobj' ) } ;
    if ( defined $State{detections} ) { pfput( "detections", $State{detections}, 'pfobj' ) } ;
    if ( defined $State{arrivals} ) { pfput( "arrivals", $State{arrivals}, 'pfobj' ) } ;

    $revision_time = time() ;
    pfput( "pf_revision_time", $revision_time, 'pfobj' ) ; 
 
    if( $opt_v ) { 
        print "Parameter file revision time is $revision_time for: $key\n" ;
    }

    my( $tw ) = $State{sources}->{$key}->{'tw'} ;
    my( $chan ) = $State{sources}->{$key}->{'chan'} ;
    my( $amin ) = $State{sources}->{$key}->{'amin'} ;
    my( $amax ) = $State{sources}->{$key}->{'amax'} ;
    my( $width ) = $State{sources}->{$key}->{'width'} ;
    my( $height ) = $State{sources}->{$key}->{'height'} ;
    my( $filter ) = $State{sources}->{$key}->{'filter'} ;
    my( $orb ) = $State{orb} ;

    my( @this_sources_tbl ) ;

    foreach( @netstas ) {
        if( exists $State{exceptions}->{$_} ) {
            my( $ex_amin ) = $State{exceptions}->{$_}->{'amin'} ;
            my( $ex_amax ) = $State{exceptions}->{$_}->{'amax'} ;
            my( $ex_width ) = $State{exceptions}->{$_}->{'width'} ;
            my( $ex_height ) = $State{exceptions}->{$_}->{'height'} ;
            my( $ex_filter ) = $State{exceptions}->{$_}->{'filter'} ;
            my( $source_str ) = trim( $_."_".$chan." ".$orb." ".$tw." ".$ex_amin." ".$ex_amax." ".$ex_width." ".$ex_height." ".$ex_filter ) ;
            push @this_sources_tbl, $source_str ;
            undef $source_str ;
            undef $ex_amin ; undef $ex_amax ; undef $ex_width ; undef $ex_height ; undef $ex_filter ;
        } else {
            #net_sta_chan   inputorb   twin   amin amax   width height [filter]
            my( $source_str ) = trim( $_."_".$chan." ".$orb." ".$tw." ".$amin." ".$amax." ".$width." ".$height." ".$filter ) ;
            push @this_sources_tbl, $source_str ;
            undef $source_str ;
        }
    }

    if( $opt_v ) {
        print "Putting values into parameter file for: $key\n" ;
    }
    pfput( "sources", \@this_sources_tbl, 'pfobj' ) ;
    pfwrite( $this_pf_file, 'pfobj' ) ;
    close( DYNAPF ) ;
}

exit 0 ;
