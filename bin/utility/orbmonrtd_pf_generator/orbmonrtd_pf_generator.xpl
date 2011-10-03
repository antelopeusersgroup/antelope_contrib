use strict ;
use warnings ;

#
# $Id$
#
# Dynamically create parameter files for orbmonrtd
# from dbmaster database tables
#
# @author     Rob Newman <rlnewman@ucsd.edu>
# @copyright  University of California, San Diego
# @version    $Revision$
#

use Getopt::Std ;
use Datascope ;

my( $Program ) = `basename $0` ;
chomp( $Program ) ;

elog_init( $Program, @ARGV ) ;

our( $dbname, $pfdir, %State, @db_subset, $this_pf_file, $revision_time ) ;
our( $opt_v, $opt_d, $opt_t, $opt_p ) ;

if( ! getopts( 'vd:t:p:' ) || @ARGV != 1 ) {
    die( "Usage: orbmonrtdpfgenerator [-v] [-d pfdirectory] [-t table] [-p pffile] dbmaster\n" ) ;
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
    my $pf_change_time = "1238630400" ;
 
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
        "filter",
        "pf_revision_time"
    ) ;
    foreach my $param ( @params ) {
        $State{$param} = pfget( $State{pf}, $param ) ;
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

my( @db ) = dbopen( $dbname, "r" ) ;

my( @db_sitechan ) = dblookup( @db, "", "sitechan", "", "" ) ;
my( @db_snetsta ) = dblookup( @db, "", "snetsta", "", "" ) ;

my( @db_joined ) = dbjoin( @db_sitechan, @db_snetsta ) ;

# Allow subsets to handle offdates in the future like IRIS wants
if( defined( $opt_t ) && $opt_t eq "deployment" ) {
    my( @db_deployment ) = dblookup( @db, "", "deployment", "", "" ) ;
    @db = dbjoin( @db_joined, @db_deployment ) ;
    @db = dbsubset( @db, "endtime > now() || endtime == NULL" ) ;
}
@db = dbsubset( @db_joined, "offdate > now() || offdate == NULL" ) ;
@db = dbsort( @db, "snet","sta" ) ;

while( my( $key, @value ) = each ( %{ $State{sources} } ) ) {

    my( $expr ) = $State{sources}->{$key}->{'subset'} ;
    my( $modulus ) = $State{sources}->{$key}->{'modulus'} ;

    if( $expr ne '' ) {
        @db_subset = dbprocess( @db, @{$expr} ) ;

        if( $opt_v ) {
            print( "Subset with the commands:\n" ) ;
            foreach( @{$expr} ) {
                print "- ".$_."\n" ;
            }
        }

    } else {
        @db_subset = @db ;
    }

    if( dbquery( @db_subset, "dbRECORD_COUNT" ) < 1 ) {
        die( 'No records returned from your subsets' ) ;
    }

    if( $opt_v ) {
        print( "Retrieving snet_station names for: $key\n" ) ;
    }

    if( defined $modulus && $modulus ne '' ) {
        print( "Subsetting using modulus value of $modulus for: $key\n" ) ;
    }

    my( @netstachans ) ;
    for( $db_subset[3]=0; $db_subset[3] < dbquery( @db_subset, "dbRECORD_COUNT" ); $db_subset[3]++ ) {
        my( $snet, $staname, $chan ) = dbgetv( @db_subset, "snet", "sta", "chan" ) ;

        if( $opt_v ) {
            print( "Values returned => Snet: ".$snet." Staname: ".$staname." Chan: ".$chan."\n" ) ;
        }

        my( $snet_sta_chan ) = $snet."_".$staname."_".$chan ;

        if( defined $modulus && $modulus ne '' ) {
            if( $db_subset[3] % $modulus == 0 ) {
                push @netstachans, trim($snet_sta_chan) ;
            }
        } else {
            push @netstachans, $snet_sta_chan ;
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
    my( $amin ) = $State{sources}->{$key}->{'amin'} ;
    my( $amax ) = $State{sources}->{$key}->{'amax'} ;
    my( $width ) = $State{sources}->{$key}->{'width'} ;
    my( $height ) = $State{sources}->{$key}->{'height'} ;
    my( $filter ) = $State{sources}->{$key}->{'filter'} ;
    my( $orb ) = $State{orb} ;

    my( @this_sources_tbl ) ;

    foreach( @netstachans ) {
        if( exists $State{exceptions}->{$_} ) {
            my( $ex_amin ) = $State{exceptions}->{$_}->{'amin'} ;
            my( $ex_amax ) = $State{exceptions}->{$_}->{'amax'} ;
            my( $ex_width ) = $State{exceptions}->{$_}->{'width'} ;
            my( $ex_height ) = $State{exceptions}->{$_}->{'height'} ;
            my( $ex_filter ) = $State{exceptions}->{$_}->{'filter'} ;
            my( $source_str ) = trim( $_." ".$orb." ".$tw." ".$ex_amin." ".$ex_amax." ".$ex_width." ".$ex_height." ".$ex_filter ) ;
            push @this_sources_tbl, $source_str ;
            undef $source_str ;
            undef $ex_amin ; undef $ex_amax ; undef $ex_width ; undef $ex_height ; undef $ex_filter ;
        } else {
            #net_sta_chan   inputorb   twin   amin amax   width height [filter]
            my( $source_str ) = trim( $_." ".$orb." ".$tw." ".$amin." ".$amax." ".$width." ".$height." ".$filter ) ;
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

    undef @db_subset ;
}
# Clean up
dbfree( @db ) ;
dbclose( @db ) ;
exit 0 ;
