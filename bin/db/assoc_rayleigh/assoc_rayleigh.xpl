use Getopt::Std ;
 
if ( ! getopts('v') || @ARGV != 2 )
    { die ( "Usage: $0 [-v] db origin_db\n" ) ; }

use Datascope ;

$rt_db   = $ARGV[0]  ;
$or_db   = $ARGV[1]  ;

@dbin      = dbopen  ( $rt_db, "r+") ;
@dbarrival = dblookup( @dbin,"","arrival","","") ;
@dbassoc   = dblookup( @dbin,"","assoc","","") ;
@dborigin  = dblookup( @dbin,"","origin","","") ;
@dbevent   = dblookup( @dbin,"","event","","") ;
@dbsite    = dblookup( @dbin,"","site","","") ;
$nrecs     = dbquery ( @dbarrival,"dbRECORD_COUNT") ;

print STDERR "$nrecs records in arrival \n" if $opt_v ; 

@dbor      = dbopen  ( $or_db, "r") ;
@dbor      = dblookup( @dbor,"","origin","","") ;
@dbor      = dbsort(@dbor);


$mintime = dbex_eval(@dbarrival,"min(time)");

print STDERR strtime($mintime), "	minimum time in arrival \n" if $opt_v ; 

if ( ! dbquery ( @dbarrival,"dbRECORD_COUNT")) {
     print STDERR  "No arrival records in $rt_db \n"  ; 
     exit (1);
}

@dbj    = dbjoin(@dbarrival, @dbsite)   ;
$subset = "iphase=='R'";
@dbj    = dbsubset(@dbj,$subset);

#  
#  set up groups based on evid (dbevent_g) and orid (dborigin_g)
#

$norigin     = dbquery ( @dbor,"dbRECORD_COUNT") ;

print STDERR "$nevents events in event group \n" if $opt_v ; 

#
#  loop over each event
#

for ($origin=0; $origin<$norigin; $origin++) {
    $dbor[3] = $origin ;
    ($olat, $olon, $otime, $author) = dbgetv(@dbor, qw(lat lon time auth));
    $subset = "111*distance(lat,lon,$olat,$olon)/(time - $otime) > 3.4 &&  111*distance(lat,lon,$olat,$olon)/(time - $otime) < 4.2";
    @dbr = dbsubset(@dbj,$subset);
    $nr = dbquery(@dbr,"dbRECORD_COUNT") ;
    print STDERR "\n",strtime($otime), "	origin $origin	$author	$nr\n" if $opt_v ; 

    for ($r=0; $r<$nr; $r++) {
        $dbr[3] = $r;
        ($slat, $slon, $stime) = dbgetv(@dbr, qw(lat lon time ));
        $distance = (dbex_eval(@dbr,"distance(lat,lon,$olat,$olon)"));
        $tt = $stime - $otime;
        $km = 111*$distance;
        $vel = $km/$tt;
        printf STDERR "%s  %8.2f %8.2f %8.2f %8.2f \n", strtime($stime),$distance,$km,$tt,$vel if $opt_v ; 
    }

    if ($nr) {
        @row = dbmatches(@dbor, @dborigin, "origin2origin", "lat", "lon", "depth", "time", "auth");
        print "matches origin rows @row \n" if $opt_v ;
        if (@row) {

            $dborigin[3] = $row[0]; 
            ($mlat, $mlon, $mtime, $mauthor, $nass, $orid) = dbgetv(@dborigin, qw(lat lon time auth nass orid));
            print STDERR strtime($mtime), "	row $row[0]	$nass  $nr \n" if $opt_v ; 
            $nass = dbgetv(@dborigin, qw(nass));
            $nass  = $nr + $nass;
            dbputv(@dborigin,"nass",$nass);
            &addassocs();

        }  else {

            @fields = grep !/lddate/, dbquery(@dbor,"dbTABLE_FIELDS") ;
            @values = dbgetv (@dbor, @fields);

            print "values	@values \n" if $opt_v ;
            foreach $field (@fields) {
                ${$field} = shift @values;
            }

            $orid = dbnextid(@dborigin,"orid");
            $nass = $nr;

            $subset = "distance(lat,lon,$olat,$olon) < 1.0 &&  abs (time - $otime) < 60 ";
            @dbmor  = dbsubset(@dborigin,$subset);
            if (dbquery(@dbmor,"dbRECORD_COUNT")) {
                $dbmor[3] = 0;
                $evid = dbgetv(@dbmor, qw(evid));
            } else {
                $evid = dbnextid(@dbevent,"evid");
                dbaddv(@dbevent,"evid", $evid, "prefor", $orid, "auth","rayl");
            }

            @record = ();
            foreach $field (@fields) {
                push @record, $field;
                push @record, ${$field};
            }
            print "record	@record \n" if $opt_v ;

            dbaddv(@dborigin,@record);

            &addassocs();
        }
    }
}



exit;
   
sub addassocs {
    my $arid, $slat, $slon, $sta, $phase, $delta, $seaz, $esaz, $time, $tt, $km, $vel;
    my $ctime,$cesza,$cseaz,$cdelta,$ckm,$cvel,$carid,$corid,$csta,$cphase;
    my @dbassoc_row,@dbjsub,@dbarrival_sub;
    my $r, $nr;
    my @dbr = @dbr;

    $nr = dbquery(@dbr,"dbRECORD_COUNT") ;
    for ($r=0; $r<$nr; $r++) {
        $dbr[3] = $r;
        ($arid, $slat, $slon, $sta, $phase, $time) = dbgetv(@dbr, qw(arid lat lon sta iphase time));
        $delta    = (dbex_eval(@dbr,"distance(lat,lon,$olat,$olon)"));
        $seaz     = (dbex_eval(@dbr,"azimuth(lat,lon,$olat,$olon)"));
        $esaz     = (dbex_eval(@dbr,"azimuth($olat,$olon,lat,lon)"));
        $tt = $time - $otime;
        $km = 111*$delta;
        $vel = $km/$tt;

        printf STDERR "%8d %8d %6s %8s %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n", $arid,$orid,$sta,$phase,$delta,$seaz,$esaz,$km,$tt,$vel if $opt_v ; 

        $result = eval { $checkrow = dbaddv(@dbassoc,"arid",  $arid, 
                               "orid",  $orid, 
                               "sta",   $sta, 
                               "phase", $phase, 
                               "delta", $delta, 
                               "seaz",  $seaz, 
                               "esaz",  $esaz);
        } ;
        if ( $@ ne "" ) {
            print STDERR "$@" ;
            $subset = "arid==$arid";
            print "$subset \n ";
            @dbassoc_row = dbsubset(@dbassoc, $subset);
            print "$subset  - @dbassoc_row \n ";
            $nassoc = dbquery(@dbassoc_row,"dbRECORD_COUNT") ;
            print "nassoc  $nassoc \n ";

            for ($assoc=0; $assoc<$nassoc; $assoc++) {
                $dbassoc_row[3] = $assoc;
                print "dbassoc_row  @dbassoc_row \n";
                ($carid, $corid, $cdelta, $cseaz, $cesaz, $csta, $cphase) = dbgetv(@dbassoc_row, qw(arid orid delta seaz esaz sta phase));
                printf STDERR "%8d %8d %6s %8s %8.2f %8.2f %8.2f\n", $carid,$corid,$csta,$cphase,$cdelta,$cseaz,$cesaz,$ckm,$ctime,$cvel if $opt_v ; 
                @dbjsub = dbjoin(@dbassoc_row,@dbarrival,"arid");
                ($catime) = dbgetv(@dbjsub, qw(time));
                printf STDERR "%s  %s   \n", "arrival",strtime($catime) if $opt_v ; 
                @dbjsub = dbjoin(@dbjsub,@dborigin);
                ($cotime) = dbgetv(@dbjsub, qw(origin.time));
                printf STDERR "%s  %s   \n", "origin",strtime($cotime) if $opt_v ; 

                $ctime     = (dbex_eval(@dbjsub,"time - origin.time"));
                $ckm = 111*$cdelta;
                $cvel = $ckm/$ctime;
                printf STDERR "%8d %8d %6s %8s %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n", $carid,$corid,$csta,$cphase,$cdelta,$cseaz,$cesaz,$ckm,$ctime,$cvel if $opt_v ; 

            }
        }
    }    
}


