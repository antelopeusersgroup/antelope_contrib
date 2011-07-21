#
#   program needs:
#
#   DONE	calculate responce for each white noise calibration in dlcalwf table
#   DONE	make sure only calculated only one time
#   DONE	create calplot table
#   DONE	put data in /anf/TA/products/calibrations/%sta/%sta_%chan_%time_%version.pdf
#   DONE	email Calibration Report
#
#   needs to be able to select calibration channels BH, HH, LH, ....
#   needs to select dbcalibrate.pf file
#

    require "getopts.pl" ;
    use strict ;
    use Datascope ;
    use archive ;
    use utilfunct ;
    
    our ( $opt_C, $opt_D, $opt_M, $opt_P, $opt_V, $opt_d, $opt_g, $opt_m, $opt_n, $opt_v ) ;
    our ( $pgm, $host );
    our ( %pf, %plots, %problems );
    
{    #  Main program

    my ( $caldir, $cmd, $db, $problems, $stime, $subject, $usage ) ;
    
#
#  Setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    $problems = 0;
    $usage    =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] [-C] [-P] \n" ;
    $usage   .=  "	[-d calibration_directory] [-D display] [-m mail_to] [-M mail_to_field_ops] [-g]\n" ;
    $usage   .=  "	db [sta_regex] \n\n"  ; 
    
    if (  ! &Getopts('CPgnvVd:D:m:M:') || @ARGV < 1 ) { 
        elog_notify ( $cmd ) ; 
        elog_die    ( $usage ) ;
    }
    
    &savemail() if $opt_m ; 
    elog_notify( $cmd ) ; 
    $stime      = strydtime( now() );
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");
                
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
#
#  Get options and parameters
#
    $db         = shift @ARGV;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    

    $caldir     = $opt_d || "/anf/TA/products/calibrations" ;
    
#
#  check db
#
    $problems = &check_tables($db,$problems,qw(dlcalwf dlsensor sensormodel wfdisc));
    if ($problems) {
        $subject = "Problems - $pgm $host	db $db	missing tables" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    
    &dbcalibrate( $db ) unless ( $opt_P ) ;
    
#
#  run displayscal on all unprocessed rows in sensorcal table
#        

    &displayscal( $db, $caldir ) unless ( $opt_C ) ;

    prettyprint(\%plots) if $opt_V;
#    prettyprint(\%ENV) if $opt_V;
    
#
#  send emails with plots of newly plotted stations if desired
#    

    &email( ) unless ( $opt_C ) ;


#
#  clean up and exit
#

    if ($opt_D) {
        $cmd = "vncserver.rt -kill :$opt_D" ;
        elog_notify( $cmd ) ;
        $problems = run($cmd,$problems) unless $opt_n ;
    }
    $stime = strydtime(now());
    elog_notify ("\ncompleted successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0);
}

sub dbcalibrate { # &dbcalibrate( $db ) ;
    my ( $db ) = @_ ;
    my ( $cmd, $endtime, $row, $sta, $start, $subset, $time ) ;
    my ( @cals, @db, @dbdlcalwf, @dbscrsc, @dbsensorcal ) ;
#
#  open db
#
    @db            = dbopen($db,'r+');
    @dbdlcalwf     = dblookup(@db,0,"dlcalwf",0,0);
    @dbsensorcal   = dblookup(@db,0,"sensorcal",0,0);
    @dbscrsc       = dblookup(@db,0,"sensorcal",0,"dbSCRATCH");
#
#  subset dlcalwf by station regular expressions.
#
    if ( @ARGV ) {
        $subset = "fsta =~ /$ARGV[0]/";
        elog_notify("subsetting dlcalwf by - 	$subset") if $opt_V;
        @dbdlcalwf = dbsubset(@dbdlcalwf, $subset) ;
    }
#
#  subset dlcalwf to BHZ channels with white noise longer than one hour.
#    
    $subset = "fchan =~ /BHZ/ && dlcalerr =~ /ok/ && dlcaltype =~ /white/ && (endtime - time) > 3600 && snet !~ /BB/" ;
    elog_notify("dlcalwf subset	$subset") if $opt_V ;
    @dbdlcalwf = dbsubset(@dbdlcalwf,$subset) ;

    $start = now() ;
#
#  run dbcalibrate on all unprocessed calibrations
#        
    for ($row=0; $row < dbquery(@dbdlcalwf,"dbRECORD_COUNT") ; $row++ ) {
        
        $dbdlcalwf[3] = $row ;
        
        ($sta,$time,$endtime) = dbgetv(@dbdlcalwf,"fsta","time","endtime") ;
        
        dbputv(@dbscrsc,"sta", $sta, "chan", "BHZ", "time", $time, "endtime", $endtime) ;
        
        @cals = dbmatches(@dbscrsc,@dbsensorcal,"processed","sta","chan","time::endtime") ;
        if ($#cals > -1) {
            elog_notify(sprintf("Already processed	$sta	%s	%s",strydtime($time),strydtime($endtime))) if $opt_v ;
            next ;
        }
        
        $cmd  = "dbcalibrate -v -dlcalwf_sifter \"fsta =~ /$sta/ && fchan =~ /BH./ && " ;
        $cmd .= "time < _" . strydtime( $endtime ) . "_ && endtime > _" . strydtime( $time ) ;
        $cmd .= "_ && dlcaltype =~ /white/ \" -out $db $db " ;
        $cmd .= " > /tmp/dbcalibrate 2>&1 " ;            
        if ( ! run_cmd( $cmd ) ) {
            $cmd = "cat /tmp/dbcalibrate" ;
            run_cmd( $cmd ) ;
        }
    }
    
    dbclose(@db);
    
    return ;
}

sub displayscal { # &displayscal( $db, $caldir ) ;
    my ( $db, $caldir ) = @_ ;
    my ( $chan, $cmd, $dlcaltype, $dlident, $dlmodel, $endtime, $pdfdir, $pdffile ) ;
    my ( $pdffull, $psfile, $rchan, $row, $rsptype, $sngen, $snident, $snmodel, $sta, $subset ) ;
    my ( $time ) ;
    my ( @db, @dbcalplot, @dbscrplot, @dbsensorcal, @plts ) ;
#
#  open db
#
    @db            = dbopen(   $db, 'r+' ) ;
    @dbsensorcal   = dblookup( @db, 0, "sensorcal", 0, 0 ) ;
    @dbcalplot     = dblookup( @db, 0, "calplot", 0, 0 ) ;
    @dbscrplot     = dblookup( @db, 0, "calplot", 0, "dbSCRATCH" ) ;
#
#  subset sensorcal by station regular expressions.
#
    
    if ( @ARGV ) {
        $subset = "sta =~ /$ARGV[0]/" ;
        elog_notify("subsetting sensorcal by - 	$subset") if $opt_V ;        
    }
    
    for ($row=0; $row < dbquery(@dbsensorcal,"dbRECORD_COUNT") ; $row++ ) {
        $dbsensorcal[3] = $row ;
        ( $sta, $chan, $time, $endtime, $dlcaltype, $dlmodel, $dlident, $snmodel, 
                                               $snident, $sngen, $rsptype, $rchan ) =
            dbgetv( @dbsensorcal, "sta", "chan", "time", "endtime", "dlcaltype", "dlmodel",
                                  "dlident", "snmodel", "snident", "sngen", "rsptype", "rchan" ) ;
                                  
#
#  do not subset sensorcal table since the actual row number is needed by displayscals
#        

        dbputv( @dbscrplot, "sta", $sta, "chan", $chan, "time", $time, "endtime", $endtime ) ;
        
        elog_notify( sprintf( "\nsensorcal	%s	%s	%s	%s", $sta, $chan, strydtime( $time ), strydtime( $endtime ) ) ) if $opt_V;
        
        @plts = dbmatches( @dbscrplot, @dbcalplot, "plot", "sta", "chan", "time::endtime" );
        
        if ($#plts > -1) {
            elog_notify( sprintf( "Already plotted	$sta	$chan	%s	%s", strydtime( $time ), strydtime( $endtime ) ) ) if $opt_v;
            next ;
        }

        $pdfdir  = "$caldir/$sta" ;
        makedir( $pdfdir) if ( ! -e $pdfdir);
        $pdffile = "$sta\_$chan\_";
        $pdffile .= epoch2str($time,"%Y-%j-%H-%M");
        $pdffile .= ".pdf" ;
        
        $psfile = $pdffile ;
        $psfile =~ s/\.pdf/\.ps/;
        
        $pdffull = $pdfdir . "/" . $pdffile;

        if (-e $pdffull ) {
            elog_notify( "$pdffull already exists" );
            next;
        }
#
#  load up most recent info from sensorcal table
#        

        $plots{$sta}{$chan}{time}       = $time ;
        $plots{$sta}{$chan}{duration}   = $endtime - $time ;
        $plots{$sta}{$chan}{pdffull}    = $pdffull ;
        $plots{$sta}{$chan}{dlcaltype}  = $dlcaltype ;
        $plots{$sta}{$chan}{dlmodel}    = $dlmodel ;
        $plots{$sta}{$chan}{dlident}    = $dlident ;
        $plots{$sta}{$chan}{snmodel}    = $snmodel ;
        $plots{$sta}{$chan}{snident}    = $snident ;
        $plots{$sta}{$chan}{sngen}      = $sngen ;
        $plots{$sta}{$chan}{rsptype}    = $rsptype ;
        $plots{$sta}{$chan}{rchan}      = $rchan ;

#
#  build plots
#        
        $cmd  = "displayscal -dumpandexit $psfile $db $row" ;
        run_cmd ( $cmd ) ;
        
        sleep 1 ;
        
        $cmd  = "ps2pdf14 $psfile $pdffull" ;
        run_cmd ( $cmd ) ;
        
        $cmd  = "rm $psfile";
        run_cmd ( $cmd ) ;
        
        dbaddv( @dbcalplot,  "sta",      $sta,
                             "chan",    $chan,
                             "time",    $time,
                             "endtime", $endtime,
                             "dir",     $pdfdir,
                             "dfile",   $pdffile ) unless $opt_n;
#
#  identify problems
#        
        if ($chan =~ /BHZ/ && $snmodel =~ /sts2.*|cmg3t/  && ($sngen < 1350 || $sngen > 1650)) {
            $plots{$sta}{problem}    = 1;
            
            $problems{$sta}{$chan}{$time}{time}       = $time;
            $problems{$sta}{$chan}{$time}{duration}   = $endtime - $time;
            $problems{$sta}{$chan}{$time}{pdffull}    = $pdffull;
            $problems{$sta}{$chan}{$time}{dlcaltype}  = $dlcaltype;
            $problems{$sta}{$chan}{$time}{dlmodel}    = $dlmodel;
            $problems{$sta}{$chan}{$time}{dlident}    = $dlident;
            $problems{$sta}{$chan}{$time}{snmodel}    = $snmodel;
            $problems{$sta}{$chan}{$time}{snident}    = $snident;
            $problems{$sta}{$chan}{$time}{sngen}      = $sngen;
            $problems{$sta}{$chan}{$time}{rsptype}    = $rsptype;
            $problems{$sta}{$chan}{$time}{rchan}      = $rchan;          
            $problems{$sta}{$chan}{$time}{error}      = "sensor gain > 1650 || sensor gain < 1350" ;          
        }
        
        if ($chan =~ /BHZ/ && $snmodel =~ /trillium.*/  && ($sngen < 1050 || $sngen > 1350)) {
            $plots{$sta}{problem}    = 1;
            
            $problems{$sta}{$chan}{$time}{time}       = $time;
            $problems{$sta}{$chan}{$time}{duration}   = $endtime - $time;
            $problems{$sta}{$chan}{$time}{pdffull}    = $pdffull;
            $problems{$sta}{$chan}{$time}{dlcaltype}  = $dlcaltype;
            $problems{$sta}{$chan}{$time}{dlmodel}    = $dlmodel;
            $problems{$sta}{$chan}{$time}{dlident}    = $dlident;
            $problems{$sta}{$chan}{$time}{snmodel}    = $snmodel;
            $problems{$sta}{$chan}{$time}{snident}    = $snident;
            $problems{$sta}{$chan}{$time}{sngen}      = $sngen;
            $problems{$sta}{$chan}{$time}{rsptype}    = $rsptype;
            $problems{$sta}{$chan}{$time}{rchan}      = $rchan;          
            $problems{$sta}{$chan}{$time}{error}      = "sensor gain > 1650 || sensor gain < 1350" ;          
        }
        
        if ($chan =~ /BH[NE]/ && $snmodel =~ /cmg3t/ && ($sngen < 1250 || $sngen > 1550)) {
            $plots{$sta}{problem}    = 1; 
            
            $problems{$sta}{$chan}{$time}{time}       = $time;
            $problems{$sta}{$chan}{$time}{duration}   = $endtime - $time;
            $problems{$sta}{$chan}{$time}{pdffull}    = $pdffull;
            $problems{$sta}{$chan}{$time}{dlcaltype}  = $dlcaltype;
            $problems{$sta}{$chan}{$time}{dlmodel}    = $dlmodel;
            $problems{$sta}{$chan}{$time}{dlident}    = $dlident;
            $problems{$sta}{$chan}{$time}{snmodel}    = $snmodel;
            $problems{$sta}{$chan}{$time}{snident}    = $snident;
            $problems{$sta}{$chan}{$time}{sngen}      = $sngen;
            $problems{$sta}{$chan}{$time}{rsptype}    = $rsptype;
            $problems{$sta}{$chan}{$time}{rchan}      = $rchan;          
            $problems{$sta}{$chan}{$time}{error}      = "sensor gain > 1550 || sensor gain < 1250" ;          
        }
        
        if ($chan =~ /BH[NE]/ && $snmodel =~ /sts2.*|trillium.*/ && $sngen > 75) {
            $plots{$sta}{problem}    = 1;
            
            $problems{$sta}{$chan}{$time}{time}       = $time;
            $problems{$sta}{$chan}{$time}{duration}   = $endtime - $time;
            $problems{$sta}{$chan}{$time}{pdffull}    = $pdffull;
            $problems{$sta}{$chan}{$time}{dlcaltype}  = $dlcaltype;
            $problems{$sta}{$chan}{$time}{dlmodel}    = $dlmodel;
            $problems{$sta}{$chan}{$time}{dlident}    = $dlident;
            $problems{$sta}{$chan}{$time}{snmodel}    = $snmodel;
            $problems{$sta}{$chan}{$time}{snident}    = $snident;
            $problems{$sta}{$chan}{$time}{sngen}      = $sngen;
            $problems{$sta}{$chan}{$time}{rsptype}    = $rsptype;
            $problems{$sta}{$chan}{$time}{rchan}      = $rchan;          
            $problems{$sta}{$chan}{$time}{error}      = "sensor gain > 75" ;          
        }
    }
    
    dbclose(@db);
}

sub email { # &email( ) ;
    my ( $cmd, $sta, $string, $time ) ;
    my ( @newdata ) ;

    @newdata = sort ( keys ( %plots ) ) ;
    foreach $sta (@newdata) {
        elog_notify ($sta) if $opt_V;

        unless (exists $plots{$sta}{BHZ}{time}) {
            $string = "Calibration data does not exist for $sta BHZ";
            elog_complain($string);
            next;
        }
        unless (exists $plots{$sta}{BHN}{time}) {
            $string = "Calibration data does not exist for $sta BHN";
            elog_complain($string);
            next;
        }
        unless (exists $plots{$sta}{BHE}{time}) {
            $string = "Calibration data does not exist for $sta BHE";
            elog_complain($string);
            next;
        }
                
        open   (CAL, ">/tmp/cal" );
        print  CAL "US Array Calibration Sequence Report\nForm V2.0\n\n";
        print  CAL "Station Code    $sta\n\n";
        
        if (exists $plots{$sta}{problem}) {
            
           if (exists $problems{$sta}{BHZ}) {
                printf CAL "BHZ Calibration Problem info\n";
                foreach $time (keys %{ $problems{$sta}{BHZ} }) {
                    print  CAL "Datalogger      $plots{$sta}{BHZ}{dlmodel}    $plots{$sta}{BHZ}{dlident}          ";
                    print  CAL "Sensor          $plots{$sta}{BHZ}{snmodel}    $plots{$sta}{BHZ}{snident}\n";
                    printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n",
                                strydtime($time),
                                $problems{$sta}{BHZ}{$time}{dlcaltype},
                                $problems{$sta}{BHZ}{$time}{duration},
                                $problems{$sta}{BHZ}{$time}{rchan},
                                $problems{$sta}{BHZ}{$time}{rsptype};
                    printf CAL "Sensor gains    BHZ  %6d    <- failed test \"%s\"\n\n", $problems{$sta}{BHZ}{$time}{sngen} , $problems{$sta}{BHZ}{$time}{error};
                    $cmd .= "-a $problems{$sta}{BHZ}{$time}{pdffull} ";
                }
            } else {
                printf CAL "BHZ Calibration info\n";
                print  CAL "Datalogger      $plots{$sta}{BHZ}{dlmodel}    $plots{$sta}{BHZ}{dlident}          ";
                print  CAL "Sensor          $plots{$sta}{BHZ}{snmodel}    $plots{$sta}{BHZ}{snident}\n";
                printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n",
                        strydtime($plots{$sta}{BHE}{time}),
                        $plots{$sta}{BHE}{dlcaltype},
                        $plots{$sta}{BHE}{duration},
                        $plots{$sta}{BHE}{rchan},
                        $plots{$sta}{BHE}{rsptype};
                printf CAL "Sensor gains    BHZ  %6d\n", $plots{$sta}{BHZ}{sngen};
                $cmd .= "-a $plots{$sta}{BHZ}{pdffull} ";
            }
            
            if (exists $problems{$sta}{BHN}) {
                printf CAL "\nBHN Calibration Problem info\n";
                foreach $time (keys %{ $problems{$sta}{BHN} }) {
                    print  CAL "Datalogger      $plots{$sta}{BHN}{dlmodel}    $plots{$sta}{BHN}{dlident}          ";
                    print  CAL "Sensor          $plots{$sta}{BHN}{snmodel}    $plots{$sta}{BHN}{snident}\n";
                    printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n",
                                strydtime($time),
                                $problems{$sta}{BHN}{$time}{dlcaltype},
                                $problems{$sta}{BHN}{$time}{duration},
                                $problems{$sta}{BHN}{$time}{rchan},
                                $problems{$sta}{BHN}{$time}{rsptype};
                    printf CAL "Sensor gains    BHN  %6d    <- failed test \"%s\"\n\n", $problems{$sta}{BHN}{$time}{sngen} , $problems{$sta}{BHN}{$time}{error};
                    $cmd .= "-a $problems{$sta}{BHN}{$time}{pdffull} ";
                }
            } else {
                printf CAL "\nBHN Calibration info\n";
                print  CAL "Datalogger      $plots{$sta}{BHN}{dlmodel}    $plots{$sta}{BHN}{dlident}          ";
                print  CAL "Sensor          $plots{$sta}{BHN}{snmodel}    $plots{$sta}{BHN}{snident}\n";
                printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n",
                        strydtime($plots{$sta}{BHN}{time}),
                        $plots{$sta}{BHN}{dlcaltype},
                        $plots{$sta}{BHN}{duration},
                        $plots{$sta}{BHN}{rchan},
                        $plots{$sta}{BHN}{rsptype};
                printf CAL "Sensor gains    BHN  %6d\n", $plots{$sta}{BHN}{sngen};
                $cmd .= "-a $plots{$sta}{BHN}{pdffull} ";
            }
            
            if (exists $problems{$sta}{BHE}) {
                printf CAL "\nBHE Calibration Problem info\n";
                foreach $time (keys %{ $problems{$sta}{BHE} }) {
                    print  CAL "Datalogger      $plots{$sta}{BHE}{dlmodel}    $plots{$sta}{BHE}{dlident}          ";
                    print  CAL "Sensor          $plots{$sta}{BHE}{snmodel}    $plots{$sta}{BHE}{snident}\n";
                    printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n",
                                strydtime($time),
                                $problems{$sta}{BHE}{$time}{dlcaltype},
                                $problems{$sta}{BHE}{$time}{duration},
                                $problems{$sta}{BHE}{$time}{rchan},
                                $problems{$sta}{BHE}{$time}{rsptype};
                    printf CAL "Sensor gains    BHE  %6d    <- failed test \"%s\"\n\n", $problems{$sta}{BHE}{$time}{sngen} , $problems{$sta}{BHE}{$time}{error};
                    $cmd .= "-a $problems{$sta}{BHE}{$time}{pdffull} ";
                }
            } else {
                printf CAL "\nBHE Calibration info\n";
                print  CAL "Datalogger      $plots{$sta}{BHE}{dlmodel}    $plots{$sta}{BHE}{dlident}          ";
                print  CAL "Sensor          $plots{$sta}{BHE}{snmodel}    $plots{$sta}{BHE}{snident}\n";
                printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n",
                        strydtime($plots{$sta}{BHE}{time}),
                        $plots{$sta}{BHE}{dlcaltype},
                        $plots{$sta}{BHE}{duration},
                        $plots{$sta}{BHE}{rchan},
                        $plots{$sta}{BHE}{rsptype};
                printf CAL "Sensor gains    BHE  %6d\n", $plots{$sta}{BHE}{sngen};
                $cmd .= "-a $plots{$sta}{BHE}{pdffull} ";
            }
            
            
            print (CAL "-------------------------------------------------------\n\n");
            close(CAL);
        
            $cmd  = "rtmail -s \"Calibration Sequence Report - $sta  Problem\" ";
            $cmd .= "$opt_M ";
            $cmd .= "< /tmp/cal";
        
        } else {
            print  CAL "Datalogger      $plots{$sta}{BHZ}{dlmodel}    $plots{$sta}{BHZ}{dlident}\n\n";
            print  CAL "Sensor          $plots{$sta}{BHZ}{snmodel}    $plots{$sta}{BHZ}{snident}\n\n";
            printf CAL "Calibration info\n";
            printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n",
                        strydtime($plots{$sta}{BHN}{time}),
                        $plots{$sta}{BHN}{dlcaltype},
                        $plots{$sta}{BHN}{duration},
                        $plots{$sta}{BHN}{rchan},
                        $plots{$sta}{BHN}{rsptype};
            printf CAL "%s    type - %s    duration - %d    refchan - %s    response type - %s\n\n",
                        strydtime($plots{$sta}{BHE}{time}),
                        $plots{$sta}{BHE}{dlcaltype},
                        $plots{$sta}{BHE}{duration},
                        $plots{$sta}{BHE}{rchan},
                        $plots{$sta}{BHE}{rsptype};
            printf CAL "Sensor gains    BHZ  %6d\n                 BHN  %6d\n                 BHE  %6d \n\n",
                        $plots{$sta}{BHZ}{sngen},
                        $plots{$sta}{BHN}{sngen},
                        $plots{$sta}{BHE}{sngen};
            print ( CAL "-------------------------------------------------------\n\n");
            close ( CAL );
        
            $cmd  = "rtmail -s \"Calibration Sequence Report - $sta\" ";
            $cmd .= "-a $plots{$sta}{BHZ}{pdffull} ";
            $cmd .= "-a $plots{$sta}{BHN}{pdffull} ";
            $cmd .= "-a $plots{$sta}{BHE}{pdffull} ";
            $cmd .= "$opt_M ";
            $cmd .= "< /tmp/cal";
        
        }
        if ( $opt_M ) {
            if ( $opt_g ) {
                run_cmd( $cmd ) ;
            } else {
                run_cmd( $cmd ) if (exists $plots{$sta}{problem});
            }
        }
        
    }
}