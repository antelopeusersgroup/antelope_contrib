#
#   program needs:
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
#     use timeslice ;
#     use timeutil ;
    use utilfunct ;
    
    our ( $pgm, $host );
    our ( $opt_v, $opt_V, $opt_n, $opt_p, $opt_s );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $baler14_end, $baler14_end_null, $baler14_start, $baler14_start_null, $baler14_sync, $baler44_end, $baler44_start, $cmd, $dbactive, $dbb44, $dbfinal, $dbops, $dbrt, $dir, $element, $msd_files, $nbaler, $ndep, $sta, $stime, $subject, $sync, $usage, $wfendtime, $wftime ) ;
    my ( $dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) ;
    my ( @dbactive, @dbbaler, @dbb44, @dbdeploy, @dbfinal, @dbnull, @dbops, @dbrt, @dirs, @files ) ;
#
#  Program setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnp:s:') ||  @ARGV != 1 ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] [-p pf] [-s sta_regex] " ;
        $usage .=  "dbops \n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die   ( $usage ) ; 
    }
    
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    chop ( $host = `uname -n` ) ;
    elog_notify ( "\nstarting execution on	$host	$stime\n\n" ) ;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    $Pf = $opt_p || $pgm ;
    %pf = getparam( $Pf );

    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        elog_die( "\n$subject" );
    }
    
    $dbops    = $ARGV[0] ;
        
    @dbops    = dbopen  ( $dbops, "r+" ) ;
    @dbdeploy = dblookup( @dbops, 0, "deployment",   0, 0 ) ;
    @dbbaler  = dblookup( @dbops, 0, "balerproc",   0, 0 ) ; 

    @dbdeploy = dbsubset( @dbdeploy, "snet =~ /$pf{net}/" ) ; 
    @dbdeploy = dbsubset( @dbdeploy, "sta  =~ /$opt_s/ " ) if $opt_s ; 
    @dbdeploy = dbsort  ( @dbdeploy, qw ( -u sta ) ) ;

    $ndep     = dbquery ( @dbdeploy, "dbRECORD_COUNT" ) ;
    elog_notify "n deployed		$ndep" ;
    
    $nbaler   = dbquery ( @dbbaler, "dbRECORD_COUNT" ) ;
    elog_notify "n balers		$nbaler" ;

    @dbdeploy = dbnojoin( @dbdeploy, @dbbaler, "sta" ) ;
    $ndep     = dbquery ( @dbdeploy, "dbRECORD_COUNT" ) ;
    elog_notify "new stations		$ndep" ;
    
    for ( $dbdeploy[3] = 0 ; $dbdeploy[3] < $ndep ; $dbdeploy[3]++ ) {
        dbaddv( @dbbaler, "net", dbgetv( @dbdeploy, "snet" ), "sta", dbgetv( @dbdeploy, "sta" ) ) ;
    }
    
    dbclose ( @dbops ) ;
    
    @dbops    = dbopen  ( $dbops, "r+" ) ;
    @dbbaler  = dblookup( @dbops,   0, "balerproc",   0, 0 ) ; 
    @dbnull   = dblookup( @dbbaler, 0, 0, 0, "dbNULL" ) ; 
    @dbbaler  = dbsubset( @dbbaler, "completed == NULL" ) ;
    @dbbaler  = dbsubset( @dbbaler, "sta  =~ /$opt_s/ " ) if $opt_s ; 
    
    ( $baler14_start_null, $baler14_end_null ) = dbgetv ( @dbnull, qw ( baler14_start baler14_end ) ) ;
    
    elog_debug ( "NULLS	$baler14_start_null	$baler14_end_null" ) if $opt_V ;

    $nbaler   = dbquery ( @dbbaler, "dbRECORD_COUNT" ) ;
    elog_notify "n balers		$nbaler" ;

    for ( $dbbaler[3] = 0 ; $dbbaler[3] < $nbaler ; $dbbaler[3]++ ) {
        ( $sta, $baler14_start, $baler14_end ) = dbgetv( @dbbaler, qw ( sta baler14_start baler14_end ) ) ;
        elog_notify ( "$sta" ) ;
        
#  loop over possible baler14 directories
#  put first and last msd files into table

        foreach $element ( @{$pf{baler14_dirs}} ) {
            $msd_files = $element . "/$sta/BVLAOU/C*[s0-9]" ;
            elog_debug ( "	$msd_files" ) if $opt_V ;
            @files = < $msd_files > ;
            if ( $#files != -1 && $baler14_start == $baler14_start_null ) {
            
                $baler14_start = $files[0] ;
                $baler14_start =~ s/.*\/// ;
                elog_debug ( "		$baler14_start" ) if $opt_V ;
                dbputv ( @dbbaler, "baler14_start", $baler14_start ) ;
                
                $baler14_end = $files[$#files] ;
                $baler14_end =~ s/.*\/// ;
                elog_debug ( "		$baler14_end" ) if $opt_V ;
                dbputv ( @dbbaler, "baler14_end", $baler14_end ) ;

            }
        }
        
#  loop over possible baler14 directories and accomodate if more than 10 msd files from baler.  
#  update last msd file only in table

        foreach $element ( @{$pf{baler14_dirs}} ) {
            $msd_files = $element . "/$sta/BVLAOU/C*bms_[1-9][0-9]" ;
            elog_debug ( "	$msd_files" ) if $opt_V ;
            @files = < $msd_files > ;
            elog_debug ( "		@files" ) if $opt_V ;
            if ( $#files != -1 ) {
                $baler14_end = $files[$#files] ;
                $baler14_end =~ s/.*\/// ;
                elog_debug ( "		$baler14_end" ) if $opt_V ;
                dbputv ( @dbbaler, "baler14_end", $baler14_end ) ;
            }
        }
        
#  process baler44 directories

        foreach $element ( @{$pf{baler44_dirs}} ) {
            $dbb44 = $element . "/$sta/$sta\_baler" ;
            if ( -f $dbb44 ) {
                elog_debug ( " 	baler44	$dbb44" ) if $opt_V ;
                @dbb44 = dbopen  ( $dbb44, 'r' ) ;
                @dbb44 = dblookup( @dbb44,   0, "rsyncbaler",   0, 0 ) ;
                
                if ( dbquery ( @dbb44, "dbTABLE_PRESENT" ) ) {
                    @dbb44 = dbsort  ( @dbb44,  "dfile" ) ;
                    $dbb44[3] = 0 ;
                    $baler44_start = dbgetv ( @dbb44, qw ( dfile ) ) ;
                    $dbb44[3] = dbquery ( @dbb44, "dbRECORD_COUNT" ) - 1 ;
                    $baler44_end = dbgetv ( @dbb44, qw ( dfile ) ) ;
                    dbputv( @dbbaler, "baler44_start", $baler44_start, "baler44_end", $baler44_end ) ;
                } 
                dbclose ( @dbb44 ) ;
            }
        }


#  process station active directory 

        $dbactive = "$pf{active_baler44_dir}/$sta/$sta" ;
        if ( -f $dbactive ) {
            elog_debug ( " 	active	$dbactive" ) if $opt_V ;
            @dbactive = dbopen  ( $dbactive, 'r' ) ;
            
            @dbactive = dblookup( @dbactive,   0, "wfdisc",   0, 0 ) ;
            @dbactive = dbsubset( @dbactive, "chan =~ /BHZ/ " ) ;
            dbputv( @dbbaler, "proc_start", dbex_eval( @dbactive, "min(time)" ), "proc_end", dbex_eval( @dbactive, "max(endtime)" ) ) ;

            @dbactive = dblookup( @dbactive,   0, "replayed",   0, 0 ) ;
            if ( dbquery ( @dbactive, "dbTABLE_PRESENT" ) ) {
                @dbactive = dbsubset( @dbactive, "chan =~ /BHZ/ " ) ;
                dbputv( @dbbaler, "dmc_start", dbex_eval( @dbactive, "min(time)" ), "dmc_end", dbex_eval( @dbactive, "max(endtime)" ) ) ;
            }
            
            dbclose ( @dbactive ) ; 
        }

#  process station active directory 

        $dbrt = "$pf{rt_sta_dir}/$sta/$sta" ;
        if ( -f $dbrt ) {
            elog_debug ( " 	rt_sta	$dbrt" ) if $opt_V ;
            @dbrt = dbopen  ( $dbrt, 'r' ) ;
            
            @dbrt = dblookup( @dbrt,   0, "wfdisc",   0, 0 ) ;
            if ( dbquery ( @dbrt, "dbTABLE_PRESENT" ) ) {
                @dbrt = dbsubset( @dbrt, "chan =~ /BHZ/ " ) ;
                dbputv( @dbbaler, "rt_start", dbex_eval( @dbrt, "min(time)" ), "rt_end", dbex_eval( @dbrt, "max(endtime)" ) ) ;
            }

            dbclose ( @dbrt ) ; 
        }

        
#  loop over station final directories

        foreach $element ( @{$pf{final_baler_dirs}} ) {
            $dbfinal = "$element/$sta/$sta" ;
            
            elog_debug ( "		$dbfinal" ) if $opt_V ;
            
            next if ( ! -f $dbfinal ) ; 
            
            @dbfinal    = dbopen  ( $dbfinal, 'r' ) ;
            
            @dbfinal    = dblookup( @dbfinal,   0, "wfdisc",   0, 0 ) ;
            @dbfinal    = dbsubset( @dbfinal, "chan =~ /BHZ/ " ) ;
            $wftime     = dbex_eval( @dbfinal, "min(time)" ) ;
            $wfendtime  = dbex_eval( @dbfinal, "max(endtime)" ) ;
            dbputv( @dbbaler, "proc_start", $wftime, "proc_end", $wfendtime ) ;

            @dbfinal = dblookup( @dbfinal,   0, "replayed",   0, 0 ) ;
            
            if ( dbquery ( @dbfinal, "dbTABLE_PRESENT" ) ) {
                @dbfinal = dbsubset( @dbfinal, "chan =~ /BHZ/ " ) ;
                dbputv( @dbbaler, "dmc_start", dbex_eval( @dbfinal, "min(time)" ), "dmc_end", dbex_eval( @dbfinal, "max(endtime)" ), "completed", dbex_eval( @dbfinal, "max(lddate)" ) ) ;
            } else {  #  original baler14 processing was did not have replayed table.  need to look at sync file
                $baler14_sync = "$pf{baler14_sync}/*" ;
                @dirs = < $baler14_sync > ;
                elog_debug ( "$baler14_sync	dirs	@dirs" ) if $opt_V ;
                
                foreach $dir ( @dirs ) {
                    $sync = "$dir/$sta\_final.sync" ;
                    if ( -f $sync ) {
                        ( $dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat( $sync ) ;
                        dbputv( @dbbaler, "dmc_start", $wftime, "dmc_end", $wfendtime, "completed", $mtime ) ;
                    }
                }
            }
            
            dbclose ( @dbfinal ) ; 
        }

    }
    
    dbclose ( @dbops ) ;    
    
#
#  Finish program
#
    $stime = strydtime(now());
    
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host ");
    elog_notify ($subject);    
  
    exit( 0 );
}


