#   Copyright (c) 2007 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. Any use of this software module without        
#   express written permission from Boulder Real Time Technologies,   
#   Inc. is prohibited.                                               


# use strict ; 
# use warnings ; 

require HTTP::Request;
use LWP ; 
use HTML::Parser ;
use HTML::Entities ;
use rt ; 
use archive ; 
use sysinfo ;
use Datascope ; 
use orb ;
use Getopt::Std ;

our ( $opt_d, $opt_n, $opt_p, $opt_s, $opt_t, $opt_v, $opt_V ) ; 
our ( $QTFORMAT, $Max ) ;

$QTFORMAT = "%Y%%2F%m%%2F%d+%H%%3A%M%%3A%S" ;

my $Ua = LWP::UserAgent->new;

$Max = "" ; 
our $Mbyte = 1000 * 1000 ; 

our ($Total_size, $Received) ;
our ($Pid_miniseed, %Old) ; 

our ($Failed) ; 

our ($Pf, $cmdorb, $balerdbdir, $balerdb, $balerinfodir, $balerdatadir) ;
our (@all_chan_skip,@info,@interface);

our  ($nhttp, $nhttpf, $thttp, $thttpf, $nbpwr, $nbpwrf, $tbpwr, $tbpwrf);
our  ($nlbwf, $nlbwff, $tlbwf, $tlbwff, $ncon);
our  ($url);
our  (@no_power, @no_info, @no_mseed_info, @no_connection);

{       
    my ($src, $orb, $srcname, $when, $target, $orbname ) ;
    my ($ref, $sta, $net, $dbdeploy, $dbops, $nsta, $ncon_try, $usage) ;
    my (@netstas, @sources, @retry);

#
#  set up
#
    @no_power = @no_info = @no_mseed_info = @no_connection = @retry = ();
    
    $nhttp = $nhttpf = $nbpwr = $nbpwrf = $nlbwf = $nlbwff = 0;
    $thttp = $thttpf = $tbpwr = $tbpwrf = $tlbwf = $tlbwff = 0.0; 
#
#  get arguments
#
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    elog_notify("$0 @ARGV");

    if ( ! getopts('d:np:s:t:vV') || @ARGV != 3 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-n]   \n" ;
        $usage .=  "	[-d deployment_db] [-p pf]  \n" ;
        $usage .=  "	[-s net_sta_regex ] [-t q3302orb_target ] \n" ;
        $usage .=  "	orb cmd_orb dbops \n\n"  ; 
        elog_die ( $usage ) ; 
    }
    
    $orbname = $ARGV[0];
    $cmdorb = $ARGV[1];
    $dbops  = $ARGV[2] ;
    
    $dbdeploy = $opt_d || $dbops ;
    
    $target  = $opt_t || ".*" ;
    $srcname = $target . "/pf/st" ;
#
#  get parameters
#
    $Pf = $opt_p || "rt_baler_info" ;
    
    elog_notify ("\n    " . announce_str() . "\n" )   if ($opt_v||$opt_V) ; 
    elog_notify ("    orb		$orbname")    if ($opt_v||$opt_V) ;
    elog_notify ("    cmdorb		$cmdorb")     if ($opt_v||$opt_V) ;
    elog_notify ("    srcname		$srcname")    if ($opt_v||$opt_V) ;

    &getpars();
#
#  open orb for reading and select q330 /pf/st files
#
    $orb = orbopen($orbname,"r");

    if( $orb < 0 ) {
        die( "Failed to open orb '$orbname' for reading\n" );
    }

    orbselect( $orb, $srcname);
    
    ($when, @sources) = orbsources ( $orb );
    if ($#sources == -1) {
        die( "no orbsource $srcname for target $target \n");
    }
    
    elog_notify ("    sources		@sources")    if ($opt_V) ;

    
    if ($opt_n) { @sources = () ;}
#
#  loop over each source name
#        
    foreach $src (@sources) {
        @netstas = () ;
#
#  select and unstuff pf packet from orb
#
        $srcname = $src->srcname() ;
        
        @netstas = &src_stas($orb, $srcname);
        
        $nsta += scalar(@netstas) ;
#
#  process station list
#
        &proc_target($orb,$srcname,$dbops,@netstas) ;
    }
    
#
#  retry missing stations
#        
    $ncon_try = $nsta ;
    push(@retry, @no_connection) ;
    push(@retry, @no_power) ;
    push(@retry, @no_info) ;
    push(@retry, @no_mseed_info) ;
    @retry = sort(@retry) ;
    
    foreach $src (@sources) {
        @netstas = () ;
#
#  select and unstuff pf packet from orb
#
        $srcname = $src->srcname() ;
        
        @netstas = &src_stas($orb, $srcname) ;
        @netstas = sort(&intersect(\@netstas,\@retry)) ;
        $ncon_try += scalar(@netstas) ;

#
#  process station list
#
        &proc_target($orb,$srcname,$dbops,@netstas) ;
    }
    
    &deploy_fix($balerdbdir,$dbdeploy);
    
    &build_balerall_db($balerdbdir,$balerdb,$dbdeploy);

    unless ( $opt_n ) {
        elog_notify ( "Number of stations	$nsta" );
        elog_notify ( "Number of connection attempts $ncon_try" );
        elog_notify ( "Number of connections $ncon " );
        elog_notify (sprintf "Number of baler power requests	%d	total time %8.2f	average %8.2f ", $nbpwr, $tbpwr, $tbpwr/$nbpwr) if ($nbpwr);
        if ($nbpwrf > 0 ) {
            elog_notify (sprintf "Number of baler power failures	%d	total time %8.2f	average %8.2f ", $nbpwrf, $tbpwrf, $tbpwrf/$nbpwrf);
            elog_notify ("@no_power");
        }
        elog_notify (sprintf "Number of http requests	%d	total time %8.2f	average %8.2f ", $nhttp, $thttp, $thttp/$nhttp) if ($nhttp);
        if ($nhttpf > 0 ) {
            elog_notify (sprintf "Number of http failures	%d	total time %8.2f	average %8.2f ", $nhttpf, $thttpf, $thttpf/$nhttpf);
            elog_notify ("@no_info");
        }
        elog_notify (sprintf "Number of baler list requests	%d	total time %8.2f	average %8.2f ", $nlbwf, $tlbwf, $tlbwf/$nlbwf) if ($nlbwf);
        if ($nlbwff > 0 ) {
            elog_notify (sprintf "Number of baler list failures	%d	total time %8.2f	average %8.2f ", $nlbwff, $tlbwff, $tlbwff/$nlbwff);
            elog_notify ("@no_mseed_info");
        }
    }

exit(0);
}

sub proc_target {    # &proc_target($orb,$srcname,$dbops,@netstas) ;
#
#  process q330 target producing results for the list of netstas
#
    my ($orb,$srcname,$dbops,@netstas) = @_;
    my ($netsta, $tmp_baler, $tmp_baler_wf, $good, $inp, $balersn, $balerfirm, $q330sn, $infodir);
    my ($tsh, $teh, $tsb, $teb, $n, $target, $connected, $start, $finish);
    my (@available,@db);
    
        
#  Loop over net_sta
    
    elog_notify ("\nsrcname	$srcname") if ($opt_v||$opt_V) ;
    elog_notify ("processing	@netstas\n") if ($opt_v||$opt_V) ;
    
    $target = $srcname ;
    $target =~ s"/pf/st"" ; 
    
    @db = dbopen($dbops,'r+');
    @db = dblookup(@db,0,"balerlist",0,0);
    

    foreach $netsta (@netstas) {
        $db[3] = dbaddnull(@db);
        dbputv(@db,"dlname",$netsta,"target",$target,"time",now(),"endtime",now());
        ($connected,$q330sn,$inp) = &connected($orb, $srcname, $netsta);
        dbputv(@db,"connection",$connected,"q330sn",$q330sn,"inp",$inp,"endtime",now());
        if ($connected !~ "yes") {
            elog_notify("$netsta is currently not connected - $connected");
            push (@no_connection, $netsta) ;
            next;
        }
        $ncon++ ;
        elog_notify("$netsta is currently connected")  if ($opt_v||$opt_V) ;

#  build database names

        $tmp_baler    = "$balerdbdir/$netsta";
        $tmp_baler_wf = $tmp_baler . "_wf" ;
        elog_notify ("baler	database	$tmp_baler") if $opt_V;
                 
#  turn baler on

        $nbpwr++ ;
        $tsb = now();
        ($good, $url) = &turn_baler_on ($netsta,$target,$cmdorb,@interface) ;
        $teb = now(); 

        if (! $good) {
            $nbpwrf++ ;
            $tbpwrf += ($teb - $tsb);
            push (@no_power, $netsta) ;
            dbputv(@db,"baleron","no","onsecs",($teb - $tsb),"endtime",now());
            next;
        } else {
            $tbpwr += ($teb - $tsb);
            dbputv(@db,"baleron","yes","onsecs",($teb - $tsb),"endtime",now());
        }

        elog_notify ("Dlname	$netsta		url	$url");
                
        $tsh = now();

#  make 5 attempts to get information pages from baler 

        for ($n = 0; $n < 5; $n++ ) {
            $nhttp++;
            ($good , $infodir, $balersn, $balerfirm, $start, $finish) = &get_info($url, $balerinfodir, $tmp_baler, $netsta) ;
            if (! $good) {
                $nhttpf++;
                elog_notify ("Attempt $n to read $url failed. \nretrying\n");
                sleep 20;
                next;
            }
            if ( $good ) {last;}
        }
        
        $teh   =  now();
        if ( $good ) {
            $thttp  += ($teh - $tsh);
            dbputv(@db,"htmlinfo","yes","htmlsecs",($teh - $tsh),"balersn",$balersn,"balerfirm",$balerfirm,"endtime",now());
        } else {
            $thttpf += ($teh - $tsh);
            push (@no_info, $netsta) ;
            dbputv(@db,"htmlinfo","no","htmlsecs",($teh - $tsh),"endtime",now());
        }

#  make 5 attempts to get true listing of available wf data from baler        

        $tsb = now();
        for ($n = 0; $n < 5; $n++ ) {
            $nlbwf++;
            ($good, @available) = &get_wf_list( $url ) ;
            if ( $good ) {
                &mk_baler_wfdisc( $tmp_baler_wf, $infodir, $balersn, $q330sn, \@available );
                last;
            }
            $nlbwff++;
            elog_notify ("Attempt $n to read $url failed. \nretrying\n");
            sleep 20;
        }
        $teb = now();
        if (! $good) {
            elog_notify ("Could not get mseed info from $netsta baler.\nTrying next baler.");
            $tlbwff += ($teb - $tsb) ;
            push (@no_mseed_info, $netsta) ;
            dbputv(@db,"msdinfo","no","msdsecs",($teb - $tsb),"endtime",now());
        } else {
            $tlbwf += ($teb - $tsb) ;
            dbputv(@db,"msdinfo","yes","msdsecs",($teb - $tsb),"data_start",$start,"data_end",$finish,"endtime",now());
        }
    }
    return;
}

sub get_range { # ($netsta, $start, $finish, $tot) = get_range($tmp_baler,\@available) ;
#
#  find range of time available on baler using files.htm 
#  makes a db with these start and end times
#  this gives a general view but is not exact and can miss gaps.
#
    my ($tmp_baler,$ref) = @_ ; 
    my @available = @$ref ; 
    my $netsta = "" ;
    my ($start, $finish) = (-1, -1) ; 
    my ($t, $line, $chan, $time, $endtime, $net, $sta, $wfdisc) ;
    my (@db);
# DT0014__.BHZ   16777216 bytes, from 2003-06-19 20:51:12 to 2003-06-27 07:40:26
    my $tot = 0 ;
    
    $wfdisc = $tmp_baler . ".wfdisc";
    if (-e $wfdisc) {
        elog_notify ("get_range removing $wfdisc") if $opt_V ;
        unlink $wfdisc;
    }
    
#    @db = dbopen($tmp_baler,"r+");
#    @db = dblookup(@db,0,"wfdisc",0,0);
    foreach $line ( @available ) { 
	    chomp($line) ;

	    if ( $line =~ /Directory Listing of (\S+)/ ) { 
	        $netsta = $1 ;
	    }
	    if ( $line =~ /Directory Listing of (\D\D)-(\S+)/ ) { 
	        $net = $1 ; 
	        $sta = $2 ; 
	    }

	    if ( $line =~ /DT.*__\.(\S+)/ ) { 
	        $chan = $1 ; 
	    }

	    if ( $line =~ /(\d+) bytes.*(\d\d\d\d-\d\d-\d\d) (\d\d:\d\d:\d\d) .* (\d\d\d\d-\d\d-\d\d) (\d\d:\d\d:\d\d)/ ) {
	        $tot += $1 ; 

	        $time    = str2epoch( "$2 $3" ) ;
	        $start   = $time if $start < 0 ;
	        $start   = $time if $time  < $start ;

	        $endtime = str2epoch( "$4 $5" ) ; 
	        $finish = $endtime if $finish  < 0 ;
	        $finish = $endtime if $endtime > $finish ;
	        
#	        $db[3] = dbaddnull(@db);
#	        dbputv (@db,"sta",$sta,"chan",$chan,"time",$time,"endtime",$endtime);

	    }
    }
#    dbclose(@db);
    return ($netsta, $start, $finish, $tot ) ;
}

sub mk_baler_wfdisc { # &mk_baler_wfdisc( $tmp_baler_wf, $infodir, $balersn, $q330sn, \@available );
#
#  find range of time available on baler using data from the miniseed files on the baler 
#  makes a db with these start and end times
#  this gives an exact view and should not miss gaps.
#
    my ($tmp_baler_wf, $infodir, $balersn, $q330sn, $ref) = @_ ; 
    my @available = @$ref ; 
    my $netsta = "" ;
    my ($line, $net, $sta, $chan, $time, $endtime, $wfdisc, $sn, $nblocks);
    my ($dirnull,$dfilenull,$stime, $etime, $nb, $check_etime, $check_dir) ;
    my (@db,@dbscr,@dbnull,@lines,@rows);
    
# Data Availability for TA-109C, Q330 Serial 01000008E9048F13
#   UHZ  2005-12-31 23:20:38 - 2006-01-27 18:22:55  175

	open ( TEXT, ">$infodir/wfinfo" ) ; 
	print TEXT @available ; 
	close TEXT ; 

    open ( TEXT, "$infodir/wfinfo" ) ; 
	@lines = <TEXT> ; 
	close TEXT ; 
	
    elog_notify(  "mk_baler_wfdisc	$infodir/wfinfo	$tmp_baler_wf	$balersn	$q330sn" ) if $opt_V ;

    $wfdisc = $tmp_baler_wf . ".wfdisc";
    if (-e $wfdisc) {
        unlink $wfdisc ; 
    }
    @db     = dbopen($tmp_baler_wf,"r+") ;
    @db     = dblookup(@db,0,"wfdisc",0,0) ;
    @dbscr  = dblookup(@db,0,"wfdisc",0,"dbSCRATCH") ;
    @dbnull = dblookup(@db,0,"wfdisc",0,"dbNULL") ;
    ($dirnull,$dfilenull) = dbgetv(@dbnull,"dir","dfile") ;
	foreach $line ( @lines ) { 
	    chomp($line) ;

	    if ( $line =~ /Data Availability for (\D\D)-(\w+), Q330 Serial (\w+)/ ) { 
	        $net = $1 ; 
	        $sta = $2 ; 
	        $sn  = $3 ; 
	        elog_notify("mk_baler_wfdisc	$net	$sta $sn ") if $opt_V ;
	    }

	    if ( $line =~ /(\w+)\s+(\S+\s+\S+)\s+-\s+(\S+\s+\S+)\s+(\d+)/ ) {
	        $chan  = $1 ; 
	        $stime = $2 ; 
	        $etime = $3 ; 
	        $nb    = $4 ; 

	        $time    = str2epoch($stime) ;
	        $endtime = str2epoch($etime) ;	  
	        
	        dbputv (@dbscr,"sta",$sta,"chan",$chan,"time",$time,"endtime",$endtime,"dir","q330_sn:$q330sn","dfile","baler_sn:$balersn");
	        dbadd(@db) ;
#	        @rows=dbmatches(@dbscr,@db,"wf_$sta", "sta", "chan", "time");
#	        if ($#rows == -1 ) {
#	            dbadd(@db) ;
#	            eqlog_notify (sprintf "adding new row	%s	%s %s	%s %s	%s", $sta, $chan, strydtime($time), strydtime($endtime),"q330_sn:$q330sn","baler_sn:$balersn") if $opt_V ;
#	        } elsif ($#rows == 0 ) {
#	            $db[3] = $rows[0];
#	            ($check_etime, $check_dir) = dbgetv(@db,"endtime","dir");
#	            if ($endtime == $check_etime) {
#	                  elog_notify (sprintf "exact match	%s	%s %s	%s", $sta, $chan, strydtime($time), strydtime($endtime)) if $opt_V ;
#	                  if ($check_dir =~ $dirnull) {
#	                      elog_notify (sprintf "updating dir and dfile	%s	%s %s	%s", $sta, $chan, strydtime($time), strydtime($endtime)) if $opt_V ;
#	                      dbputv(@db,"dir","q330_sn:$q330sn","dfile","baler_sn:$balersn") 
#	                  }
#	            }  else {
#	            	  elog_notify (sprintf "diff endtimes	%s	%s %s	%s	%s", $sta, $chan, strydtime($time), strydtime($endtime), strydtime($check_etime)) if $opt_V ;
#	            	  dbputv(@db,"endtime",$endtime,"dir","q330_sn:$q330sn","dfile","baler_sn:$balersn") 
#	            }
#	        } else {
#	            elog_die (" Duplicate rows in $tmp_baler_wf for $sta, $chan, $time, $endtime");
#	        }
	    }
    }
    dbclose(@db);
    return () ;
}

sub get_info { # ($good , $infodir, $balersn, $balerfirm, $start, $finish) = &get_info($url, $balerinfodir, $tmp_baler, $netsta) ;
#
#  download selected web pages defined in parameter file and write to disk 
#
    my ( $url, $balerinfodir, $tmp_baler, $netsta ) = @_ ;
    my ($good, $netsta_tmp,  $infodir, $start, $finish, $tot);
    my (@text) ;
    my ($balersn, $q330sn, $tmp, $balerfirm);
    $balersn = $q330sn = "";
    $infodir = "$balerinfodir/$netsta";
    elog_notify ("unloading informational files into $infodir")  if $opt_V  ;
    my @summary = () ;
    foreach my $info (@info) { 
	    ($good,@text) = &get_text($url, $info) ; 
	    if (! $good ) {
	        return $good ;
	    }
	    makedir($infodir) ; 
	    open ( TEXT, ">$infodir/$info" ) ; 
	    print TEXT @text ; 
	    close TEXT ; 
	    if ( $info eq "baler.htm" ) { 
	        my $text = join("", @text) ; 
	        @text = split("\n", $text) ;
	        foreach (@text) { 
		        if ( /Baler Model/ ) { 
		            s/Software/\n    Software/ ; 
		            push (@summary, $_) ; 
		            ($tmp,$balerfirm) = split("Version:",$_);
		        } else { 
		            push (@summary, $_) if /TAGID|Disk Size|Percent/ ; 
		            if (/TAGID/) { 
                        ($tmp,$balersn) = split(":",$_);
                        $balersn =~ s/^\s+//g ;
                        elog_notify ("balersn $balersn")  if $opt_V  ; 
                    }
		        }
	        }
	    }
	    if ( $info eq "info.htm" ) { 
	        my $text = join("", @text) ; 
	        @text = split("\n", $text) ;
	        foreach (@text) { 
		        push (@summary, $_) if /Q330 Serial Number/ ; 
	        }
	    }
	    
	    if ( $info eq "files.htm" ) { 
            ($netsta_tmp, $start, $finish, $tot) = &get_range($tmp_baler,\@text) ;
	        printf STDERR "\n%10.3f Mbytes data is present in $netsta_tmp from\n\t%s to\n\t%s\n",
	           $tot/1000000, strydtime($start), strydtime($finish)  if ($opt_v||$opt_V) ;
        }
	    

    }
    print STDERR "\n\n    ", join("\n    ", @summary) . "\n\n" if ($opt_v||$opt_V) ;
    
    return ($good , $infodir, $balersn, $balerfirm, $start, $finish) ;
}

sub get_text { # ($good,@text) = &get_text($url, $info) ;
    my ( $url, $name) = @_ ; 
    my ($good, @text);
    $url = "$url/$name" ; 
    ($good, @text) = read_url ($url ) ; 
    grep(s/\r//g, @text) ; 
    return ($good, @text) ; 
}
    
# http://joebaler/RETRIEVE.HTM?SEED=*&MAX=500000&BEG=*&END=*&FILE=&REQ=List+Data+Avail.&DONE=YES

sub get_wf_list { # ($good, @available) = &get_wf_list( $url ) ;
#
#  get range of time available on baler from miniseed listing 
#  this gives the best possible view but and should not miss gaps.
#
    my ( $url, $tmp_baler ) = @_ ; 
    my ( $good, $time, $stime, $qt0 );
    my ( @text, @db );

    $url = "$url/RETRIEVE.HTM?SEED=%3F%3F%3F&MAX=&BEG=&END=&STN=&FILE=&REQ=List+Data+Avail.&DONE=YES" ; 
    ($good, @text) = read_url ($url ) ; 
    grep(s/\r//g, @text) ; 
    return ($good, @text) ; 
}

sub read_url { #     ($good, @text) = &read_url ( $url ) ;
#
#  reads html files from baler
#
    my ( $url ) = @_ ;
    my ( $good ) ;
    $good = 1 ;
    elog_notify ( "read_url	url='$url' ") if $opt_V ;
    my $request = HTTP::Request->new(GET => $url ) ;
    my $response = $Ua->request($request);
    if (! $response->is_success) {
	    my $msg = $response->message ;
	    elog_notify( $msg ) ;
        $good = 0;
    }
    return ($good, &htmltext ( $response )) ; 
}

sub htmltext { # &htmltext ( $response ) ;
    my ( $response ) = @_ ;
    local ( @Text ) ;
    my $p = HTML::Parser->new('text_h' => [\&text, "self, text, is_cdata"]) ;  
    $p->parse($response->content);
    $p->eof;                 
    return @Text ; 
}

sub text {
    my($self, $text, $is_cdata) = @_;
    if ( $text ne "" ) { 
	    $text = decode_entities($text) ;
	    push @Text, $text ; 
    }
}


sub run_dlcmd { # ($dispostion, $url) = &run_dlcmd ($netsta, $target, $interface) ;
#
#  excecutes dlcmd command
#
    my ($netsta,$target,$interface) = @_ ;
    my ($key,$line,$ref,$disposition,$url,$cmd,$pfname);
    my (@keys,@dlcmdpf,@dlinfo);
    
    $pfname = $netsta . $interface;
#
#  execute dlcmd and return ouput to @dlcmdpf
#
    $cmd = "dlcmd $cmdorb $target q330 $netsta control sbpwr $interface poweron 600";
    elog_notify ( $cmd ) if $opt_V;
    open (DLCMD, "$cmd |") ; 
    @dlcmdpf = <DLCMD> ; 
    close DLCMD ;
#
#  convert @dlcmdpf into Pf object and process
#    
    $line = join( "", @dlcmdpf ) ;
    pfnew ( $pfname );
    pfcompile ( $line, $pfname ) ;    
    $ref  = pfget( $pfname, "" ) ;
    @keys = sort ( keys  %$ref );

    $disposition = $ref->{$keys[0]}{"disposition"};
    if ($disposition =~ /done/) {
        @dlinfo = split(":",$ref->{$keys[0]}{"dlcom"});
        $url = "http://$dlinfo[1]:5354" ;
    }
    return ($disposition, $url);
}

sub getpars { # &getpars();
#
#  get parameters from pf file
#
    my ($ref);

    $balerdb       = pfget ( $Pf, 'balerdb' ) ;
    $balerdbdir    = pfget ( $Pf, 'balerdbdir' ) ;
    $balerinfodir  = pfget ( $Pf, 'balerinfodir' ) ;

    $ref           = pfget ( $Pf, 'all_chan_skip' ) ;
    @all_chan_skip = @$ref ;

    $ref           = pfget ( $Pf, 'info' ) ;
    @info          = @$ref ;

    $ref           = pfget ( $Pf, 'interface' ) ;
    @interface     = @$ref ;
    
    print STDERR "    balerdb		$balerdb\n"      if ($opt_v||$opt_V) ;
    print STDERR "    balerdbdir		$balerdbdir\n"      if ($opt_v||$opt_V) ;
    print STDERR "    balerinfodir	$balerinfodir\n"      if ($opt_v||$opt_V) ;
    print STDERR "    all_chan_skip	@all_chan_skip\n"      if ($opt_v||$opt_V) ;
    print STDERR "    info		@info\n"      if ($opt_v||$opt_V) ;
    print STDERR "    interface		@interface\n" if ($opt_v||$opt_V) ;

    makedir($balerdbdir);
    makedir($balerinfodir);
    return;
}

sub turn_baler_on { # $url = &turn_baler_on ($netsta, $target) ;
#
#  turns baler power on
#
    my ($netsta,$target) = @_;
    my ($interface,$disposition, $url) ;
    my $good = 1 ;
    
    foreach $interface (@interface) {
        ($disposition, $url) = &run_dlcmd ($netsta,$target,$interface) ;    
        elog_notify ( "Interface	$interface	Disposition	$disposition	url	$url ") if $opt_V;
        if ($disposition =~ /done/) {last;}
    }
    if ($disposition !~ /done/) {
        elog_notify ("$netsta baler could not be turned on");
        $good = 0;
    }
  
    if ($good) {
        elog_notify ("turn_baler_on	- sleeping 10 ") if $opt_V ;
        sleep (10);
    }
    return ($good, $url);
}    

sub build_balerall_db { # &build_balerall_db($balerdbdir,$balerdb,$dbdeploy);
#
#  combines individual baler dbs into one combined db
#
    my ($balerdbdir,$balerdb,$dbdeploy) = @_;
    my ($allwfdisc,$cmd,$chan_list,$dir,$base,$suf,$dbpath);
    
    $allwfdisc = $balerdb . ".wfdisc";
    if (-e $balerdb) {
        elog_notify ( "build_balerall_db removing $balerdb ")  if $opt_V;
        unlink $balerdb;
    }
    
    if (-e $allwfdisc) {
        elog_notify ( "build_balerall_db removing $allwfdisc ")  if $opt_V;
        unlink $allwfdisc;
    }
    
    $cmd = "cat $balerdbdir/*_wf.wfdisc >> $allwfdisc" if ($opt_v||$opt_V) ;
    elog_notify ( "$cmd \n") if ($opt_v||$opt_V) ;
    system ($cmd) ;
    
    $chan_list = join("|",@all_chan_skip);
    $cmd = "dbsubset $allwfdisc \"chan =~ /$chan_list/ \" | dbdelete - ";
    elog_notify ( "build_balerall_db	$cmd ") if ($opt_v||$opt_V) ;
    
    ($dir,$base,$suf) = parsepath(abspath($dbdeploy)) ;
    $dbpath  = $dir . "/{" . $base . "}";
    
    cssdescriptor($balerdb,$dbpath,"","") ;
    system ($cmd) ;
    
    return;
}

sub deploy_fix { # &deploy_fix($balerdbdir,$dbdeploy);
#
#  removes data from baler before installation time
#
    my ($balerdbdir,$dbdeploy) = @_;
    my ($wfdisc,$dlsta,$snet,$sta,$chan,$time,$endtime,$dbwf,$db,$equip_install,$row);
    my (@wfdiscs,@dbdeploy,@dbscr,@dbwf,@dbwscr,@rows);

    elog_notify("deploy_fix( $balerdbdir, $dbdeploy )");

    @dbdeploy = dbopen($dbdeploy,"r");
    @dbdeploy = dblookup(@dbdeploy,0,"deployment",0,0);
    @dbscr    = dblookup(@dbdeploy,0,"deployment",0,"dbSCRATCH");
    
    if (! dbquery ( @dbdeploy, "dbTABLE_PRESENT" )) {
       elog_notify ("No deployment table in dbdeploy for $dbdeploy!");
       return;
    } 
    
    opendir(DIR,$balerdbdir) ; 
    @wfdiscs = grep { /_wf\.wfdisc/ } readdir(DIR);
    closedir(DIR);
    
    foreach $wfdisc (sort(@wfdiscs)) {
        $dlsta = $wfdisc;
        $dlsta =~ s/_wf.wfdisc//;
        $snet  = $dlsta;
        $snet  =~ s/_.*//;
        $sta   = $dlsta ;
        $sta   =~ s/.*_//;
        $db    = $wfdisc ;
        $db    =~ s/\.wfdisc//;
        $db    = $balerdbdir . "/$db" ;
        
        dbputv(@dbscr,"snet",$snet,"sta",$sta);
        @rows = dbmatches(@dbscr,@dbdeploy,"deploy","snet","sta");
        $dbdeploy[3] = $rows[0];
        $equip_install = dbgetv(@dbdeploy,"equip_install");
        elog_notify( sprintf ("\n$dlsta installation time -  %s\n",strydtime($equip_install))) if ($opt_v||$opt_V) ;
        
        
        @dbwf = dbopen($db,"r+");
        @dbwf = dblookup(@dbwf,0,"wfdisc",0,0);
        @dbwscr = dblookup(@dbwf,0,"wfdisc",0,"dbSCRATCH");
        dbputv(@dbwscr,"sta",$sta,"time",($equip_install-(2*365*86400)),"endtime",$equip_install);

        @rows = dbmatches(@dbwscr,@dbwf,$sta,"sta","time::endtime");
        foreach $row (@rows) {
            $dbwf[3] = $row;
            ($chan,$time,$endtime) = dbgetv(@dbwf,qw (chan time endtime));
            printf STDERR "	$sta	$chan	%s	%s \n",strydtime($time),strydtime($endtime) if ($opt_v||$opt_V) ;
            if ($endtime < $equip_install) { dbmark(@dbwf); }
        }

        dbcrunch(@dbwf);
        dbclose(@dbwf);
    }
        
    return;
}

sub src_stas {# @netstas = &src_stas($orb, $srcname);
#
# return a list of net_stas which are to be processed from a specific q330 target sourcename
#
    my ($orb, $srcname) = @_ ;
    my ($src, $pktid, $pkttime, $pkt, $nbytes, $result, $chan, $loc, $when);
    my ($target, $orbname, $pf, $suffix, $subcode, $type, $desc) ;
    my (@netsta,@netstas);
#
# get proper pf packet
#

    orbselect ( $orb, $srcname ) ;
    ($pktid, $srcname, $pkttime, $pkt, $nbytes) = orbget ( $orb, "ORBNEWEST" ) ;
    elog_notify (sprintf "\nsrc_stas	%s	%s \n", $srcname, strydtime($pkttime)) if $opt_V;
    if (!defined $pktid) {
        next ;
    }
    if ( $nbytes == 0 ) {
        next ;
    }
    ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

    if ( $result ne "Pkt_pf" ) {
        if ( $opt_v ) {
            elog_notify( "Received a $result, skipping\n" );
        }
        next;
    }
#
#  extract list of stations from pf packet
#
    ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
    ($type, $desc) = $pkt->PacketType() ;

    $pf = $pkt->pf ;

    if ( defined $pf ) {
        $ref = pfget($pf, "");
        my @stas = sort keys %{$ref->{dls}};
        push @netstas, @stas;
    }
    @netstas = sort(@netstas);

    elog_notify ("@netstas")  if $opt_V;
#
#  modify by station selection.
#
    if ($opt_s) {
        foreach $sta (@netstas) {
            push (@netsta,$sta) if ($sta =~ /$opt_s/);
        }
        elog_notify ("src_stas	@netsta") if $opt_V;;
        @netstas = @netsta;
    }
    return @netstas;
}


sub connected {# ($connected,$sn,$inp) = &connected($orb, $srcname, $netsta);
#
# determine if $net_sta is currently connected from from a specific q330 target sourcename
#
    my ($orb, $srcname, $netsta) = @_ ;
    my ($src, $pktid, $pkttime, $pkt, $nbytes, $result, $chan, $loc, $when) ;
    my ($target, $orbname, $pf, $suffix, $subcode, $type, $desc) ;
    my ($connected,$sn,$inp) ;
    my (@netsta,@netstas) ;

    $connected = 0;
#
# get proper pf packet
#

    orbselect ( $orb, $srcname ) ;
    ($pktid, $srcname, $pkttime, $pkt, $nbytes) = orbget ( $orb, "ORBNEWEST" ) ;
    elog_notify (sprintf "\nsrc_stas	%s	%s \n", $srcname, strydtime($pkttime)) if $opt_V;
    if (!defined $pktid) {
        next ;
    }
    if ( $nbytes == 0 ) {
        next ;
    }
    ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

    if ( $result ne "Pkt_pf" ) {
        if ( $opt_v ) {
            elog_notify( "Received a $result, skipping\n" );
        }
        next;
    }
#
#  extract info from pf packet
#
    ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
    ($type, $desc) = $pkt->PacketType() ;

    $pf = $pkt->pf ;

    if ( defined $pf ) {
        $ref = pfget($pf, "");
        $connected = $ref->{dls}{$netsta}{"con"};
        $inp = $ref->{dls}{$netsta}{"inp"};
        $sn = $ref->{dls}{$netsta}{"sn"};
        elog_notify("$netsta connection status - $connected	$sn	$inp") if $opt_V;
    }
    return ($connected,$sn,$inp);
}
