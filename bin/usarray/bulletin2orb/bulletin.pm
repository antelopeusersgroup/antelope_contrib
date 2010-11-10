package bulletin ;

require Exporter;
require DynaLoader;
use Carp;

@ISA = ('Exporter');

@EXPORT=qw( 	collect_htmltagged
		collect_htmlnotags
		collect_text 	
		collect_file 	
		collect_ftp
		collect_search_post
		collect_search_qf
		collect_dbsubset 
		parse_CUBE
		parse_EMSC
		parse_recenteqs
		parse_AEIC
		parse_finger
		parse_HYPO2000
		parse_NESN
		parse_NBEsearch 
		parse_NBEwww 
		parse_mtech_hypo71
		parse_mtechAUTO 
		parse_ehdf 
		parse_mchdr
		parse_uussLIST  
		parse_uwcard
		parse_dbsubset 
	) ; 

use warnings ; 
use strict 'vars' ;
use Datascope;
use HTML::Parser ;
require HTTP::Request;
use Encode;
use LWP ;
use LWP::Simple; 
use LWP::UserAgent;
use URI::URL; 
use HTTP::Request::Common; 
use HTML::Entities ;
use Net::FTP ;


our (%magnitude_info, %pfinfo, %parsed_info) = () ;
our ($myauth,$auth,$myenddate,$enddate)  = "" ;
our ($mlid, $mbid, $msid, $mb, $ms, $ml, $mn, $mc, $mbsta, $mssta, $comment, $depthinfo) = "" ;
our ($ftp,$myurl,$localdir,$algorithm, $file, $getnetmag) = "" ;
our ($lat, $lon, $depth, $or_time, $nsta, $magid, $magtype, $magnitude, $mag_uncert, $mag_type, $mag, $etype, $dtype) = "" ;
our ($latNS, $lonEW, $latd, $latm, $lond, $lonm, $E, $qual, $extevid, $evid, $erh, $erz, $rms, $dmin, $gap, $ndp, $ndef, $nph) = "" ;
our ($mintime, $maxtime, $mytime, $hr, $hour, $hm, $ymd, $hms, $min, $day, $sec, $month, $yr, $year, $n) = ""  ;
our ($syr, $smo, $sday, $shr, $smin, $ssec, $eyr, $emo, $eday, $ehr, $emin, $esec)  = "" ;
our ($listref, $remote_host, $partcnt, $ok)  = "" ;
our (@listing, @convert_list, @lines, $upd_yr, $upd_time, $num_mo) = "" ;
our (@mt, @mv, @ma) = "" ;
our (@db, @dborigin, @dbnetmag, @dbevent, @dbj)  = "" ;
#our (@textout, @out) = "" ;
# this may cause multi-ftp grabs to fail miserably
our (@textout) = "" ;
our ($TZ, $Q) = "" ;



# this module contains the bulletin collectors and parsers
# used by the bull2orb program
#
# Created July/August 2009
# Jennifer Eakins
# jeakins@ucsd.edu
#

#
# generalized collectors under here:
#	- collect_dbmaster
#	- collect_ftp
#	- collect_htmlnotags
#	- collect_htmltagged
#	- collect_search_qf
#	- collect_search_post
#	- collect_text
#


sub collect_ftp	{		

  ($listref,%pfinfo) = @_ ;

  my $ftphost	= $pfinfo{ftphost};
  my $ftpdir	= $pfinfo{ftpdir};
  my $ftpmatch	= $pfinfo{ftpmatch};
  my $linestart	= $pfinfo{linestart};
  my $linelength = (defined $pfinfo{linelength}) ? $pfinfo{linelength} : 0 ;
  my $extract_handler = ( defined $pfinfo{'extractor'} ) ? "extract_" . $pfinfo{extractor} : 0   ;

  $localdir = $pfinfo{localdir};
  my $account	= $pfinfo{account} ;
  $myauth	= $pfinfo{auth};

  my ($mb, $ms, $partcnt, $ok, $ymd, $hour  ) ; 
  my ($lat, $lon, $depth, $or_time) ;

  my @outlist ;
  my @saved ;

  $ftp = Net::FTP->new("$ftphost") or elog_complain("Can't connect: $@\n") ;
  if (!$ftp) {
    elog_complain("Not able to complete bulletin collection via ftp for: $ftphost\n") ;
    next; 
  }
  $ftp->login("anonymous",$account) or  elog_complain("Couldn't login\n") ;
  if (!$ftp) {
    elog_complain("Not able to complete bulletin collection via anonymous login for: $ftphost\n") ;
    next; 
  }
  my $remote_pwd = $ftp->pwd ;
  elog_notify ("CWD on $ftphost is: $remote_pwd\n");
  $ftp->cwd("$ftpdir") or elog_complain("Couldn't change to remote dir $ftpdir\n")  ;
  $ftp->pasv() ;
  $remote_pwd = $ftp->pwd ;
  elog_notify ("CWD on $ftphost is now: $remote_pwd\n");
  $ftp->binary ;
  @listing = $ftp->dir  or  elog_complain("Couldn't get file listing on remote host\n") ;
  check_remote_list($ftpmatch, @listing);	# check to see if remote site has newer file than local, 
				# recovers file with get_remote_ftpfile,
				# populates @convert_list with filenames that need to be parsed/converted
  $ftp-> quit ;

# files are collected, now have to populate @saved
# open each file and make sure first line starts with "GS", or "E", or "whatever"

  for (@convert_list) {
    elog_notify("File is: $_ \n") ;
    my $tmpfilename = $_ ;

    open FILE, "<$_";
    while (<FILE>) {
      my $line = $_ ;
      next if ( $line !~/^($linestart)/ ) ;	
      next if ( ($linelength > 0 ) && (length($line) < $linelength) ) ; 
      push @saved, $line ;	# put all lines from file into @saved
    }

    my $x = $#saved + 1 ;
    elog_notify("   Found $x events from recovered file\(s\)\n");
    close FILE ;
    @saved = &$extract_handler($tmpfilename) if ($extract_handler) ;
    @outlist = list2search(@saved) ;
  }

  return @outlist ;

} 

sub collect_search_get {

  ($listref,%pfinfo) = @_ ;
  my $myndays = $pfinfo{ndays} ;
  my $myenddate = $pfinfo{enddate} ;
  my $myurl = $pfinfo{url} ;
  $myauth = $pfinfo{auth};

  my $get_handler = "get_" . $pfinfo{parser} ;
  my $extract_handler = "extract_" . $pfinfo{extractor} ; 
  
  my $tmpfile = "/tmp/search".$pfinfo{parser} ;
  my @textout ;
  my @outlist ;
  my @saved ;

  if ($myenddate) {
    our $maxtime = str2epoch($myenddate);  
  } else {
    our $maxtime = time()  ;
  }

  our $mintime = $maxtime - ($myndays * 86400) ;

  our ($syr, $smo, $sday, $shr, $smin, $ssec) = split(/\s+/,epoch2str($mintime,"%Y %m %d %H %M %S"));
  our ($eyr, $emo, $eday, $ehr, $emin, $esec) = split(/\s+/,epoch2str($maxtime,"%Y %m %d %H %M %S"));

  $mintime = epoch2str($mintime, "%Y/%m/%d,%H:%M:%S") ;
  $maxtime = epoch2str($maxtime, "%Y/%m/%d,%H:%M:%S") ;

  my $ua = new LWP::UserAgent; 

  my $doc = get(&$get_handler($myurl));

  open(TEMP, ">$tmpfile") || die "Can't open tmp file: $tmpfile\n";

  if (defined $doc) {
     print TEMP $doc;
  } else {
     print TEMP "get of $myurl timed out\n" ;
     elog_complain("Couldn't get info from $myurl\n") unless defined $doc;
  }

  close(TEMP) ;

  @textout = &$extract_handler($tmpfile);
  unlink $tmpfile; 

  my $m = @textout;

  @outlist = list2search(@textout) ;
  return @outlist ;

}

sub collect_search_qf {		# formerly collect_scecHYPO2000

  ($listref,%pfinfo) = @_ ;
  my $myndays = $pfinfo{ndays} ;
  my $myenddate = $pfinfo{enddate} ;
  my $myurl = $pfinfo{url} ;
  $myauth = $pfinfo{auth};

  my $postqf_handler = "postqf_" . $pfinfo{parser} ;
  my $extract_handler = "extract_" . $pfinfo{extractor} ; 
  
  my $tmpfile = "/tmp/search".$pfinfo{parser} ;
  my @textout ;
  my @outlist ;
  my @saved ;

  if ($myenddate) {
    our $maxtime = str2epoch($myenddate);  
  } else {
    our $maxtime = time()  ;
  }
  
  our $mintime = $maxtime - ($myndays * 86400) ;

  our ($syr, $smo, $sday, $shr, $smin, $ssec) = split(/\s+/,epoch2str($mintime,"%Y %m %d %H %M %S"));
  our ($eyr, $emo, $eday, $ehr, $emin, $esec) = split(/\s+/,epoch2str($maxtime,"%Y %m %d %H %M %S"));

  $mintime = epoch2str($mintime, "%Y/%m/%d,%H:%M:%S") ;
  $maxtime = epoch2str($maxtime, "%Y/%m/%d,%H:%M:%S") ;

  my $ua = new LWP::UserAgent; 

  my $doc = get(&$postqf_handler($myurl));

  open(TEMP, ">$tmpfile") || die "Can't open tmp file: $tmpfile\n";

  if (defined $doc) {
     print TEMP $doc;
  } else {
     print TEMP "get of $myurl timed out\n" ;
     elog_complain("Couldn't get info from $myurl\n") unless defined $doc;
  }

  close(TEMP) ;

  @textout = &$extract_handler($tmpfile);
  unlink $tmpfile; 

  my $m = @textout;

  @outlist = list2search(@textout) ;
  return @outlist ;

}

sub collect_search_post  {	# 

  ($listref,%pfinfo) = @_ ;
  my $myndays = $pfinfo{ndays} ;
  my $myenddate = $pfinfo{enddate} ;
  $myauth = $pfinfo{auth};
  my $tmpfile = "/tmp/search_post$myauth" ;
  my $postreq_handler = "postreq_" . $pfinfo{parser} ;
  my $extract_handler = "extract_" . $pfinfo{extractor} ; 
  
  my @outlist ;
  my @saved ;
  my @textout;

# need to share with posrtreq_handler

  our $myurl = $pfinfo{url} ;

  if ($myenddate) {
    our $maxtime = str2epoch($myenddate);  
  } else {
    our $maxtime = time()  ;
  }
  
  our $mintime = $maxtime - ($myndays * 86400) ;
  
  our ($syr, $smo, $sday, $shr, $smin, $ssec) = split(/\s+/,epoch2str($mintime,"%Y %m %d %H %M %S"));
  our ($eyr, $emo, $eday, $ehr, $emin, $esec) = split(/\s+/,epoch2str($maxtime,"%Y %m %d %H %M %S"));

  $mintime = epoch2str($mintime, "%Y/%m/%d,%H:%M:%S") ;
  $maxtime = epoch2str($maxtime, "%Y/%m/%d,%H:%M:%S") ;

  my $ua = new LWP::UserAgent; 

  $ua->agent("PerlLWP/0.1") ;

# formulate correct POST request for this specific instance/bulletin
  my $req = &$postreq_handler  ;

  $req ->content_type('application/x-www-form-urlencoded');
  
  open(TEMP, ">$tmpfile") || die "Can't open tmp file: $tmpfile\n";

  my $res = $ua->request($req,$tmpfile);
  $res = $ua->request($req);
 
  close(TEMP);

  if (! $res->is_success) {
     my $err = $res->error_as_HTML ;
     elog_complain ("$err\n" ) ;
  }

  @textout = &$extract_handler($tmpfile);
  unlink $tmpfile; 

  my $m = @textout;

  @outlist = list2search(@textout) ;
  return @outlist ;

}

sub collect_text {	# collect_text(@ref, %pfinfo)  # formerly collect_qedCUBE(@HoA{qedCUBE})

# completely stolen from Danny's example USGScube2orb

  ($listref,%pfinfo) = @_ ;

  my $myurl = $pfinfo{url} ;
  $myauth	= $pfinfo{auth};

  my $content = get ( $myurl ) ; 

  my @outlist ;

  foreach my $line (split /^/, $content) {
    my $match = 0;
    foreach my $oldline (@{$listref}) {
        if ($line eq $oldline) {
            $match = 1;
            last;
        }
    }   
    if ($match == 0) {
        push @{$listref}, $line ;
        push @outlist, $line ;
    }   
  }

  $n = @outlist;
  elog_notify ("\t $n new origins to be included\n\n") ; 

  return @outlist ;

}

sub collect_file	{		

  ($listref,%pfinfo) = @_ ;

  my $myfile	= $pfinfo{file} ;
  $myauth	= $pfinfo{auth};
  my $linestart	= $pfinfo{linestart};
  my $extract_handler = ( defined $pfinfo{'extractor'} ) ? "extract_" . $pfinfo{extractor} : 0   ;

  my ($mb, $ms, $partcnt, $ok, $ymd, $hour  ) ; 
  my ($lat, $lon, $depth, $or_time) ;

  my @outlist ;
  my @saved ;

# now have to populate @saved
# open each file and make sure first line starts with "GS", or "E", or "whatever"

  elog_notify("File is: $myfile \n") ;
  open FILE, "<$myfile";
  while (<FILE>) {
      my $line = $_ ;
      next if ( $line !~/^($linestart)/ ) ;	
#      next if ( ($linelength > 0 ) && (length($line) < $linelength) ) ; 
      push @saved, $line ;	# put all lines from file into @saved
  }

  my $x = $#saved + 1 ;
  elog_notify("   Found $x events from recovered file\(s\)\n");
  close FILE ;
  @saved = &$extract_handler(@saved) if ($extract_handler) ;
  @outlist = list2search(@saved) ;

  return @outlist ;

} 

sub collect_dbsubset {		# 

  ($listref,%pfinfo) = @_ ;

  my $mydb	= $pfinfo{db};
  $myauth	= $pfinfo{auth};
  my $myndays = $pfinfo{ndays} ;
  my $myenddate = $pfinfo{enddate} ;
  my $authsubset	= $pfinfo{authsubset} ;

  my $postreq_handler = "postreq_" . $pfinfo{parser} ;
  
  my @outlist ;
  my @saved ;
  my @textout;
  my $nrecs ;

  if ($myenddate) {
    our $maxtime = str2epoch($myenddate);  
  } else {
    our $maxtime = time()  ;
  }
  
  our $mintime = $maxtime - ($myndays * 86400) ;

  my $timesubset = "time>='$mintime' && time<'$maxtime'" ;
  my ($lat, $lon, $depth, $or_time) ;

  our ($mb, $ms, $ml) ;

  #@db	= dbopen($mydb, "r") || croak ("Can't open database: $mydb");
  @db	= dbopen($mydb, "r") ; 
  @dborigin = dblookup (@db, "", "origin", "", "") ;
  @dbnetmag = dblookup (@db, "", "netmag", "", "") ;
  @dbevent  = dblookup (@db, "", "event",  "", "") ;

  @dborigin  = dbsubset ( @dborigin, $authsubset ) ;
  @dborigin  = dbsubset ( @dborigin, $timesubset ) if ($myndays >= 1) ;
  @dborigin  = dbsort ( @dborigin, "time") ;

  if (dbquery ( @dbnetmag, "dbRECORD_COUNT") ) {
     @dbj = dbjoin ( @dborigin, @dbnetmag, -outer, "orid") ;
     $nrecs = dbquery( @dbj, "dbRECORD_COUNT") ;
     my $getnetmag = 1 ;
  } else {
     $nrecs = dbquery( @dborigin, "dbRECORD_COUNT") ;
     my $getnetmag = 0 ;
  }

#  ARGGG.  This doesn't work for events that have more than one solution!
#  You get a new origin and event row for each one rather than a single
#  event row and multiple origin rows.  NO WORKAROUND!!!


  for (my $row = 0; $row<$nrecs; $row++ ) {
      if ($getnetmag) {
        $dbj[3] = $row ;
	($lat, $lon, $depth, $or_time, $etype, $dtype, $mb, $ms, $ml, $ndef, $ndp, $algorithm, $magnitude, $mag_uncert, $magtype, $nsta, $auth) = dbgetv( @dbj, qw (lat lon depth origin.time etype dtype mb ms ml ndef ndp algorithm magnitude uncertainty magtype nsta origin.auth) ) ;
      } else {
        $dborigin[3] = $row ;
	($lat, $lon, $depth, $or_time, $etype, $dtype, $mb, $ms, $ml, $ndef, $ndp, $algorithm, $auth) = dbgetv( @dborigin, qw (lat lon depth origin.time etype dtype mb ms ml ndef ndp algorithm origin.auth) ) ;
	($magnitude, $mag_uncert, $magtype, $nsta) = qw (-99.99 -1.0 - -1 )  ;

      }
      $ndef = 0 if ($ndef == -1) ;
      push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $etype, $dtype, $mb, $ms, $ml, $ndef, $ndp, $algorithm, $magnitude, $mag_uncert, $magtype, $nsta, $auth ))) ;
# HERE - when using dbsubset option, no origin info is displayed if verbose option is selected.  You may want to take this out?
#      elog_notify sprintf "      %s %7.3f %8.3f %5.0f   %3.1f\n", strydtime($or_time), $lat, $lon, $depth, $magnitude, $magtype ; 
 
  }

  dbclose(@db) ;

  @outlist = list2search(@saved) ;
  return @outlist ;

}

sub collect_htmlnotags {		# 

  ($listref,%pfinfo) = @_ ;

  my $myurl = $pfinfo{url};
  $myauth = $pfinfo{auth};

  my ($mb, $ms, $partcnt, $ok, $ymd, $hour  ) ; 
  my ($lat, $lon, $depth, $or_time) ;

  my @outlist ;

  my $extract_handler = "extract_" . $pfinfo{extractor} ; 
  my $tmpfile = "/tmp/notags".$pfinfo{parser} ;

  open(TEMP, ">$tmpfile") || die "Can't open tmp file: $tmpfile\n" ;

  my $doc = get($myurl);
  if (defined $doc) {
     print TEMP $doc;
  } else {
     print TEMP "get of $myurl timed out\n" ;
     elog_complain("Couldn't get info from $myurl\n") unless defined $doc;
  }

  close(TEMP) ;

  @textout = &$extract_handler($tmpfile);
  unlink $tmpfile; 

  my $m = @textout;

  @outlist = list2search(@textout) ;
  return @outlist ;

}

sub collect_htmltagged {		# 

  ($listref,%pfinfo) = @_ ;

  my $myurl = $pfinfo{url};
  my $match = $pfinfo{match};
  my $reject = $pfinfo{reject};
  $myauth = $pfinfo{auth};

  my ($mb, $ms, $partcnt, $ymd, $hour  ) ; 
  my ($lat, $lon, $depth, $or_time) ;

  my @textout ;
  my @outlist ;

#  my $ok = 0 ;
  $ok = 0 ;

  elog_notify("Attempting to connect to $myurl\n");

  @textout = read_url($myurl) ;

  my $extract_handler = "extract_" . $pfinfo{extractor} ; 
  @textout = &$extract_handler(@textout);

  my $m = @textout;
  elog_notify("Nlines extracted from $extract_handler: $m\n");

  @outlist = list2search(@textout) ;

  return @outlist ;
}

sub parse_recenteqs {	# 

  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;
    my $line = shift ; 

    ($lat, $lon, $depth, $or_time, $mag, $magtype, $etype ) = split(",",$line) ;

    $magtype = lc $magtype if ($magtype =~ /[A-Z]{2}/) ; 

    %magnitude_info = ( $magtype, $mag ) ;

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

# populate hash of parsed info for use in create_$table subs 

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> "0" ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> $etype   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> $mb , 
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> $magtype ,
                magnitude	=> $mag,
		auth		=> $myauth,
		algorithm	=> "latest_eqs" 
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}

sub parse_EMSC {	

  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;
    my $line = shift ; 

    ($lat, $lon, $depth, $or_time, $mag, $magtype) = split(",",$line) ;

    %magnitude_info = ( $magtype, $mag ) if ($magtype !~ /-/) ;

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> "0" ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> $mb , 
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> $magtype ,
                magnitude	=> $mag,
		auth		=> $myauth,
		algorithm	=> "-" 
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}

sub parse_AEIC   {	

    my $line = shift ; 
    our %magnitude_info = () ;
    my  $mb  = "" ;
    my  $ms  = "" ;
    my  $ml  = "" ;

    ($lat, $lon, $depth, $or_time, $mb, $ml, $ms) = split(",",$line) ;

   $magnitude_info{mb} = $mb if ($mb =~ /\d/ ) ;
   $magnitude_info{ml} = $ml if ($ml =~ /\d/ ) ;
   $magnitude_info{ms} = $ms if ($ms =~ /\d/ ) ;

   ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;


#    while( my ($k, $v) = each %magnitude_info) {
#        print "Before adding to parsed_info ...  ";
#        print "key: $k, value: $v.\n";
#    }

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> "0" ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
		auth		=> $myauth , 
		algorithm	=> "unk" 
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}


sub parse_NBEwww {	# parse_NBEwww( @originlist)

    my $line = shift ; 
    our %magnitude_info = () ;
    our $mb  = "" ;
    our $ms  = "" ;
    our $ml  = "" ;

    ($lat, $lon, $depth, $or_time, $mag_type, $mag, $nph, $comment, $evid) = split(",",$line) ;

    $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "ml"  ;
    %magnitude_info = ( $magtype, $mag ) ;

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

# this format only reports a single magnitude

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $nph ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "0" ,
                mag_uncert	=> "0" ,
                magid		=> $magid ,
                mb		=> "-999.0" ,
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> "-999.0" ,
                msid		=> $msid ,
                magtype		=> "ml",
                magnitude	=> $ml,
		auth		=> $myauth . ":" . $evid, 
		algorithm	=> $comment 
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}

sub parse_mtechAUTO {	# parse_mtechAUTO( @originlist)

    my $line = shift ; 

    our %magnitude_info = () ;
    our $mb  = "" ;
    our $ms  = "" ;
    our $ml  = "" ;

    ($lat, $lon, $depth, $or_time, $mag, $magtype, $nph, $evid, $qual) = split(",",$line) ;

    %magnitude_info = ( $magtype, $mag ) ;

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

# qual symbols:

    if ($qual eq 'A' ) {
       $algorithm = "auto-best";
    } elsif ($qual eq 'B' ) {
       $algorithm = "auto";
    } elsif ($qual eq 'C' ) {
       $algorithm = "auto";
    } elsif ($qual eq 'D' ) {
       $algorithm = "auto-poor";
    } else {
       $algorithm = "auto";
    }

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $nph ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> "-999.0" ,
                mbid		=> $mbid ,
                ml		=> "-999.0" , 
                mlid		=> $mlid ,
                ms		=> "-999.0" ,
                msid		=> $msid ,
                magtype		=> $magtype,
                magnitude	=> $mag,
		auth		=> $myauth . ":" . $evid, 
		algorithm	=> $algorithm 
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}

sub parse_dbsubset	{	# 

    my $line = shift ; 

    our %magnitude_info = () ;
    our $mb  = "" ;
    our $ms  = "" ;
    our $ml  = "" ;

    ($lat, $lon, $depth, $or_time, $etype, $dtype, $mb, $ms, $ml, $ndef, $ndp, $algorithm, $magnitude, $mag_uncert, $magtype, $nsta, $auth ) = split (",", $line)  ;

  if ($magtype =~ /-/) {
    if ($ml != -999.0) {
	$magtype = 'ml' ;
	$magnitude = $ml ;
        %magnitude_info = ( $magtype, $magnitude ) ;
    }
    if ($mb != -999.0) {
	$magtype = 'mb' ;
	$magnitude = $mb ;
        %magnitude_info = ( $magtype, $magnitude ) ;
    }
    if ($ms != -999.0) {
	$magtype = 'ms' ;
	$magnitude = $ms ;
        %magnitude_info = ( $magtype, $magnitude ) ;
    }

  }

    if ($magtype ne "-") {
      %magnitude_info = ( $magtype, $magnitude ) ;
    }

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $ndef ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> $ndp   ,
                etype		=> $etype   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> $dtype   ,
                mag_nsta        => $nsta ,
                mag_uncert	=> $mag_uncert ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> $magtype ,
                magnitude	=> $mag ,
		auth		=> $auth , 
		algorithm	=> $algorithm  
    ) ;   


  return (\%parsed_info, \%magnitude_info) ;

}

sub parse_uussLIST  {	# parse_uussLIST ( @originlist)

    my $line = shift ; 

    our %magnitude_info = () ;
    our $mb  = "" ;
    our $ms  = "" ;
    our $ml  = "" ;

    ($lat, $lon, $depth, $or_time, $mag, $magtype, $nph, $qual, $dtype) = split(",",$line) ;

    %magnitude_info = ( $magtype, $mag ) ;

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

# qual symbols:

    if ($qual eq 'A' ) {
       $algorithm = "best";
    } elsif ($qual eq 'B' ) {
       $algorithm = "-";
    } elsif ($qual eq 'C' ) {
       $algorithm = "-";
    } elsif ($qual eq 'D' ) {
       $algorithm = "poor";
    } else {
       $algorithm = "-";
    }

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $nph ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> $dtype   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> "-999.0" ,
                mbid		=> $mbid ,
                ml		=> "-999.0" , 
                mlid		=> $mlid ,
                ms		=> "-999.0" ,
                msid		=> $msid ,
                magtype		=> $magtype ,
                magnitude	=> $mag ,
		auth		=> $myauth , 
		algorithm	=> $algorithm  
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}


#
# individual bulletin parsers here
#

sub parse_NBEsearch {	# 

  my $line = shift ; 
  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;

  my ($lat, $lon, $depth, $or_time, $mag_type, $mag, $qual) = split(",",$line); 

  %magnitude_info = ( $mag_type,  $mag) ;

  ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

  %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> "0" ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> $ms, 
                msid		=> $msid ,
                magtype		=> $mag_type,
                magnitude	=> $mag,
		auth		=> $myauth ,  
		algorithm	=> "current-" . $qual  
  ) ;   

 	
  return (\%parsed_info, \%magnitude_info) ;
}

sub parse_HYPO2000 {	# BK and CI HYPO2000 versions mostly the same

  my $line = shift ; 
  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;

  ($lat, $lon, $depth, $or_time, $nsta, $mag_type, $mag, $extevid) = split(",",$line); 

  %magnitude_info = (
                $mag_type  	=> $mag
  ) ;

  ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

  %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $nsta,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml ,
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> $mag_type,
                magnitude	=> $mag,
		auth		=> $myauth . ":" . $extevid, 
		algorithm	=> "hypo2000" 
  ) ;   

 	
  return (\%parsed_info, \%magnitude_info) ;
}

sub parse_finger {	# 

  my $line = shift ; 

  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;

  $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "ml"  ;

  ($lat, $lon, $depth, $or_time, $mag, $qual) = split(",",$line); 

  if ($qual eq 'A' ) {
     $algorithm = "best";
  } elsif ($qual eq 'B' ) {
     $algorithm = "-";
  } elsif ($qual eq 'C' ) {
     $algorithm = "-";
  } elsif ($qual eq 'D' ) {
     $algorithm = "poor";
  } elsif ($qual eq '*' ) {	# implies NEIC supplied magnitude either mb, Ms, or MW
     $algorithm = "-";
     $magtype = 'M' ;	
  } else {
     $algorithm = "-";
  }

  %magnitude_info = ( $magtype,  $mag) ;

  ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

  %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> "0" ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml ,
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> "",
                magnitude	=> "-999.0",
		auth		=> $myauth, 
		algorithm	=> $algorithm 
  ) ;   

 	
  return (\%parsed_info, \%magnitude_info) ;
}

sub parse_NESN {	# 

  my $line = shift ; 

  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;

  ($lat, $lon, $depth, $or_time, $mn, $mc, $ml) = split(",",$line); 

  $magnitude_info{ml} = $ml if ($ml =~ /\d/ ) ;
  $magnitude_info{mc} = $mc if ($mc =~ /\d/ ) ;
  $magnitude_info{mn} = $mn if ($mn =~ /\d/ ) ;

  ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

  %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> "0" ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "-1" ,
                mag_uncert	=> "-1" ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml ,
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> "",
                magnitude	=> "-999.0",
		auth		=> $myauth, 
		algorithm	=> "search" 
  ) ;   

 	
  return (\%parsed_info, \%magnitude_info) ;
}

sub parse_ehdf {		# 

  my $line = shift ; 
  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;
  my @mt = (); 
  my @mv = (); 
  my @ma = (); 

  my $dtype = "f";
  my $etype = "-";

  my $year	= substr($line, 4, 4);
  my $month	= substr($line, 8, 2);
  my $day  	= substr($line,10, 2);
  my $hr   	= substr($line,12, 2);
  my $min  	= substr($line,14, 2);
  my $sec  	= substr($line,16, 4);
  $sec *= 0.01; 
  my $lat  	= substr($line,20, 5);
  $lat *= 0.001; 
  my $latNS  	= substr($line,25, 1);
  my $lon  	= substr($line,26, 6);
  $lon *= 0.001; 
  my $lonEW  	= substr($line,32, 1);
  my $depth  	= substr($line,33, 4);
  $depth *= 0.1; 
  my $depthinfo	= substr($line,37, 1);
  my $ndp   	= substr($line,38, 2);
  my $nph   	= substr($line,40, 3);
  my $qual	= substr($line,46, 1);
  my $mbgs	= substr($line,47, 2);
  my $nmbgs	= substr($line,49, 2);	# number of magnitude amps used
  my $msgs	= substr($line,51, 2);
  my $nmsgs	= substr($line,53, 2);	# number of magnitude amps used
  $mv[1]	= trim(substr($line,56, 3));
  $mt[1]	= substr($line,59, 2);
  $ma[1]	= trim(substr($line,61, 5));	
  $mv[2]	= trim(substr($line,66, 3)); 
  $mt[2]	= substr($line,69, 2);
  $ma[2]	= trim(substr($line,71, 5));	
  my $type   	= substr($line,89, 1);	 
  my $contribauth   	= substr($line,93, 5);	 

  $nph = ($nph =~ /\d/ ) ? $nph :  0 ;
  $ndp = ($ndp =~ /\d/ ) ? $ndp : -1 ;
  $nmbgs = ($nmbgs =~ /\d/ ) ? $nmbgs : -1 ;
  $nmsgs = ($nmsgs =~ /\d/ ) ? $nmsgs : -1 ;

  $mv[1] = 0.01 * $mv[1]  if ($mv[1] =~/\d/) ; 
  $mv[2] = 0.01 * $mv[2]  if ($mv[2] =~/\d/) ; 

  $mbgs = ($mbgs =~ /\d/ ) ? $mbgs *= 0.1 : -999.0 ;
  $msgs = ($msgs =~ /\d/ ) ? $msgs *= 0.1 : -999.0 ;


  ($lat, $lon) = fix_lat_lon($lat,"0",$latNS,$lon,"0",$lonEW);

# depthinfo symbols:
#
#N  Indicates depth was restrained at 33 km for earthquakes whose character on seismograms 
#   indicate a shallow focus but whose depth is not satisfactorily determined by the data.
#
#D  Indicates depth was restrained by the computer program based on 2 or more compatible pP 
#   phases and/or unidentified secondary arrivals used as pP.
#
#G  Indicates the depth was restrained by a geophysicist.
#
#*  Indicates a less well-constrained free depth.  The 90% marginal confidence interval on 
#   depth is greater than 8.5 km and less than or equal to 16.0 km.
#
#?  Indicates a poorly-constrained free depth.  The 90% marginal confidence interval on depth
#   is greater than 16.0 km.
#
#   The lack of any symbol indicates that the 90% marginal confidence interval on depth is 
#   less than or equal to 8.5 km, or that a contributed hypocenter was computed with a free
#   depth, regardless of the size of the confidence interval. 

  if ($depthinfo eq 'G' ) {
     $dtype = 'g';
  } elsif ($depthinfo eq 'D' ) {
     $dtype = 'd';
  } elsif ($depthinfo eq 'N' ) {
     $dtype = 'r';
  } elsif ($depthinfo eq '*' ) {
     $dtype = 'F';
  } elsif ($depthinfo eq '?' ) {
     $dtype = 'P';
  } else {
     $dtype = 'f';
  }

# qual symbols:

#Symbols Following Origin Time:
#
# &  Indicates that parameters of the hypocenter were supplied or determined by a computational 
#    procedure not normally used by NEIS.  The source or nature of the determination is indicated
#    by a 2 to 5 letter code enclosed by angle brackets and appearing in the first line of 
#    comments.  A "-P" appended to the code indicates that the computation is preliminary.  These
#    codes are included in the list of abbreviations below.
#
# %  Indicates a single network solution.  A non-furnished hypocenter has been computed using 
#    data reported by a single network of stations for which the date and/or origin time cannot 
#    be confirmed from seismograms available to a NEIS analyst.  The geometric mean of the 
#    semi-major and semi-minor axes of the horizontal 90% confidence ellipse is less than or 
#    equal to 16.0 km.
#
# *  Indicates a less reliable solution.  In general, the geometric mean of the semi-major and 
#    semi-minor axes of the horizontal 90% confidence ellipse is greater than 8.5 km and less 
#    than or equal to 16.0 km.
#
# ?  Indiates a poor solution, published for completeness of the catalogue.  In general, the 
#    geometric mean of the semi-major and semi-minor axes of the horizontal 90% confidence 
#    ellipse is greater than 16.0 km.  This includes a poor solution computed using data 
#    reported by a single network.
#
# Q  Indicates a preliminary solution obtained from the NEIC Earthquake Early Alerting Service 
#    program "Quick-quake." 
#
#    The lack of any symbol indicates that the geometric mean of the semi-major and semi-minor 
#    axes of the horizontal 90% confidence ellipse is less than or equal to 8.5 km.

  if ($qual eq '&' ) {
     $algorithm = "furnished";
  } elsif ($qual eq '%' ) {
     $algorithm = "single_network";
  } elsif ($qual eq '*' ) {
     $algorithm = "less_reliable";
  } elsif ($qual eq '?' ) {
     $algorithm = "poor_solution";
  } elsif ($qual eq 'Q' ) {
     $algorithm = "quick_quake";
  } else {
     $algorithm = "-";
  }

#etypes:
#
#non-tectonic source (E = explosion, I = collapse, 
#C = coalbump or rockburst in coal mine, R = rockburst,
#M = meteoritic source)
# (E => etype = 'ex',  M => etype = 'me')

  if ($type eq 'E' ) {
     $etype = "ex";
  } elsif ($type eq 'M' ) {
     $etype = "me";
  } elsif ($type eq ' ' ) {
     $etype = "-";
  } else {
     $etype = "-";
  } 
    
  if ( ($mbgs != 0 && $mbgs != -999.0 ) ) {
    $magnitude_info{'mb'} =  trim($mbgs).":NEIC:".trim($nmbgs)  ;
  }
  if ( ($msgs != 0 && $msgs != -999.0 ) ) {
    $magnitude_info{'ms'} =  trim($msgs).":NEIC:".trim($nmsgs)  ;
  }
  for (my $m = 1; $m < 3; $m++) {

     if ($mt[$m] eq "MW") {
	$magnitude_info{'mw'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "MS") {
	$magnitude_info{'ms'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "MB") {
	$magnitude_info{'mb'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "ML") {
	$magnitude_info{'ml'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "MD") {
	$magnitude_info{'md'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "ME") {
	$magnitude_info{'me'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "LG") {
	$magnitude_info{'mblg'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "RG") {
	$magnitude_info{'mbrg'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "CL") {
	$magnitude_info{'mc'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "NU") {
	$magnitude_info{'mn'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "M*") {
	$magnitude_info{'ml'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
     if ($mt[$m] eq "M ") {
	$magnitude_info{'mw'} =  $mv[$m].":".$ma[$m].":-1"  ;
     }
  }

  my $or_time = str2epoch ( sprintf ( "%s/%s/%s %s:%s:%s", $year, $month, $day, $hr, $min, $sec ) ) ;

  ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

#    while( my ($k, $v) = each %magnitude_info) {
#        print "Before adding to parsed_info ...  ";
#        print "key: $k, value: $v.\n";
#    }


  %parsed_info = (
		or_time		=> $or_time ,
		lat             => $lat ,
		lon             => $lon ,
		depth           => $depth ,
		ndef		=> $nph ,
		orid		=> "1" ,
		evid		=> "1" ,
		nass		=> "0"   ,
		ndp 		=> $ndp   ,
		etype		=> $etype   ,
		depdp		=> "-999.0"   ,
		review		=> "-"   ,
		dtype 		=> $dtype ,
		mag_nsta        => "-1" ,
		mag_uncert	=> "-1" ,
		magid		=> $magid ,
		mb		=> $mb ,
		mbid		=> $mbid ,
		ml		=> $ml , 
		mlid		=> $mlid ,
		ms		=> $ms , 
		msid		=> $msid ,
		magtype		=> "-" ,
		magnitude	=> "-999.0" ,
		auth		=> $myauth ,  
		algorithm	=> $algorithm  
  ) ;   

 	
  return (\%parsed_info, \%magnitude_info) ;
}

sub parse_mchdr  {	

    my $line = shift ; 

    our %magnitude_info = () ;
    our $mb  = "" ;
    our $ms  = "" ;
    our $ml  = "" ;
    my @mt = (); 
    my @mv = (); 
    my @ma = (); 
    my ($v, $a, $t) = "" ;

# ugly
    ($lat, $lon, $depth, $or_time, $dtype, $nsta, $mb, $mbsta, $ms, $mssta, $mv[1], $ma[1], $mt[1], $mv[2], $ma[2], $mt[2] ) = split(",", $line)  ;

      if ($mb && ($mb != 0) ) {
        $magnitude_info{'mb'} =  $mb.":NEIC:".$mbsta  ;
      }
      if ($ms && ($ms != 0) ) {
        $magnitude_info{'ms'} =  $ms.":NEIC:".$mssta  ;
      }

      for (my $m = 1; $m < 3; $m++) {

         if ($mt[$m] eq "MW") {
    	    $magnitude_info{'mw'} =  $mv[$m].":".$ma[$m].":-1"  ;
         }
         if ($mt[$m] eq "MS") {
            $magnitude_info{'ms'} =  $mv[$m].":".$ma[$m].":-1"  ;
         }
        if ($mt[$m] eq "MB") {
	   $magnitude_info{'mb'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "ML") {
	   $magnitude_info{'ml'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "MD") {
	   $magnitude_info{'md'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "ME") {
	   $magnitude_info{'me'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "LG") {
	   $magnitude_info{'mblg'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "RG") {
	   $magnitude_info{'mbrg'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "CL") {
	   $magnitude_info{'mc'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "NU") {
	   $magnitude_info{'mn'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "M*") {
	   $magnitude_info{'ml'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "M ") {
	   $magnitude_info{'mw'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
      }

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $nsta ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> $dtype   ,
                mag_nsta        => "0" ,
                mag_uncert	=> "0" ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> "-",
                magnitude	=> "-99.99" ,
		auth		=> $myauth , 
		algorithm	=> "-"  
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}

sub parse_mtech_hypo71  {	

    my $line = shift ; 

    our %magnitude_info = () ;
    our $mb  = "" ;
    our $ms  = "" ;
    our $ml  = "" ;
    my @mt = (); 
    my @mv = (); 
    my @ma = (); 
    my ($v, $a, $t) = "" ;

    ($lat, $lon, $depth, $or_time, $nsta, $mv[1], $ma[1], $mt[1], $mv[2], $ma[2], $mt[2], $mv[3], $ma[3], $mt[3], $Q ) = split (",", $line);

      for (my $m = 1; $m < 4; $m++) {

        if ($mt[$m] eq "ml") {
	   $magnitude_info{'ml'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "M") {
	   $magnitude_info{'M'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
        if ($mt[$m] eq "md") {
	   $magnitude_info{'md'} =  $mv[$m].":".$ma[$m].":-1"  ;
        }
      }

    ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

    %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $nsta ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => "0" ,
                mag_uncert	=> "0" ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml , 
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> "-",
                magnitude	=> "-99.99" ,
		auth		=> $myauth , 
		algorithm	=> "-"  
    ) ;   

  return (\%parsed_info, \%magnitude_info) ;

}


sub parse_uwcard {		# 

  my $line = shift ; 
  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;
  my @mt = (); 
  my @mv = (); 
  my @ma = (); 

  my $dtype = "-";
  my $etype = "-";
  my $skipit = 0 ;

  my $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "md"  ;

  my $type	= substr($line, 1, 1);
  my $year	= substr($line, 2, 4);
  my $month	= substr($line, 6, 2);
  my $day  	= substr($line, 8, 2);
  my $hr   	= substr($line,10, 2);
  my $min  	= substr($line,12, 2);
  my $sec  	= substr($line,15, 5);
  
  if ( $sec =~ /[a-z]/ || $sec eq "  " ) {
     $skipit = 1 ;
     return ;
  }

  my $lat  	= substr($line,21, 2);
  my $latNS  	= substr($line,23, 1);
  my $latm  	= substr($line,24, 4);
  $latm *= 0.01; 

  my $lon  	= substr($line,29, 3);
  my $lonEW  	= substr($line,32, 1);
  my $lonm  	= substr($line,33, 4);
  $lonm *= 0.01; 

  my $depth  	= substr($line,37, 6);
  my $depthinfo	= substr($line,43, 1);

  my $mag	= substr($line,44, 4);	# there may be a leading "-"
  my $nph 	= substr($line,52, 3) ;
  $nph =~ s/^0*//; 

  my $qual 	= substr($line,72, 1);	# second qual char deals with azim coverage, ignored

  $mag = "-99.99" if ($mag =~ /^\s{3}/) ; 	# netmag.magnitude default value

  $nph =  ($nph =~ /\d/ ) ? $nph : 0 ;

  if ($hr =~ "  ") { $hr = 0 ; }
  if ($min =~ "  ") { $min = 0 ; }
  if ($sec =~ "     ") { $sec = 0 ; }
  if ($depth =~ "      ") { $depth = 0 ; }


  $or_time = str2epoch( fix_time_values ($year,$month,$day,$hr,$min,$sec) ) ; 

  ($lat, $lon) = fix_lat_lon($lat,$latm,$latNS,$lon,$lonm,$lonEW);

  if ($depthinfo eq '*' ) {
     $dtype = 'g';
  } elsif ($depthinfo eq '$' ) {
     $dtype = 'r';
  } elsif ($depthinfo eq '#' ) {
     $dtype = 'r';
  } else {
     $dtype = '-';
  }

# qual symbols:

  if ($qual eq 'A' ) {
     $algorithm = "best";
  } elsif ($qual eq 'B' ) {
     $algorithm = "-";
  } elsif ($qual eq 'C' ) {
     $algorithm = "-";
  } elsif ($qual eq 'D' ) {
     $algorithm = "poor";
  } else {
     $algorithm = "-";
  }

#etypes:
#
#	F		- earthquakes reported to have been felt
#	P		- probable explosion
#	L		- low frequency earthquakes
#	H		- handpicked from helicorder records
#	X		- known explosion
#	R		- regional events
#	T		- teleseisms	
#	8		- Earthquake information from another source
#			  and event occurred between  1800  and  1899
#	9		- Earthquake information from another source
#			  and  event occurred between 1900 and 1999.
# (P => etype = 'ex',  X => etype = 'ex')

  if ($type eq 'P' ) {
     $etype = "ex";
  } elsif ($type eq 'X' ) {
     $etype = "ex";
  } elsif ($type eq 'F' ) {
     $etype = "f";
  } elsif ($type eq ' ' ) {
     $etype = "-";
  } else {
     $etype = "-";
  } 
    
  $magnitude_info{$magtype} =  $mag ;

  ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

#    while( my ($k, $v) = each %magnitude_info) {
#        print "Before adding to parsed_info ...  ";
#        print "key: $k, value: $v.\n";
#    }

  %parsed_info = (
		or_time		=> $or_time ,
		lat             => $lat ,
		lon             => $lon ,
		depth           => $depth ,
		ndef		=> $nph ,
		orid		=> "1" ,
		evid		=> "1" ,
		nass		=> "0"   ,
		ndp 		=> "-1"   ,
		etype		=> $etype   ,
		depdp		=> "-999.0"   ,
		review		=> "-"   ,
		dtype 		=> $dtype ,
		mag_nsta        => "-1" ,
		mag_uncert	=> "-1" ,
		magid		=> $magid ,
		mb		=> $mb ,
		mbid		=> $mbid ,
		ml		=> $ml , 
		mlid		=> $mlid ,
		ms		=> $ms , 
		msid		=> $msid ,
		magtype		=> $magtype ,
		magnitude	=> $mag ,
		auth		=> $myauth ,  
		algorithm	=> $algorithm  
  ) ;   

  return (\%parsed_info, \%magnitude_info) ;
}


sub parse_CUBE  {

# completely stolen from Danny's example USGScube2orb

  our %magnitude_info = () ;
  our $mb  = "" ;
  our $ms  = "" ;
  our $ml  = "" ;

  (@lines, %pfinfo) = @_ ;
#  $myauth	= $pfinfo{auth};

  my $line = shift (@lines) ;

  if (substr ($line, 0, 1) ne "E") {return ;}

  my $net = substr ($line, 10, 2) ;
  my $year = substr($line, 13, 4);
  my $month = substr ($line, 17, 2);
  my $day = substr ($line, 19, 2);
  my $hour = substr ($line, 21, 2);
  my $minute = substr ($line, 23, 2);
  my $second = substr ($line, 25, 3);
  $second *= 0.1 ;
  my $lat = substr ($line, 28, 7);
  $lat *= 0.0001 ;
  my $lon = substr ($line, 35, 8);
  $lon *= 0.0001 ;
  my $depth = substr ($line, 43, 4);
  $depth *= 0.1 ;
  our $magnitude = substr ($line, 47, 2);
  if ($magnitude eq "  ") {
      $magnitude = -999.0 ;
  } else {
      $magnitude *= 0.1 ;
  }
  my $ndef = substr ($line, 52, 3);
  my $mag_type = substr ($line, 73, 1);
  my $mag_nstations = substr ($line, 74, 2);

  $ndef = ($ndef =~ /\d/ ) ? $ndef :  0 ;

  my $loc_method = substr ($line, 78, 1);		# loc_method is ill defined in documentation

  #"Location Method" field: varies by source (in parentheses):
  #     Upper-case indicates an unconfirmed event,
  #     Lower-case indicates event is confirmed by human review

  #     A = Binder (AK)
  #     D = Antelope (NN)
  #     F = nonNEIC-furnished (US)
  #     H = Hypoinverse (CI,UU,UW)
  #     L = Earthworm "local" event (NC)
  #     M = macroseismic or "felt" (US)
  #     R = NEIC-furnished (US)

  if ($mag_nstations eq "  " || $mag_nstations eq " 0" || $mag_nstations eq "00" ) {
      $mag_nstations = -1 ;
  }

  my $mag_uncertainty = substr ($line, 76, 2);
  if ($mag_uncertainty eq "  ") {
      $mag_uncertainty = -1.0 ;
  } else {
      $mag_uncertainty *= 0.1 ;
  }

  if ($loc_method ne " ") {
      $algorithm	= "qedCUBE-" . $loc_method ;
  } else {
      $algorithm	= "qedCUBE"  ;
  }

  if ($mag_type eq "B") {
      $mag_type = "mb" ;
      $mb = $magnitude; 
  }
  if ($mag_type eq "C") {
      $mag_type = "md" ;
  }
  if ($mag_type eq "D") {
      $mag_type = "md" ;
  }
  if ($mag_type eq "E") {
      $mag_type = "me" ;
  }
  if ($mag_type eq "G") {
      $mag_type = "ml" ;
      $ml = $magnitude; 
  }
  if ($mag_type eq "I") {
      $mag_type = "mi" ;
  }
  if ($mag_type eq "L") {
      $mag_type = "ml" ;
      $ml = $magnitude; 
  }
  if ($mag_type eq "N") {
      $mag_type = "mblg" ;
  }
  if ($mag_type eq "O") {
      $mag_type = "mw" ;
  }
  if ($mag_type eq "P") {
      $mag_type = "mb" ;
      $mb = $magnitude; 
  }
  if ($mag_type eq "S") {
      $mag_type = "ms" ;
      $ms = $magnitude; 
  }
  if ($mag_type eq "T") {
      $mag_type = "mw" ;
  }
  if ($mag_type eq "W") {
      $mag_type = "mw" ;
  }

  if ($mag_type eq "*") {		# deal with non-documented mag_type reported by CI
      $mag_type = "ml" ;
      $ml = $magnitude; 
  }

  my $or_time = str2epoch ( sprintf ( "%s/%s/%s %s:%s:%.1f", $year, $month, $day, $hour, $minute, $second ) ) ;

  %magnitude_info = (
                $mag_type	=> $magnitude
  ) ;

  ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) = &default_mags(%magnitude_info) ;

        %parsed_info = (
                or_time		=> $or_time ,
                lat             => $lat ,
                lon             => $lon ,
                depth           => $depth ,
                ndef		=> $ndef ,
		orid		=> "1" ,
        	evid		=> "1" ,
                nass		=> "0"   ,
                ndp 		=> "-1"   ,
                etype		=> "-"   ,
                depdp		=> "-999.0"   ,
                review		=> "-"   ,
                dtype 		=> "-"   ,
                mag_nsta        => $mag_nstations ,
                mag_uncert	=> $mag_uncertainty ,
                magid		=> $magid ,
                mb		=> $mb ,
                mbid		=> $mbid ,
                ml		=> $ml ,
                mlid		=> $mlid ,
                ms		=> $ms ,
                msid		=> $msid ,
                magtype		=> $magtype,
                magnitude	=> $magnitude,
		auth		=> $myauth . ":" . $net, 
		algorithm	=> $algorithm  
        ) ;   

 	
  return (\%parsed_info, \%magnitude_info) ;

}

#
# origin info extracts below here
#

sub extract_mchdr   	{	# card format with multi-line information 

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";

  while (<PARSE>) {

    my $line = $_ ;

    if ($line =~ /^\n|^<|#/) {
      next;
    } elsif ($line =~ /^(HY)/) {
      $year       = substr($line,2,4);
      $month      = substr($line,6,2);
      $day        = substr($line,8,2);
      $hr         = substr($line,11,2);
      $min        = substr($line,13,2);
      $sec        = substr($line,15,5);
      $lat        = substr($line, 21,6);
      $latNS      = substr($line, 27,1);
      $lon        = substr($line, 29,7);
      $lonEW      = substr($line, 36,1);
      $depth      = substr($line, 38,5);
      $depthinfo  = substr($line, 43,1); # depth fixed?  N, G, D, * or ? are valid
      $nsta       = substr($line, 48,3);

      $or_time	= str2epoch( fix_time_values ($year,$month,$day,$hr,$min,$sec) ) ; 
      ($lat,$lon) = fix_lat_lon ($lat,"0" ,$latNS,$lon,"0",$lonEW) ; 

      if ($depthinfo eq 'G' ) {
        $dtype = 'g';
      } elsif ($depthinfo eq 'D' ) {
        $dtype = 'd';
      } elsif ($depthinfo eq 'N' ) {
        $dtype = 'r';
      } elsif ($depthinfo eq '*' ) {
        $dtype = 'F';
      } elsif ($depthinfo eq '?' ) {
        $dtype = 'P';
      } else {
        $dtype = 'f';
      }

    } elsif ($line =~ /^E/) {
      my $mbgs	= trim(substr($line, 28,3));
      my $nmbgs	= trim(substr($line, 32,3));
      my $msgs	= trim(substr($line, 36,3));
      my $nmsgs	= trim(substr($line, 40,2));
      $mv[1]	= trim(substr($line, 42,3));
      $mt[1]	= trim(substr($line, 45,2));
      $ma[1]	= trim(substr($line, 47,4));
      $mv[2]	= trim(substr($line, 51,3));
      $mt[2]	= trim(substr($line, 54,2));
      $ma[2]	= trim(substr($line, 56,4));

     push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $dtype, $nsta, $mbgs, $nmbgs, $msgs, $nmsgs, $mv[1], $ma[1], $mt[1], $mv[2], $ma[2], $mt[2] ))) ;

      next;

    } elsif ($line =~ /^L/) {
      next;
    } elsif ($line =~ /^A/) {
      next;
    } elsif ($line =~ /^P/) {
      next;
    } elsif ($line =~ /^S/) {
      next; 
    } elsif ($line =~ /^M/) {
      next; 
    } elsif ($line =~ /^C/) {	# all origin info obtained
      $comment    = substr($line, 2,58);
      next;
    }

  }
  
  close PARSE ;
  return @saved; 
}

sub extract_mtech_hypo71   	{	# MTECHs modified hypo71 format 

  my (@info) = @_ ;
  my @saved = ();
  
  for (@info) {		# another hackery follows...
    my $line = $_ ;
    elog_notify("Line is: $line\n");
    $year       = substr($line,0,2);	# non-Y2K compliant.  Assume year = 2000 + $year
    elog_notify("Year is: $year\n");
    $year 	= 2000 + $year ;
    $month      = substr($line,3,2);
    $day        = substr($line,6,2);
    $hr         = substr($line,9,2);
    $min        = substr($line,11,2);
    $sec        = substr($line,14,4);
    $lat        = substr($line, 19,6);
    $latNS      = "N"; 
    $lon        = substr($line, 26,7);
    $lonEW      = "W"; 
    $depth      = substr($line, 34,4);

    $mv[1]	= trim(substr($line, 39,3));
    $mt[1]	= ($mv[1]) ? "md" : "-" ; 
    $ma[1]	=  "ESO" ;
    $mv[2]	= trim(substr($line, 43,3));
    $mt[2]	= ($mv[2]) ? "M" : "-" ; 
    $ma[2]	=  "BUT"; 
    $mv[3]	= trim(substr($line, 47,3));
    $mt[3]	= ($mv[3]) ? "ml" : "-" ; 
    $ma[3]	=  "BB"; 

    $nsta       = substr($line, 51,2);
    $Q          = substr($line, 79,1);

    $or_time	= str2epoch( fix_time_values ($year,$month,$day,$hr,$min,$sec) ) ; 
    ($lat,$lon) = fix_lat_lon ($lat,"0" ,$latNS,$lon,"0",$lonEW) ; 

    push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $nsta, $mv[1], $ma[1], $mt[1], $mv[2], $ma[2], $mt[2], $mv[3], $ma[3], $mt[3], $Q ))) ;

      next;

  }
  
  close PARSE ;
  return @saved; 
}

sub extract_EMSC {	

  my (@info) = @_ ;
  my @saved =  () ;

  my ($lat,$lon,$depth,$mag) ;
  my ($partcnt,$magval,$date,$time, $dt) ; 

  my $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "M"  ;

  my @evinfo = () ;

  for (@info) {		# another hackery follows...
    my $line = $_ ;

    if ($line =~ /^\n|^</ ) {
      next;

    } elsif  ($line =~ /.*Back.*/)  {	# Start of listing 
      if ($ok == 0) {
	$ok++ ;
      } else {
	$ok = 0 ;
      }

    } elsif    ($ok && ($line =~ /^\d{4}\-\d{2}\-\d{2}/) ) {	# match 2009-09-08
      if (length($line) > 20 ) { 
        $date = substr($line, 0, 10);
        $time = substr($line, 13, 10);
	push ( @evinfo, "$date", "$time" ) ; 
      } else {	# this is a lddate, add info for event
	$date = $evinfo[0];
	$time = $evinfo[1];
	$latd  = $evinfo[2];
	$latNS  = $evinfo[3];
	$lond  = $evinfo[4];
	$lonEW  = $evinfo[5];

	if ($#evinfo == 8 ) 	{ 	# event listing + dtype
	   $depth   = $evinfo[6];
	   $magtype = $evinfo[7];
	   $mag	    = $evinfo[8] ;	# don't need comment line
	} elsif ($#evinfo == 7)  {	# event listing w/ no depth
	   $depth   = -999.0 ;		
	   $magtype = $evinfo[6];
	   $mag	    = $evinfo[7] ;	
	} elsif ($#evinfo == 6)  {	# event listing w/ no maginfo
	   $depth   = $evinfo[6];
	   $magtype = "-" ;	
	   $mag     = "-99.99";
	}

	if ($magtype=~/ML|MD/) {
	   $magtype = lc($magtype) ;
	}

	$or_time = str2epoch( "$date $time" ) ; 
	($lat,$lon) = fix_lat_lon ($latd,"0",$latNS,$lond,"0",$lonEW) ; 

	push (@saved, join(',', ($lat, $lon, $depth, $or_time, $mag, $magtype) ) ) ; 
	@evinfo = ();
	next; 
	
      }
    } elsif ( $ok && $line =~ /^\d|^[A-Z,a-z]/ && $line !~ /.*ago.*|F/ )  {
        $line =~ s/|//g ;
        $line = encode_entities($line) ; 
        $line =~ s/&nbsp;//g;
	push (@evinfo, $line) if ($#evinfo >=1 )  ;
    } elsif ( $ok && $line =~ /.*recent.*/ )  {
	last; 
    } else {
	next;
    }

  }
  
  close PARSE ;
  return @saved; 
}

sub extract_neicWWW {	# differs from many extract subs because input is @array, not $filename

my (@info) = @_ ;
my @saved =  () ;

my ($lat,$lon,$depth,$mag) ;

# Deal with non-publishing of magnitude types
  my $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "M"  ;


  for (@info) {
    my $line = $_ ;
    if ($line =~ /^\n|^</) {
      next;
    } elsif ( $line =~ /MAP/ ) {	
      $ok++ ;
      $partcnt = 0;
      next;
    } elsif ($ok && ($line !~ /km/) )  {

      if ( ($line =~ /\d\.\d/) && ($partcnt == 0) ) {
        $mag = $line;
        $mag =~ s/^[0-9]|^\.//g ;
        $mag = encode_entities($mag) ;
        $mag =~ s/&nbsp;//g;
        $partcnt++;
        next;
      }  elsif    ( ($line=~ /^\d{4}/) && ($partcnt == 1) ) {
        ($ymd, $hms) = split(/\s+/,$line) ;
        $partcnt++;
        next;
      } elsif ( ($line =~ /.\d{3}/) && ($partcnt == 2) ) {         # THIS IS A TOTALLY UGLY HACK!
        $lat = $line;
        $lat =~ s/|//g ;
        $lat = encode_entities($lat) ;
        $lat =~ s/&nbsp;//g;
        if ( $lat =~ /N$/ ) {
          $lat =~ s/N$// ;
        } elsif ( $lat =~ /S$/ ) {
          $lat =~ s/S$// ;
          $lat = -$lat ;
        }

        $partcnt++;
        next;

      } elsif ( ($line =~ /.\d{3}/) && ($partcnt == 3) ) {         # THIS IS A TOTALLY UGLY HACK!
        $lon = $_;
        $lon =~ s/|//g ;
        $lon = encode_entities($lon) ;
        $lon =~ s/&nbsp;//g;

        if ( $lon =~ /E$/ ) {
          $lon =~ s/E$// ;
        } elsif ( $lon =~ /W$/ ) {
          $lon =~ s/W$// ;
          $lon = -$lon ;
        }

        $partcnt++;
        next;

      }  elsif ( ($line =~ /\.\d/ && $line !~ /[A-Z][a-z]/) && ($partcnt == 4) ) {          # THIS IS A TOTALLY UGLY HACK!
        $depth = $line;
        $depth =~ s/\|//g ;
        $depth = encode_entities($depth) ;
        $depth =~ s/&nbsp;//g;
        $ok--;

      } else {
        next ;
      }

      $mag = "-999.0" if $mag eq "" ; 

      $or_time = str2epoch ( "$ymd $hms" ) ; 
      $etype = "-" ;

      push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $mag, $magtype, $etype ))) ;
      next;


    } elsif ( (/\w+/) && !/^map|^MAP/) {    # this should match the region and thus be the end of our grab
      $ok--;
      $partcnt = 0;
      next;
    
    } else {
      next;
    }

  } 

  return @saved;

} 

sub extract_qedWWW {		

  my (@info) = @_ ;
  my @saved =  () ;

# Deal with the moronic practice of non-UTC timezones in published bulletins 
  my $TZ = ( defined $pfinfo{TZ} ) ? $pfinfo{TZ} : "UTC"  ;

  my ($lat,$lon,$depth,$mag,$magtype) ;

# Deal with non-publishing of magnitude types
# for these "recenteqs" listings, most seem to use "ml" for events with mag >= 1.9
#  and md for mag < 1.9

  foreach my $line (@info) {

    if ($line =~ /^\n|^</) {
      next;
    } elsif ( $line =~ /MAP|map/ ) {	
      $ok++ ;
      $partcnt = 0;
      next;
    } elsif ($ok && ($line !~ /km/) )  {

      if ( ($line =~ /\d\.\d/) && ($partcnt == 0) ) {
         $mag = $line;
         $mag =~ s/^[0-9]|^\.//g ;
         $mag = encode_entities($mag) ;
         $mag =~ s/&nbsp;//g;
         $partcnt++;
         next;
      }  elsif    ( ($line=~ /^\d{4}/) && ($partcnt == 1) ) {
         ($ymd,$hour,$lat,$lon,$depth) = split(/\s+/,$line) ;
         $lat =~ s/|//g ;
         $lat = encode_entities($lat) ;
         $lat =~ s/&nbsp;//g;
         $lat = trim($lat);
         if ( $lat =~ /N$/ ) {
           $lat =~ s/N$// ;
         } elsif ( $lat =~ /S$/ ) {
           $lat =~ s/S$// ;
           $lat = -$lat ;
         } else {
           elog_complain( "dropping $line because of parsing error with lat = '$lat'\n" ) ;
           next ;
         }

         $lon =~ s/|//g ;
         $lon = encode_entities($lon) ;
         $lon =~ s/&nbsp;//g;

         if ( $lon =~ /E$/ ) {
           $lon =~ s/E$// ;
         } elsif ( $lon =~ /W$/ ) {
           $lon =~ s/W$// ;
           $lon = -$lon ;
         } else {
           elog_complain ( "dropping $line because of parsing error with lon = '$lon'\n" ) ;
           next ;
         }

         $depth =~ s/\|//g ;
         $depth = encode_entities($depth) ;
         $depth =~ s/&nbsp;//g;
         $ok--;
         $partcnt++;

         $mag = "-999.0" if $mag eq "" ; 

         $or_time = str2epoch ( "$ymd $hour $TZ" ) ; 

	 $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "M"  ;
	 $magtype = ( $mag >= 1.9 ) ? "ml" : "md"  ;

         $etype = "-" ;

         push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $mag, $magtype, $etype ))) ;
         next;

       }


      } else {
        next ;
      }

  } 

  return @saved;

} 

sub extract_finger {	# (insert giggle here)  - just following naming conventions!

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";
  while (<PARSE>) {

    my $line = $_ ;

    if ($line !~ /^\d{4}\/\d{2}\/\d{2}/)  {	# match XXXX/XX/XX
      next;
    } else {

      my $ymd	= substr($line, 0, 10);
      my $hms	= substr ($line, 11, 8);

      my $latd	= substr ($line, 20, 5);
      my $latNS	= substr ($line, 26, 1);

      my $lond	= substr ($line, 28, 6);
      my $lonEW	= substr ($line, 34, 1);

      my $depth	= substr ($line, 36, 5);

      my $mag	= (substr ($line,42, 3));
      my $qual	= (substr ($line,46, 3));

      $or_time = str2epoch( "$ymd $hms" ) ; 

      ($lat,$lon) = fix_lat_lon($latd,"0",$latNS,$lond,"0",$lonEW) ;
      push (@saved, join(',', ($lat, $lon, $depth, $or_time, $mag, $qual) ) ) ; 
   }

  }
  
  close PARSE ;
  return @saved; 
}


sub extract_NESN {	# yet another format for screen scrape 

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";
  while (<PARSE>) {

    my $line = $_ ;

    if ($line =~ /^\n|^<|#/) {
      next;
    }  elsif    ($line =~ /\d{4}\/\d{2}\/\d{2}/)  {	# match XXXX/XX/XX

      ($ymd,$hms,$lat,$lon,$depth,$mn,$mc,$ml) = split(/\t/,$line) ;

      $lon = -$lon ; 	# NESN assumes West longitude with no indicator

      $or_time = str2epoch( "$ymd $hms" ) ; 

      push (@saved, join(',', ($lat, $lon, $depth, $or_time, $mn, $mc, $ml) ) ) ; 
    } else {
      next; 
    }

  }
  
  close PARSE ;
  return @saved; 
}

sub extract_HYPO2000	{	# BK and CI HYPO2000 versions mostly the same

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";
  while (<PARSE>) {

    if (/^\n|^<|#/) {
      next;
    } else {
      my $line = $_ ;
      my $year	= substr($line, 0, 4);
      my $month	= substr ($line, 4, 2);
      my $day	= substr ($line, 6, 2);
      my $hr	= substr ($line, 8, 2);
      my $min	= substr ($line, 10, 2);
      my $sec	= substr ($line, 12, 4);

      my $latd	= substr ($line, 16, 2);
      my $latNS	= substr ($line, 18, 1);
      my $latm	= substr ($line, 19, 4);

      my $lond	= substr ($line, 23, 3);
      my $lonEW	= substr ($line, 26, 1);
      my $lonm	= substr ($line, 27, 4);

      my $depth	= substr ($line, 31, 5);

      my $nsta	= (substr ($line,118, 3));

      my $extevid	= (substr ($line,136,10)); 
      my $mag_type	= "m" . lc(substr ($line,146, 1)); 
      my $mag	= substr ($line,147, 3); 

# NC uses version number:
# Version number of information: 0=25 pick; 1=Final EW with MD;
#                   2=ML added, etc. 0-9, then A-Z. Hypoinv. passes this through.
# seems to be 0 -> 5?

      my $revlevel	= substr ($line,162, 1); 

# Version # of last human review. blank=unreviewed, 1-9, A-Z.
# seems to be either "F" or "A"?

      my $humanrevlevel	= substr ($line,163, 1); 

      $sec	= $sec/100 ;
      $latm = $latm/100;
      $lonm = $lonm/100; 
      $depth = trim($depth)/100; 
      $nsta = trim($nsta);
      $mag = trim($mag)/100;
      $extevid = trim($extevid) . $revlevel . $humanrevlevel ;

      if (!$nsta) {
        $nsta= "0"; 
      }
      if ($mag <= 0) {
        $mag = "-999.00"; 
      }
  
      ($lat,$lon) = fix_lat_lon ($latd,$latm,$latNS,$lond,$lonm,$lonEW) ; 

      my $value =  fix_time_values ($year,$month,$day,$hr,$min,$sec)  ; 

      $or_time = str2epoch( fix_time_values ($year,$month,$day,$hr,$min,$sec) ) ; 

      push (@saved, join(',', ($lat, $lon, $depth, $or_time, $nsta, $mag_type, $mag, $extevid) ) ) ; 
    }

  }
  
  close PARSE ;
  return @saved; 
}

sub extract_AEIC {	

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";
  while (<PARSE>) {

    if (/^\s{1}\d{2}\/\d{2}\/\d{4}/) {
      my $line = $_ ;
      my $date	= substr ($line, 1, 10);
      my $time	= substr ($line, 13, 10);

      my $lat	= substr ($line, 26, 7);
      my $latNS	= substr ($line, 34, 1);

      my $lon	= substr ($line, 37, 8);
      my $lonEW	= substr ($line, 46, 1);

      my $depth	= substr ($line, 50, 5);
      my $mb 	= trim(substr ($line, 58, 4)); 
      my $ml 	= trim(substr ($line, 65, 4)); 
      my $ms 	= trim(substr ($line, 72, 4)); 

      ($lat,$lon) = fix_lat_lon ($lat,"0",$latNS,$lon,"0",$lonEW) ; 

      $or_time = str2epoch( "$date $time" )  ; 

      push (@saved, join(',', ($lat, $lon, $depth, $or_time, $mb, $ml, $ms ) ) ) ; 

    } else {
      next;
    }

  }
  
  close PARSE ;
  return @saved; 
}

sub extract_newNBE {	

  my (@info) = @_ ;
  my @saved =  () ;

  my ($lat,$lon,$depth,$mag,$nph,$magval) ;
  my ($text, $tz) ;
  my $partcnt = 0 ;
  my ($date,$time,$or_time) ; 

  my $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "ml"  ;

  my @evinfo = () ;

  for (@info) {		# another hackery follows...
    my $line = $_ ;

    if ($line =~ /^\n|^</ ) {
      next;

    } elsif  ($line =~ /The Nevada.*/)  {	# Start of listing, maybe 
      if ($ok == 0) {
	$ok++ ;
      } else {
	$ok = 0 ;
      }

    } elsif    ($ok && ($line =~ /^\d{2}\-\d{2}\-\d{4}/) ) {	# match 07-06-2010
	$date   = trim($line);
	$date	=~  s/\-/\//g ;
	$partcnt = 1 ;
    } elsif ( ($line =~ /^\d{2}\:/) && ($partcnt == 1) ) { 
	($time,$tz) = split(/\s+/,$line) ;  # morons use PDT (or PST)
        $or_time = fix_or_time ($date, $time, $tz) ;
	$partcnt++;
    } elsif ( $partcnt == 2) {
	$text   = trim($line);
	$partcnt++;
    } elsif ( $partcnt == 3) {
	$lat    = trim($line);
	$partcnt++;
    } elsif ( $partcnt == 4) {
	$lon    = trim($line);
	$partcnt++;
    } elsif ( $partcnt == 5) {
	$depth  = trim($line);
	$partcnt++;
    } elsif ( $partcnt == 6) {
	$mag	= trim($line);	
	$partcnt++;
    } elsif ( $partcnt == 7) {
	$nph	= trim($line);	
	$partcnt++;
    } elsif ( $partcnt == 8) {
	$comment	= trim($line);	
	$partcnt++;
    } elsif ( $partcnt == 9 && $line =~ /[0-9]*/ ) {
	$evid	= trim($line); 
	push (@saved, join(',', ($lat, $lon, $depth, $or_time, $magtype, $mag, $nph, $comment, $evid) ) ) ; 
	$partcnt = 0 ;
    } else {
	next;
    }
	     
  }
  
  close PARSE ;
  return @saved; 
}


sub extract_NBEwww {	# &extract_NBEwww($file) 

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";

  while (<PARSE>) {

    if (/^\n|^<|#/) {
      next;
    } elsif ( /^[A-Z][a-z]/ ) {
      next;

    } elsif ( /^\d{4}/  )  {
      my ($ymd, $hms, $TZ, $lat, $latNS, $lon, $lonEW, $depth, $mag, $nph, $comment, $evid ) = split(/\s+/,$_) ;

      ($lat,$lon) = fix_lat_lon ($lat,0,$latNS,$lon,0,$lonEW) ; 

      # Deal with non-publishing of magnitude types
      $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "ml"  ;

      # ARGH.  Who in their right mind would use anything other than UTC?!?!?!
      my $or_time = fix_or_time ($ymd, $hms, $TZ) ;

      push (@saved, join(',', ($lat, $lon, $depth, $or_time, $magtype, $mag, $nph, $comment, $evid) ) ) ; 
      next;
    } else {
      next;
    }

  }

  return @saved; 

} 


sub extract_simsum {	# extract_simsum ($file) 

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";

  while (<PARSE>) {

    my $line = $_ ;

    if ($line =~ /^\n|^</) {
      next;
    }  elsif    ($line=~ /^\d{2}\/\d{2}\/\d{2}|^\d{4}\/\d{2}\/\d{2}/)  {	# match XX/XX/XX or XXXX/XX/XX
      ($ymd,$hms,$lat,$lon,$depth,$mag,$qual,$comment) = split(/\s+/,$line) ;

      $lat = trim($lat);
      if ( $lat =~ /N$/ ) {
         $lat =~ s/N$// ;
      } elsif ( $lat =~ /S$/ ) {
         $lat =~ s/S$// ;
         $lat = -$lat ;
      } 

      if ( $lon =~ /E$/ ) {
         $lon =~ s/E$// ;
      } elsif ( $lon =~ /W$/ ) {
         $lon =~ s/W$// ;
         $lon = -$lon ;
      } 

      $depth = trim($depth) ;

      if ($depth =~ /\*/) {	# get rid of "*" used in nrcan
	$depth = substr($depth,0,-1);
      }

      if ($mag =~ /.*M.*|.*m.*/) {
	if ($mag =~ /.*([A-Z][a-z])/) { 
	   $magtype = $1 ; 
           $mag =~ s/[A-Z][a-z]// ;
	} elsif ($mag =~ /.*([A-Z]{2})/) {
	   $magtype = lc $1 ; 
           $mag =~ s/[A-Z]{2}// ;
	}
      } else {
# Deal with non-publishing of magnitude types
	$magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "ml"  ;
      } 

      if ($qual =~ /F|f/) {
	$etype = 'f' ;
      } else { 
	$etype = "-" ;
      }

      $mag = "-999.0" if $mag eq "" ; 

      ($year,$month,$day) = split('/',$ymd);
      ($hr,$min,$sec) = split(':',$hms) ;

      $or_time = str2epoch( fix_time_values ($year,$month,$day,$hr,$min,$sec) ) ; 

#      elog_notify sprintf "      %s %7.3f %8.3f %5.0f   %3.1f\n", strydtime($or_time), $lat, $lon, $depth, $mag, $magtype ; 
      push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $mag, $magtype, $etype ))) ;
      next;

    } else {
      next ;
    }

  } 

  return @saved; 
}


sub extract_NBEsearch	{	# 

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";
  while (<PARSE>) {

    if (/^[0-9]{4}/ && !/.*event.*/) {
      my $line = $_ ;
      my $year	= substr($line, 0, 4);
      my $month	= substr ($line, 5, 2);
      my $day	= substr ($line, 8, 2);
      my $hr	= substr ($line, 11, 2);
      my $min	= substr ($line, 13, 2);
      my $sec	= substr ($line, 16, 7);

      my $lat	= substr ($line, 25, 8);
      my $lon	= substr ($line, 34, 9);
      my $depth	= substr ($line, 45, 8);

      my $mag	= substr ($line, 55, 5); 
      my $qual	= substr ($line, 63, 2); 


# may need to use  ml for larger mag events and md for smaller mag events...
# See: 
#     http://www.seismo.unr.edu/Catalog/catalog-search-help.html#output_format 
# arbitrarily choose 3.0 as change between md and ml

# Deal with non-publishing of magnitude types
	$magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "M"  ;

      if ($mag >= 2.9) {
      	$mag_type	= "ml";
      } elsif ($mag < 2.9) {
      	$mag_type	= "md";
      }

      if ($mag <= 0) {
        $mag = "-999.00"; 
        $mag = "-999.00"; 
      }

      $or_time = str2epoch( fix_time_values ($year,$month,$day,$hr,$min,$sec) ) ; 

      push (@saved, join(',', ($lat, $lon, $depth, $or_time, $mag_type, $mag, $qual) ) ) ; 
    } else {
      next;
    }

  }
  
  close PARSE ;

  return @saved; 

}

sub extract_mtechAUTO {	# extract_mtechAUTO ($file) 

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";

  while (<PARSE>) {

    my $line = $_ ;

    if ($line =~ /^\n|^</) {
      next;
    }  elsif    ($line=~ /^\d{4}/)  {
      ($ymd,$hm,$sec,$lat,$latm,$lon,$lonm,$depth,$E,$mag,$nph,$gap,$dmin,$rms,$erh,$erz,$qual,$evid) = split(/\s+/,$line) ;
      $lat = trim($lat);
      if ( $lat =~ /N$/ ) {
         $lat =~ s/N$// ;
      } elsif ( $lat =~ /S$/ ) {
         $lat =~ s/S$// ;
         $lat = -$lat ;
      } 

      if ( $lon =~ /E$/ ) {
         $lon =~ s/E$// ;
      } elsif ( $lon =~ /W$/ ) {
         $lon =~ s/W$// ;
         $lon = -$lon ;
      } 

      $mag = "-999.0" if $mag eq "" ; 

      $year = substr($ymd,0,4) ;
      $month = substr($ymd,4,2) ;
      $day = substr($ymd,6,2) ;

      $hr = substr($hm,0,2) ;
      $min = substr($hm,2,2) ;

      $or_time = str2epoch ( "$month\/$day\/$year $hr:$min:$sec " ) ; 

      ($lat,$lon) = fix_lat_lon ($lat,$latm,"",$lon,$lonm,"") ; 
 
# Deal with non-publishing of magnitude types
      my $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "md"  ;

#      elog_notify sprintf "      %s %7.3f %8.3f %5.0f   %3.1f\n", strydtime($or_time), $lat, $lon, $depth, $mag ; 
      push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $mag, $magtype, $nph, $evid, $qual ))) ;
      next;

    } else {
      next ;
    }

  } 

  return @saved; 
}

sub extract_uussLIST {	# extract_uuSSLIST ($file) 

  my ($file)  = @_; 
  my @saved = ();
  
  elog_notify("File name is: $file\n");

  open(PARSE, "$file") || die "Can't open $file\n";

  while (<PARSE>) {

    my $line = $_ ;

    if ($line =~ /^\n|^</) {
      next;
    }  elsif    ($line=~ /^\d{6}/)  {
      ($ymd,$hm,$sec,$lat,$latm,$lon,$lonm,$depth,$mag,$nph,$gap,$dmin,$rms,$erh,$erz,$qual,$evid) = split(/\s+/,$line) ;
      my $year	= substr($line, 0, 2);
      my $month	= substr($line, 2, 2);
      my $day  	= substr($line, 4, 2);
      my $hr   	= substr($line, 7, 2);
      my $min  	= substr($line, 9, 2);
      my $sec  	= substr($line,12, 5);
      my $lat  	= substr($line,18, 2);
      my $latm  = substr($line,21, 5);
      my $lon  	= substr($line,27, 3);
      my $lonm  = substr($line,31, 5);
      my $depth = substr($line,38, 5);
      my $dtype	= substr($line,43, 1);
      my $magtype	= substr($line,44, 1);
      my $mag	= substr($line,45, 5);
      my $nph	= substr($line,51, 2);
      my $qual	= substr($line,78, 1);

      $lat = trim($lat);
      $latNS = "N" ;
      $lon = trim($lon);
      $lonEW = "W" ;

      if ($mag == -9.99 || $mag eq "" ) {
	$mag = "-999.0" ;  
      }

      if ($ymd =~ /^[0-7]/) {	# dear god, are there really people who don't have a Y2K fix in place?!?!?!
	$year = 2000 + $year ; 
      } else {
	$year = 1900 + $year ;
      }

      $or_time = str2epoch ( "$month\/$day\/$year $hr:$min:$sec " ) ; 

      ($lat,$lon) = fix_lat_lon ($lat,$latm,$latNS,$lon,$lonm,$lonEW) ; 

      if ($dtype =~ /\*/) {
	$dtype = 'g' ;
      } else {
	$dtype = '-' ;
      }

      if ($magtype eq "W") {
	  $magtype = "ml" ;
      } else {
          $magtype = ( defined $pfinfo{'defaultmt'} ) ? $pfinfo{defaultmt} : "mc"  ;
      }

#      elog_notify sprintf "      %s %7.3f %8.3f %5.0f   %3.1f\n", strydtime($or_time), $lat, $lon, $depth, $mag ; 
      push ( @saved, join(',' ,  ($lat, $lon, $depth, $or_time, $mag, $magtype, $nph, $qual, $dtype ))) ;
      next;

    } else {
      next ;
    }

  } 

  return @saved; 
}


#
# random subs below here 
#

sub check_remote_list {		#&check_remote_list($match,@ll_of_remote_dir)  returns @convert_list
    my ($match, @file_list)  = @_ ;
    my ($thismonth) = epoch2str( now(), "%m") ;
    my ($thisyear)  = epoch2str( now(), "%Y") ;

    our @convert_list = () ;	# this may cause multi-ftp grabs to fail miserably

    foreach (@file_list) {

        my (@lsout)        = split(/\s+/, $_);
	my $lsfile ;
	my $last_remote_update ;

        if ($lsout[0] =~ /total/) {
            next;
        } elsif ( ((scalar @lsout == 9) && $lsout[8] =~ /$match/) || ((scalar @lsout == 4) && $lsout[3] =~ /$match/) ) {  
	    
	    if (scalar @lsout == 9) {
               $lsfile     = $lsout[8];   
               my $upd_mo     = $lsout[5];
               my $upd_dy     = $lsout[6];
               my $upd_t_yr   = $lsout[7];

               my %mo_names = (
                            Jan => 1,
                            Feb => 2,
                            Mar => 3,
                            Apr => 4,
                            May => 5,
                            Jun => 6,
                            Jul => 7,
                            Aug => 8,
                            Sep => 9,
                            Oct => 10,
                            Nov => 11,
                            Dec => 12,
               );

# looks like it will be year if time is gt 6 months previous to now, time if less than 6 mos.
               if ($upd_t_yr !~ /\d{4}/ ) {
                   $num_mo = $mo_names{$upd_mo};

                   if ($num_mo <= $thismonth) {
                       $upd_yr = $thisyear;
                   } else {
                       --$upd_yr ; 
                   }


# need to get abreviated month converted to 1-12 and current month converted to 1-12.
# if ab_month >1 and < cur_month than set upd_yr = this_yr
# else set upd_yr = this_yr - 1

                   $upd_time       = $upd_t_yr;		# because of the differences in ls results...
               } else {
                   $upd_yr         = $upd_t_yr;
                   $upd_time       = "00:00:00";
                   $num_mo         = $mo_names{$upd_mo};
               }
# get last update time of remote file (this could be replaced by an ftp->mdtm if permissions allow)
#          $last_remote_update  = $ftp->mdtm( $test_run )  || die "Couldn't get modify time on remote $lsfile \n";

               $last_remote_update  = str2epoch("$upd_mo $upd_dy $upd_yr $upd_time") || elog_complain("Couldn't find last update time of $lsfile \n" ) ;

           } else {
               $lsfile     = $lsout[3];   
               $last_remote_update  = str2epoch("$lsout[0] $lsout[1]") || elog_complain("Couldn't find last update time of $lsfile \n" ) ;
	   }

           my $gzipfile    = $localdir."/".$lsfile.".gz";
           if (-e $localdir."/".$lsfile) {
                my $local_update   = (stat($localdir."/".$lsfile))[9]  || elog_complain("Couldn't find last update time for $localdir."/".$lsfile \n") ;
#               elog_notify("file: $lsfile \tlast remote update: $last_remote_update last local update: $local_update\n") ;  
                if ($local_update < $last_remote_update) {
                    @convert_list = get_remote_ftpfile($lsfile) ;
                    next;
                } 
           } elsif (-e $gzipfile) {      # add check to see if gzipped file exists
                my $local_update   = (stat("$gzipfile"))[9]  || elog_complain("Couldn't find last update time for $gzipfile \n");
                if ($local_update < $last_remote_update) {
                    @convert_list = get_remote_ftpfile($lsfile);
                    next;
                } 
           } else {
                elog_notify("File $lsfile does not exist locally, grabbing it. \n");
                @convert_list = get_remote_ftpfile($lsfile);
                next;
           }
        } else {
#          elog_notify("no match for $lsout[8] \n") ;
          next;
        }
    }

}

sub get_remote_ftpfile {	# recovers file from remote ftp site and saves locally

  my ($ftpfile) = @_ ;

  my $outputfile = "$localdir" . "/" . "$ftpfile" ;

  elog_notify("File to get: $ftpfile.  Put file:  $outputfile\n");

  $ftp->get($ftpfile,"$outputfile")  || elog_complain("Can't retreive file $file from $remote_host or store it as $localdir"/".$file.\n");
  push(@convert_list, $outputfile );

  return (@convert_list) ;
}

sub fix_time_values {
  ($yr,$month,$day,$hr,$min,$sec) = @_;

  if ($day < 10) {
    $day =~ s/^\s+//; 
  }

  if ($month < 10) {
    $month =~ s/^\s+//; 
  }

  if ( ($min =~ /\d/) &&  ($min < 10) ){
    $min =~ s/^\s+//; 
  } elsif (!$min) {
    $min = "00" ; 
  }

  if ( ($sec =~ /\d/) && ($sec < 10) ) {
    $sec =~ s/^\s+//; 
  } elsif (!$sec || $sec =~ /\s+/) {
    $sec = "0.0";
  }

  if ( ($hr =~ /\d/) && ($hr < 10) ) {
    $hr =~ s/^\s+//; 
  } elsif (!$hr || $hr =~ /\s+/) {
    $hr = "00" ;
  }

#							#
# Attepmt to cover the case where sec or min = 60 	#
# or hr = 24. 						#
# (i.e. the input is slightly foobar).			#
#							#
  while ($sec < 0.0) {	# deal with negative seconds
    $sec 	= 60.0 + $sec ; 
    $min 	= $min - 1;
  }

  while ($sec >= 60.0) {
    $sec 	= $sec - 60.0; 
    $min 	= $min + 1;
  }

  while ($min <   0.0) {
    $min 	= 60.0 + $min ; 
    $hr 	= $hr - 1;
  }

  while ($min >= 60.0) {
    $min 	= $min - 60.0; 
    $hr 	= $hr + 1;
  }

  while ($hr  <   0.0) {
    $hr  	= 24.0 + $min ; 
    $day 	= $day - 1;
  }

  while ($hr >= 24.0) {
    $hr 	= $hr - 24.0; 
    $day 	= $day + 1;
  }

  $mytime	= sprintf ("%s\/%s\/%s %s:%s:%s", $month, $day, $yr, $hr, $min, $sec) ;

  return ($mytime);
	    
}

sub fix_lat_lon {

  my ($latd,$latm,$latNS,$lond,$lonm,$lonEW) = @_;
	
  if ($latNS =~ /S|s/)  {
    $lat = -1 * $latd; 
    $lat = $lat - ($latm/60.0);
  } else {
    $lat = $latd + ($latm/60.0);
  }

  if ($lonEW =~ /E|e/) {
    $lon = $lond + ($lonm/60.0);
  } else {
    $lon = -1 * $lond; 
    $lon = $lon - ($lonm/60.0);
  }

  return ($lat,$lon); 
}

sub fix_or_time {	#	@fix_or_time($ymd, $hour, $TZ) ;

 my ($ymd, $hr, $tz) = @_ ;
 my $epoch;
 
 if ($tz =~ /PDT|PST/) {
   $epoch = str2epoch ( "$ymd $hr US/Pacific" ) ;
 } elsif ($tz =~ /MDT|MST/) {
   $epoch = str2epoch ( "$ymd $hr US/Mountain" ) ;
 } elsif ($tz =~ /CDT|CST/) {
   $epoch = str2epoch ( "$ymd $hr US/Central" ) ;
 } elsif ($tz =~ /EDT|EST/) {
   $epoch = str2epoch ( "$ymd $hr US/Eastern" ) ;
 } else {
   $epoch = str2epoch ( "$ymd $hr" );
 }

 return ($epoch) ; 

}

sub trim {

  my @out = @_;
  for (@out) {
     s/^\s+//;
     s/\s+$//;
  }

  return wantarray ? @out : $out[0];
}

sub list2search {	# list2search(@text)  returns @outlist

  my (@textlist)  = @_; 
  my @out = () ;	# this may cause multi-ftp grabs to fail miserably

  foreach my $newline (@textlist) {
    my $linematch = 0;
    foreach my $oldline (@{$listref}) {
        if ($newline eq $oldline) {
            $linematch = 1;
            last;
        }
    }   
    if ($linematch == 0) {
        push @{$listref}, $newline ;
        push @out, $newline ;
    }   
  }

  $n = @out;
  elog_notify ("\t $n new origins to be included\n\n") ; 

  return @out ;
}

sub postreq_NBEsearch {

  my $postreq ;
  $postreq = POST $myurl, 
	     [
                 input           => "UNR2000-present.cat",
                 psoutput        => "text",
                 nmax            => "20000",
                 mintime         => "$mintime",
                 maxtime         => "$maxtime",
                 minmag          => "0.0",
                 maxmag          => "10.0",
                 mindepth        => "-3.0",
                 maxdepth        => "700.0",
                 minlat          => "30.0",
                 maxlat          => "45.0",
                 minlon          => "-124.0",
                 maxlon          => "-112.0",
             ];

  return ($postreq) ;

}

sub postreq_HYPO2000 {

  my $postreq ;
  $postreq = POST $myurl, 
	     [
		format		=> "ncraw",
		mintime		=> "$mintime",
		maxtime		=> "$maxtime",
		minmag		=> "0.0",
		maxmag		=> "9.0",
		mindepth	=> "0.0",
		maxdepth	=> "700.0",
		minlat		=> "30.0",
		maxlat		=> "43.0",
		minlon		=> "-130.0",
		maxlon		=> "-113.0",
		etype		=> "A",
		no_mag		=> "",
		keywds		=> "",
		outputloc	=> "web",
		no_mag		=> "no_mag",
		searchlimit	=> "20000",
	     ]; 

  return ($postreq) ;

}

sub postqf_AEIC {
  
  my ($geturl) = @_  ;

  my $url = url("$geturl") ;
     $url->query_form(	
		lowerlat  	=> "50.0",
		upperlat  	=> "71.0",
		leftlon		=> "172.0",
		rightlon	=> "-130.0",
		minz		=> "0.0",
		maxz		=> "700.0",
		mintime		=> "$smo"."/"."$sday"."/"."$syr",
		maxtime		=> "$emo"."/"."$eday"."/"."$eyr",
		minmag		=> "0.0",
		maxnum		=> "20000",
	     ); 

  return ($url) ; 
}

sub postqf_HYPO2000 {
  
  my ($geturl) = @_  ;

  my $url = url("$geturl") ;
     $url->query_form(	
		outputfmt	=> "hypoin",
		start_year	=> "$syr",
		start_month	=> "$smo",
		start_day  	=> "$sday",
		start_hr   	=> "$shr",
		start_min  	=> "$smin",
		start_sec  	=> "$ssec",
		end_year  	=> "$eyr",
		end_month 	=> "$emo",
		end_day   	=> "$eday",
		end_hr    	=> "$ehr",
		end_min   	=> "$emin",
		end_sec   	=> "$esec",
		min_mag		=> "0.0",
		max_mag		=> "9.0",
		min_depth	=> "0.0",
		max_depth	=> "700.0",
		south_latd		=> "28.0",
		north_latd		=> "40.0",
		west_long		=> "-122.0",
		east_long		=> "-110.0",
		etype		=> ["le","RE","ts","qb","nt","sn","st","uk"],
	     ); 

  return ($url) ; 
}

sub postreq_NESN {

  my $postreq ;
  $postreq = POST $myurl, 
	     [
		smonth		=> $smo,
		sday		=> $sday,
		syear		=> $syr,
		emonth		=> $emo, 
		eday		=> $eday, 
		eyear		=> $eyr, 
		mag_min		=> "0.0",
		mag_max		=> "9.0",
		lat_min		=> "39.0",
		lat_max		=> "49.0",
		lon_min		=> "64.0",
		lon_max		=> "79.0"
	     ]; 

  return ($postreq) ;
}

sub read_url	{	# read_url ($url)  

    my ( $geturl ) = @_  ;
    my $ua = LWP::UserAgent->new;
    my $request = HTTP::Request->new(GET => $geturl ) ;
    my $response = $ua->request($request);
    if (! $response->is_success) {
        my $err = $response->error_as_HTML ;
	elog_complain ("$err\n" ) ;
    }
   
    return htmltext ( $response ) ;

}

sub htmltext {

  my ( $response ) = @_ ;
  our ( @Text ) = () ;

  my $p = HTML::Parser->new('text_h' => [\&text, "self, text, is_cdata"]) ;
  $p->parse($response->content);
  $p->eof;
  return @Text ;

}

sub text {

  my($self, $text, $is_cdata) = @_;
  our ( @Text ) ;
  if ( $text ne "" ) {
     $text = decode_entities($text) ;
     push @Text, $text ;
  }

}

sub default_mags {	# ($ml,$mb,$ms) = default_mags(%magnitude_info)

  my (%get_magnitude_info) = @_ ; 

  my ($afoo, $bfoo) ;

  $magid = 1; 	# always the case if only a single magnitude is reported

  $ml = ( defined $get_magnitude_info{'ml'} ) ? $get_magnitude_info{'ml'} : "-999.0" ;
  $mb = ( defined $get_magnitude_info{'mb'} ) ? $get_magnitude_info{'mb'} : "-999.0" ;
  $ms = ( defined $get_magnitude_info{'ms'} ) ? $get_magnitude_info{'ms'} : "-999.0" ;

  $mlid = ( (defined $get_magnitude_info{'ml'}) ) ? 1 : -1 ;
  $mbid = ( (defined $get_magnitude_info{'mb'}) ) ? 1 : -1 ;
  $msid = ( (defined $get_magnitude_info{'ms'}) ) ? 1 : -1 ;

  if ($ml =~ /:/) {
     ($ml,$afoo,$bfoo) = split (':',$ml) ; 
  } 
  if ($mb =~ /:/) {
     ($mb,$afoo,$bfoo) = split (':',$mb) ; 
  } 
  if ($ms =~ /:/) {
     ($ms,$afoo,$bfoo) = split (':',$ms) ; 
  }

  return ($ml,$mlid,$mb,$mbid,$ms,$msid,$magid) ;

}

1;
