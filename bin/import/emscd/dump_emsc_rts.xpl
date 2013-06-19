# this beast is necessary to parse the EMSC webpage...
# 2010-07-06: new format, omitted dtype

use strict;
use warnings;
use Datascope;
use LWP;
use Data::Dumper;
use HTML::Parser;
use URI::URL;
use Getopt::Std ;

	my $defdepth=33;
	my $default_magtype="ms";
	my (@originrows,$prefor_row, $evname,  $evauth, $orid, $evid);

	my $datetag='a';
	my ($quicklist);

	my( $proxy_url,$sleeptime,$quake_url, $Shutdown_signal, $dboutname);
	our( $opt_p, $opt_s, $opt_v, $opt_d,$opt_S, $last_date_seen);


	my ($time,$lat,$lon,$jdate,$date, $newevent);
	my (@db,@dborigin, @dbevent);
	my ($evstr, $this_url, $step);
	my $num_pages=10;

	$proxy_url = "";
	$sleeptime = 900;
	#$quake_url="http://www.emsc-csem.org/index.php?page=current&sub=list&view=";
	$quake_url="http://www.emsc-csem.org/Earthquake/index.php?view=";

	if ( ! &getopts('u:p:S:s:vd:') || @ARGV != 1 ) { 
			die ( "Usage: $0 [-v] [-s seconds] [-d pages] [-S startdate] [-p proxy-url] database\n" ) ; 
	}
	elog_init($0,@ARGV);

	if ($opt_p) {
		$proxy_url = $opt_p;
		if ($proxy_url !~ /:\/\//) {
                $proxy_url = "http://" . $proxy_url;
        }
	}
	if ($opt_s) {
		$sleeptime = $opt_s;
	}
	if ($opt_d) {
		$num_pages = $opt_d + 1;
	}

	$dboutname = $ARGV[0];

	@db=dbopen($dboutname, "r+");
	@dborigin=dblookup(@db, 0, "origin", "", "dbNULL");
	@dbevent=dblookup(@db, "", "event", "", "");
	
	my $addable  ;
	eval { $addable = dbquery(@dborigin, "dbTABLE_IS_ADDABLE"); } ;
	$addable = 1 if ( $@ );
	if ( ! $addable ) { 
		my $table = dbquery(@dborigin, "dbTABLE_FILENAME") ;
		die ( "Can't add to origin table $table\n" ); 
	}
	eval { $addable = dbquery(@dbevent, "dbTABLE_IS_ADDABLE"); } ;
	$addable = 1 if ( $@ );
	if ( ! $addable ) { 
		my $table = dbquery(@dbevent, "dbTABLE_FILENAME");
		die ( "Can't add to event table $table\n" ); 
	}

	our ($mag_null) = dbgetv(@dborigin, "ms") ;


	my $parser = HTML::Parser->new(
		start_h => [ \&_starttag, 'self, tagname, attr' ],
		end_h   => [ \&_endtag,   'self, tagname' ],
		text_h  => [ \&_text,     'self, dtext' ]
	);
	$parser->report_tags(qw(a td));
	$parser->ignore_tags(qw(i));
	$parser->ignore_elements(qw(script style form));
	if ($opt_S) {
		my $startdate=str2epoch($opt_S);
		$last_date_seen=now();
		$step=0;
		while ($last_date_seen > $startdate) {
			$step++;
			$this_url=sprintf("%s%d",$quake_url,$step);
			$quicklist=get_url( $proxy_url, $this_url);
			$parser->parse( $quicklist );

		}
		exit;
	}

	for ($step=1;$step<$num_pages;$step++) {

		$this_url=sprintf("%s%d",$quake_url,$step);
		$quicklist=get_url( $proxy_url, $this_url);
		$parser->parse( $quicklist );
	}


sub get_url {
	my($my_proxy,$my_url) = @_ ;
  
    my($hdrs,$url,$req,$ua,$resp);
	$hdrs = new HTTP::Headers(Accept => 'text/plain',User_Agent => 'Antelope/1.0');
	$url = new URI::URL($my_url);
	$req = new HTTP::Request(GET => $url);
	$ua = new LWP::UserAgent;
	$ua->env_proxy;
	$ua->proxy($url->scheme, $my_proxy) if $opt_p;
	if ( $Shutdown_signal ) {
		exit 0;
	}
	$resp = $ua->request($req);
	if ($resp->is_success) {
		return $resp->content;
	} else {
		elog_complain($resp->message) if $opt_v;
		return (0);
	}
}	



sub _starttag {
    my ($self, $tag, $attr) = @_;
    $self->{'_date'} = 1 if ($tag eq $datetag);
}

sub _endtag {
    my ($self, $tag) = @_;
	return unless(defined($self->{'_next'}));
    if ($tag eq $datetag) {;
		$self->{'_date'} = undef;
		return;
	}
		if ($self->{'_next'} eq 'start') {
			$self->{'_next'} ='lat';
		} elsif ($self->{'_next'} eq 'lat') {
			$self->{'_next'} ='lats';
		} elsif ($self->{'_next'} eq 'lats') {
			$self->{'_next'} ='lon';
		} elsif ($self->{'_next'} eq 'lon') {
			$self->{'_next'} ='lons';
		} elsif ($self->{'_next'} eq 'lons') {
			$self->{'_next'} ='depth';
		} elsif ($self->{'_next'} eq 'depth') {
#$self->{'_next'} ='dtype';
			$self->{'_next'} ='magt';
		} elsif ($self->{'_next'} eq 'dtype') {
			$self->{'_next'} ='magt';
		} elsif ($self->{'_next'} eq 'magt') {
			$self->{'_next'} ='mag';
		} elsif ($self->{'_next'} eq 'mag') {
			$self->{'_next'} ='region';
		} elsif ($self->{'_next'} eq 'region') {
			$self->{'_next'} ='auth';
		} elsif ($self->{'_next'} eq 'auth') {
			$self->{'_next'} =undef;
		}
}

sub _text {
	my ($self, $dtext) = @_;

	$dtext =~ s/&nbsp;/ /g;
	$dtext =~ s/&#160;/ /g;
	$dtext =~ s/&#032;/ /g;
	$dtext =~ s/&#32;/ /g;
	$dtext =~ s/\x{A0}/ /g;
    $dtext =~ s/^\s+//m;
    $dtext =~ s/\s+$//m;
    return() unless ( length($dtext) > 0 || defined( $self->{'next'}));


    if ( defined($self->{'_date'}) ) {
		if ($dtext=~/\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}\.\d/) {
			$self->{'_time'}= $dtext;
			$self->{'_next'} ='start';
		}
	}elsif ( defined($self->{'_next'}) ) {
		if ($self->{'_next'} eq 'lat') {
			$self->{'_lat'} =$dtext;
		} elsif ($self->{'_next'} eq 'lats') {
			$self->{'_lat'}  *= -1.0 if ($dtext=~/S/i);	
		} elsif ($self->{'_next'} eq 'lon') {
			$self->{'_lon'} =$dtext;
		} elsif ($self->{'_next'} eq 'lons') {
			$self->{'_lon'}  *= -1.0 if ($dtext=~/W/i);	
		} elsif ($self->{'_next'} eq 'depth') {
			$self->{'_depth'} =$dtext;
		} elsif ($self->{'_next'} eq 'dtype') {
			$self->{'_dtype'} =$dtext;
		} elsif ($self->{'_next'} eq 'magt') {
			$self->{'_magt'} =$dtext;
		} elsif ($self->{'_next'} eq 'mag') {
			$self->{'_mag'} =$dtext;
		} elsif ($self->{'_next'} eq 'auto') {
			$self->{'_auto'} =$dtext;
		} elsif ($self->{'_next'} eq 'region') {
			$self->{'_region'} =$dtext;
		} elsif ($self->{'_next'} eq 'auth') {
			$self->{'_auth'} =$dtext;
			my($dts,$nmagt,$jdate,$row,$auth,$evstr,$grn,$srn,$orid,$evid,$evrow);
			my $dtype=defined($self->{'_dtype'}) ? $self->{'_dtype'} : "-";
			my $depth=defined($self->{'_depth'}) ? $self->{'_depth'} : $defdepth;
			my $mag=defined($self->{'_mag'}) ? $self->{'_mag'} : $mag_null;
			my $magt=defined($self->{'_magt'}) ? $self->{'_magt'} : "-";
			my $lat=$self->{'_lat'};
			my $lon=$self->{'_lon'};
			my $region=$self->{'_region'};
			eval {
			$time=str2epoch($self->{'_time'});
			};
			return() if ($@); #ignore lines with erroneous times...
			$last_date_seen=$time;
			$dts=strtime($time);
			$jdate=&yearday($time);
			$dtype=~s/\A\s+//;$dtype=~ tr/A-Z/a-z/;	
			$magt=~ tr/A-Z/a-z/;	
			if ($magt eq "mb" || $magt eq "ml" || $magt eq "ms") {
				$nmagt=$magt;
			} else {
				$nmagt=$default_magtype;
			}
			eval{
				$grn=grn($lat,$lon);
			};
			if ($@) {
				$grn=-1;
			}
			eval{
				$srn=srn($lat,$lon);
			};
			if ($@) {
				$srn=-1;
			}

			$evstr="$dts $lat $lon $depth $nmagt$mag";
			eval {
				$row=dbaddv(@dborigin, "time", $time,
					"lat",$lat,"lon",$lon,
					"depth",$depth,
					"jdate",$jdate,
					"grn",$grn,"srn",$srn,
					"nass", 0, "ndef", 0,
					"auth","EMSC",
					"review","y",
					"dtype",$dtype,
					$nmagt,$mag
				);
					
			};
			if ($@) {
				$row=undef;
#I expect errors here since I modify the origins later...
				if ($@ =~ /.*record #(\d+).*/) {
					$row=$1;
					print "found record: $row\n";
					$dborigin[3]=$row;
					eval {
						dbputv(@dborigin,"time", $time,
							"lat",$lat,"lon",$lon,
							"depth",$depth,
							"jdate",$jdate,
							"grn",$grn,"srn",$srn,
							"auth","EMSC",
							"review","y",
							"dtype",$dtype,
							$nmagt,$mag
						);
		
					};
				} else {
					elog_notify("problem adding new origin: $evstr") if ($opt_v);
				}
			} else {
				$dborigin[3]=$row;
				($orid,$evid)=dbgetv(@dborigin,"orid","evid");
				if ($evid <0) {
					elog_notify("new origin: $evstr") if ($opt_v);
					eval{
						$evrow=dbaddv(@dbevent,"prefor",$orid,"evname",$region,"auth","EMSC");
					};
					if ($@) {
						elog_complain("problem adding event for $evstr");
						
					} else {
						$dbevent[3]=$evrow;
						($evid)=dbgetv(@dbevent,"evid");
						dbputv(@dborigin,"evid",$evid);
					}
				}
				
			}
			$time= undef;
			$self->{'_next'}=undef;
			$self->{'_lat'}=undef;
			$self->{'_lon'}=undef;
			$self->{'_magt'}=undef;
			$self->{'_mag'}=undef;
			$self->{'_dtype'}=undef;
			$self->{'_depth'}=undef;
			$self->{'_auth'}=undef;
			$self->{'_region'}=undef;
		}
    }
}

