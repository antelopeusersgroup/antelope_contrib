
# reb2db for IMS1.0, IMS1.0:SHORT, and GSE2.0 format REB bulletins
#
# Kent Lindquist
# Geophysical Institute
# University of Alaska
# December, 1999 (IMS1.0)
# September 2000 (Added GSE2.0 support)
# October 2000 (IMS1.0:SHORT)
# November 2005 (ARRIVAL:AUTOMATIC)
# March 2006 case insensitive state switching, following data seen from ISC
# July 2006	 no assocs, no stamags for arrivals that have been forced
#            no arrivals if preceeeding event had NO origins (seen on ISC webpage)!
#			 allow for multiple origins, assuming last one is preferred (ISC practice)


use Datascope ;

use vars qw($opt_d);

use Getopt::Std;

if ( ! getopts('d') || @ARGV < 2 ) {

	die( "Usage: $0 [-d] filename [filename ...] database\n" );

}

sub init_globals {

# since I use this only to write detections from CTBTO, this works. 
# The "regular" approach to obtain net/sta/chan would be:
# 	$sta= map_autodrm_netsta($anet,$fsta)	
#	$chan= map_autodrm_chanaux($sta,$fchan,$aux)	
#	$snet= seed_net($sta)
#	
#	the filter is not specified in the input, but part of the primary key	
	$default_net="IM";
	$default_chan="BHZ";
	$default_filter="unk";
	
	$Phase_unpack{"IMS1.0:ARRIVAL:AUTOMATIC"}="A9 x A5 x A12 x A10 x A12 x A8 " .
	   "x A5 x A5 x A5 x A9 x A5 x A5 x A5 x A9 x A8";
  	$Phase_names{"IMS1.0:ARRIVAL:AUTOMATIC"}= ["ph_net", "ph_sta", "ph_beamid", "ph_date", "ph_time",
	   "ph_code", "ph_az", "ph_slow", "ph_snr", "ph_amp_nm", "ph_per", "ph_short_time_average",
	   "ph_duration", "ph_auth", "ph_arid" ];
	
	$State = "startup";

# format change ~ Aug 2005	
#$Event_unpack{"IMS1.0-line1"} = "A22 A1 x A5 x A5 x A8 x A9 A1 x A4 " .
	$Event_unpack{"IMS1.0-line1"} = "A22 A1 x A5 x A5 x A8 x A9 A1 A5 " .
	  "x A5 x A3 x A5 A1 x A4 x A4 x A4 x A3 x A6 x A6 x A1 x A1 x A2 " .
	  "x A9 x A8";

	$Event_names{"IMS1.0-line1"} = [ "or_timestr", "or_fixedtime", 
	  "or_timeerr", "or_rms", "or_lat", "or_lon", "or_fixedepi", 
	  "or_smaj", "or_smin", "or_strike", "or_depth", "or_fixdpth", 
	  "or_deptherr", "or_ndef", "or_nsta", "or_gap", "or_nearest", 
	  "or_furthest", "or_antype", "or_locmeth", "or_evtype", 
	  "or_auth", "orid" ];

	$Event_unpack{"IMS1.0:SHORT-line1"} = $Event_unpack{"IMS1.0-line1"};

	$Event_names{"IMS1.0:SHORT-line1"} = $Event_names{"IMS1.0-line1"};

	$Event_unpack{"GSE2.0-line1"} = "A21 x A1 x2 A8 x A9 x A1 x2 A5 x " .
	  "A1 x2 A4 x A4 x A3 x2 A2 A4 x A2 x2 A2 A4 x A2 x2 A2 A4 x A2 x2 " .
	  "A8 x2 A8";

	$Event_names{"GSE2.0-line1"} = [ "or_timestr", "or_fixedtime", 
	  "or_lat", "or_lon", "or_fixedepi", "or_depth", "or_fixdpth", 
	  "or_ndef", "or_nsta", "or_gap", "or_magtype1", "or_mag1", 
	  "or_magnsta1", "or_magtype2", "or_mag2", "or_magnsta2", 
	  "or_magtype3", "or_mag3", "or_magnsta3", "or_auth", "orid" ];

	$Event_unpack{"GSE2.0-line2"} = "x5 A5 x5 A6 x4 A6 x A6 x2 A3 x6 " .
	  "A5 x2 A6 x A6 x5 A3 x8 A3 x8 A3 x5 A1 x A1 x A2";

	$Event_names{"GSE2.0-line2"} = [ "or_rms", "or_timeerr", "or_smaj", 
	  "or_smin", "or_strike", "or_deptherr", "or_nearest", "or_furthest",
	  "or_mag1err", "or_mag2err", "or_mag3err", "or_antype", "or_locmeth",
	  "or_evtype" ];

	$Phase_unpack{"IMS1.0"} = "A5 x A6 x A5 x A8 x A12 x A5 x A5 x A5 x " .
	  "A6 x A6 x A1 A1 A1 x A5 x A9 x A5 x A1 A1 A1 x A5 A1 A4 x A8";

	$Phase_names{"IMS1.0"} = [ "ph_sta", "ph_delta", "ph_esaz", "ph_code",
	  "ph_arrtime", "ph_timeres", "ph_az", "ph_azres", "ph_slow",
	  "ph_slowres", "ph_timedef", "ph_azdef", "ph_slowdef", "ph_snr",
	  "ph_amp_nm", "ph_per", "ph_type", "ph_fm", "ph_qual", "ph_magtype",
	  "ph_minmax", "ph_mag", "ph_arid" ];

	$Phase_unpack{"IMS1.0:SHORT"} = $Phase_unpack{"IMS1.0"};

	$Phase_names{"IMS1.0:SHORT"} =  $Phase_names{"IMS1.0"};

	$Phase_unpack{"GSE2.0"} = "A5 x A6 x A5 x A1 A1 A1 x A7 x A21 x A5 x " .
	  "A5 x A6 x A5 x A5 x A1 A1 A1 x A5 x A9 x A5 x A2 A4 x A2 A4 x A8";

	$Phase_names{"GSE2.0"} = [ "ph_sta", "ph_delta", "ph_esaz", "ph_type",
	  "ph_fm", "ph_qual", "ph_code", "ph_arrtime", "ph_timeres", "ph_az",
	  "ph_azres", "ph_slow", "ph_slowres", "ph_timedef", "ph_azdef",
	  "ph_slowdef", "ph_snr", "ph_amp_nm", "ph_per", "ph_magtype1", 
	  "ph_mag1", "ph_magtype2", "ph_mag2", "ph_arid" ];

	$Mag_unpack{"IMS1.0"} = "A5 A1 A4 x A3 x A4 x A9 x A8";

	$Mag_names{"IMS1.0"} = [ "mag_type", "mag_minmax", "mag_val", "mag_err",
	  "mag_nsta", "mag_auth", "mag_orid" ];

	$Mag_unpack{"IMS1.0:SHORT"} = $Mag_unpack{"IMS1.0"};

	$Mag_names{"IMS1.0:SHORT"} = $Mag_names{"IMS1.0"};

	use vars qw(%Phase_unpack %Event_unpack %Mag_unpack
		    $or_strike $or_antype $or_deptherr $or_smaj $or_smin
		    $or_rms $or_depth $or_timeerr $or_evtype $or_fixedtime
		    $or_mag1 $or_mag2 $or_mag3 $or_magtype1 $or_magtype2
		    $or_magtype3 $or_magnsta1 $or_magnsta2 $or_magnsta3
		    $or_mag1err $or_mag2err $or_mag3err
		    $mag_type $mag_val $mag_nsta $mag_err
		    $ph_qual $ph_arrtime $ph_slow $ph_fm $ph_amp_nm $ph_snr
		    $ph_code $ph_per $ph_magtype $ph_az $ph_delta $ph_azres
		    $ph_esaz $ph_slowres $ph_mag $ph_timeres $ph_azdef $ph_slowdef
		    $ph_timedef $ph_type $ph_mag1 $ph_mag2 $ph_magtype1
		    $ph_magtype2
			$default_net $default_chan $default_filter
			$ph_beamid $ph_date $ph_time $ph_short_time_average $ph_net $ph_sta
			@Db_arrival @Db_detection @Db_stamag @Db_netmag @Db_event @Db_origin 
			@Db_origerr @Db_assoc @Db
			);
}

sub stateswitch {
	( $line ) = @_;

	if( $line =~ /^\s*DATA_TYPE\s+BULLETIN\s+(\S+)/i ) {
		$format = uc( $1 );
		if( $format ne "IMS1.0" &&
		    $format ne "IMS1.0:SHORT" &&
		    $format ne "GSE2.0" ) {
			die( "File $ARGV Not in IMS1.0, IMS1.0:SHORT, " .
			     "or GSE2.0 format\n" ); 
		}
		$State = "searching";
		return 1;
	}
	if( $line =~ /^\s*EVENT\s+(\d+)/i ) {
		$evid = $1;
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--assume IMS1.0 format\n";
			$format = "IMS1.0";
		}
		$State = "event";
		$Event_line = 0;
		%Magid_cache = ();
		local ( $key ) = $format . "-line1";

		eval "(" . join( ",", map { "\$"."$_" } @{$Event_names{$key}} ) . ") =''";
		
		return 1;
	}

	if( $line =~ /^\s*DATA_TYPE\s+ARRIVAL:(\S+)\s+(\S+)/ ) {
		$subformat= uc( $1 );
		$format = uc( $2 );
		if( $format ne "IMS1.0" &&
		    $format ne "GSE2.0" &&
		    $subformat ne "AUTOMATIC" &&
		    $subformt ne "REVIEWED" && 
		    $subformat ne "GROUPED" && 
		    $subformt ne "ASSOCIATED" && 
		    $subformt ne "UNASSOCIATED" ) {
			die( "File $ARGV Not in IMS1.0, IMS1.0:SHORT, " .
			     "or GSE2.0 format\n" ); 
		}
		$format="$format:ARRIVAL:$subformat";
		$State = "searching";
		return 1;
	}
	
	if( $line =~ /^\s*Magnitude/i ) {
		$State = "magnitude";
		return 1;
	}
	if( $line =~ /^\s*Sta\s+Dist/i ) {
		$State = "phase";
		return 1;
	}

	if ( $line =~/^\s*Net\s+Sta/i ) {
		if ($State eq "startup" ) {
			die("unknown format\n");
		}
		$State = "phase";
		return 1;
	}

	return 0;
}

sub dtype {
	if( $or_fixdpth eq "f" ) {
		if( $or_antype eq "a" ) {
			return "r"; #restrained by location program
		} else {
			return "g"; #restrained by geophysicist
		} 
	} elsif( $or_fixdpth eq "d" ) {
		return "d";	# depth phases
	} else {
		return "f";	# free
	} 
}

sub deptherr {

	if( $or_fixdpth eq "f" || $or_fixdpth eq "d" ) {

		return 0;

	} else {

		return $or_deptherr;
	}
}

sub phase_fm {
	
	for( $ph_fm ) {
		/c/ && return "c.";
		/d/ && return "d.";
		/_/ && return "-";
	}

	return "-";
}

sub phase_qual {

	for( $ph_qual ) {
		/i/ && return "i";
		/e/ && return "e";
		/q/ && return "w";
		/_/ && return "-";
	}
	
	return "-";
}

sub etype {
	
	for( $or_evtype ) {
		/uk/ && return "-";
		/ke/ && return "eq";
		/se/ && return "eq";
		/kr/ && return "-";
		/sr/ && return "-";
		/ki/ && return "eq";
		/si/ && return "eq";
		/km/ && return "me";
		/sm/ && return "me";
		/kx/ && return "ex";
		/sx/ && return "ex";
		/kn/ && return "ex";
		/sn/ && return "ex";
		/ls/ && return "-";
	}

	return "-";
}

sub loc_algorithm {
	
	local( $algorithm );

	if( $or_antype eq "a" ) {

		$algorithm = "auto";

	} elsif( $or_antype eq "m" ) {

		$algorithm = "man";

	} elsif( $or_antype eq "g" ) {

		$algorithm = "guess";

	} else {

		$algorithm = "";
	}

	if( $or_locmeth eq "i" ) {

		$algorithm .= ":inversion";

	} elsif( $or_locmeth eq "p" ) {

		$algorithm .= ":patnrec";

	} elsif( $or_locmeth eq "g" ) {

		$algorithm .= ":grndtruth";

	} elsif( $or_locmeth eq "o" ) {

		$algorithm .= ":other"
	}

	$algorithm =~ s/^://;

	return $algorithm;
}

sub write_netmag_row {
	
	local( $type, $val, $nsta, $err ) = @_;
	
	local( $lc_type ) = lc( $type );

	if( $type eq "" || $val eq "" ) { 
		return;
	}
	
	local( $magid ) = dbnextid( @Db, "magid" );

	$Magid_cache{$type} = $magid;

#@Db = dblookup( @Db, "", "netmag", "", "" );
	@Db = @Db_netmag;


	dbaddv( @Db,
         	"magid", $magid,
	#        "net", $net,     
	        "orid", $orid,   
	        "evid", $evid,   
	        "magtype", $type,     
	        "nsta", $nsta ne "" ? $nsta : -1,   
	        "magnitude", $val,         
	        "uncertainty", $err ne "" ? $err : -1,     
	        "auth", $or_auth,   
	#        "commid", commid
         	);

	if( $lc_type eq "mb" ||
	    $lc_type eq "ms" || 
	    $lc_type eq "ml" ) {
#@Db = dblookup( @Db, "", "origin", "orid", "$orid" );
		@Db = @Db_origin;
		if( $Db[3] < 0 ) { return; }
		dbputv( @Db, $lc_type, $val, $lc_type . "id", $magid );
	}
}

sub write_net_magnitude {

	if( $format eq "GSE2.0" ) {

		write_netmag_row($or_magtype1, $or_mag1, $or_magnsta1, $or_mag1err);
		write_netmag_row($or_magtype2, $or_mag2, $or_magnsta2, $or_mag2err);
		write_netmag_row($or_magtype3, $or_mag3, $or_magnsta3, $or_mag3err);

	} elsif( $format eq "IMS1.0" || $format eq "IMS1.0:SHORT" ) {
		
		write_netmag_row( $mag_type, $mag_val, $mag_nsta, $mag_err );

	} else {

		return;
	}
}

sub write_hypocenter {
	
#@Db = dblookup( @Db, "", "event", "", "" );
	@Db = @Db_event;


	eval {
		dbaddv( @Db,
				"evid", $evid,
		#       "evname", evname,        
				"prefor", $orid,
				"auth", $or_auth
		#       "commid", commid,
				);
	};
	if ($@) {
		if ($@ =~ /.*record #(\d+).*/) {
			$row=$1;
			$Db[3]=$row;
			dbputv(@Db,"prefor", $orid, "auth", $or_auth);
		}
	}

#@Db = dblookup( @Db, "", "origin", "", "" );
	@Db = @Db_origin;

	eval {
		dbaddv( @Db, 
				"lat", $or_lat,
				"lon", $or_lon,
				"depth", $or_depth,
				"time", str2epoch( $or_timestr ),
				"orid", $orid,
				"evid", $evid,
				"jdate", yearday( str2epoch( $or_timestr ) ),
				"nass", $or_ndef,
				"ndef", $or_ndef,
			 #	"ndp", ndp,
				"grn", grn( $or_lat, $or_lon ),
				"srn", srn( $or_lat, $or_lon ),
				"etype", etype(),
			 #	"review", review,
			 #	"depdp", depdp,
				"dtype", dtype(),
			 #	"mb", mb,
			 #	"mbid", mbid,
			 #	"ms", ms,
			 #	"msid", msid,
			 #	"ml", ml,
			 #	"mlid", mlid,
				"algorithm", loc_algorithm(),
				"auth", $or_auth
			 #	"commid", commid
				);
	};
	if ($@) {
# ISC gives multiple origins, and sometimes the last one is a repetition 
		print STDERR "\nWarning: problem adding origin:\n $@";
		print STDERR "...Forcing the addition of orid $orid from file '$ARGV' (evid $evid auth $or_auth)\n\n";
		$Db[3]=dbaddnull( @Db );
		dbputv( @Db, 
				"lat", $or_lat,
				"lon", $or_lon,
				"depth", $or_depth,
				"time", str2epoch( $or_timestr ),
				"orid", $orid,
				"evid", $evid,
				"jdate", yearday( str2epoch( $or_timestr ) ),
				"nass", $or_ndef,
				"ndef", $or_ndef,
			 #	"ndp", ndp,
				"grn", grn( $or_lat, $or_lon ),
				"srn", srn( $or_lat, $or_lon ),
				"etype", etype(),
			 #	"review", review,
			 #	"depdp", depdp,
				"dtype", dtype(),
			 #	"mb", mb,
			 #	"mbid", mbid,
			 #	"ms", ms,
			 #	"msid", msid,
			 #	"ml", ml,
			 #	"mlid", mlid,
				"algorithm", loc_algorithm(),
				"auth", $or_auth
			 #	"commid", commid
				);
	}

#@Db = dblookup( @Db, "", "origerr", "", "" );
	@Db = @Db_origerr;

	dbaddv( @Db,
        	"orid", $orid,
	#        "sxx", sxx,     
	#        "syy", syy,     
	#        "szz", szz,     
	#        "stt", stt,     
	#        "sxy", sxy,     
	#        "sxz", sxz,     
	#        "syz", syz,     
	#        "stx", stx,     
	#        "sty", sty,     
	#        "stz", stz,     
	        "sdobs", $or_rms,         
	        "smajax", $or_smaj,       
		"sminax", $or_smin,       
	        "strike", $or_strike,       
		"sdepth", deptherr(),
	        "stime", $or_fixedtime eq "f" ? 0 : $or_timeerr,
	        "conf", 0.9
	#        "commid", commid,       
	);

	if( $format eq "GSE2.0" ) {
		
		write_net_magnitude;
	}
}

sub write_stamag_row {

	local( $type, $val ) = @_;

	if( $type eq "" || $val eq "" ) {

		return;
	}

#@Db = dblookup( @Db, "", "stamag", "", "" );
	@Db = @Db_stamag;

	dbaddv( @Db,
	        "magid", $Magid_cache{$type},         
       		"sta", $ph_sta,
       		"arid", $ph_arid,
       		"orid", $orid,
        	"evid", $evid,   
        	"phase", $ph_code,         
       		"magtype", $type,
        	"magnitude", $val,         
	#        "uncertainty", uncertainty,     
        	"auth", $or_auth   
	#        "commid", commid,       
       		);
}

sub write_phase {

	# Simple trap for a common apparent bug in current PIDC web
	# server for REB bulletins: almost-blank phase rows are
	# sometimes returned that echo the magnitude field:
	if( $ph_sta eq "" ) { return; } 


	


	if( $format eq "IMS1.0" || $format eq "IMS1.0:SHORT" ) {
		# Another problem seen with data from ISC: 
		# almost blank lines with amplitude and phase, 
		# without time and phasename
		if( $ph_arrtime eq "" ) { return; } 
		#if( $ph_code eq "" ) { return; } 

		if ($or_timestr eq "") { return; }
		
		$arrival_auth= $or_auth;
		
		$origin_time = str2epoch( $or_timestr );

		$arrival_time = str2epoch( strdate( $origin_time ) ) + 
				str2epoch( $ph_arrtime );

		if( $arrival_time < $origin_time ) {
			# Wrapping error due to IMS storage
			# of arrival times without date
			$arrival_time += 86400;
		}
	} elsif( $format =~/IMS1.0:ARRIVAL/ ) {
		$arrival_time = str2epoch( "$ph_date $ph_time" );
		$arrival_auth= $ph_auth;
	} else {
		$arrival_time = str2epoch( $ph_arrtime );
	}

	@Db = @Db_arrival;
	eval {
		dbaddv( @Db,
         		"sta", $ph_sta,
         		"time", $arrival_time,
	        	"arid", $ph_arid,   
	        	"jdate", yearday( $arrival_time ),         
		#        "stassid", stassid,     
		#        "chanid", chanid,       
		#        "chan", chan,   
	        	"iphase", $ph_code,       
		#        "stype", stype,         
		#        "deltim", deltim,       
	        	"azimuth", $ph_az ne "" ? $ph_az : -1,     
		#        "delaz", delaz,         
	        	"slow", $ph_slow ne "" ? $ph_slow : -1,   
		#        "delslo", delslo,       
		#        "ema", ema,     
		#        "rect", rect,   
	        	"amp", $ph_amp_nm ne "" ? $ph_amp_nm : -1,     
	        	"per", $ph_per ne "" ? $ph_per : -1,     
		#        "logat", logat,         
		#        "clip", clip,   
	        	"fm", phase_fm(),       
	        	"snr", $ph_snr ne "" ? $ph_snr : -1,     
	        	"qual", phase_qual(),
	        	"auth", $arrival_auth . ( $ph_type eq "a" ? ":auto" : "" )
		#        "commid", commid,       
         		);
	};

	if( $@ ) {
		print STDERR "\nWarning: problem adding phase:\n $@";
		print STDERR "...Forcing the addition of arid $ph_arid from file '$ARGV' (orid $orid evid $evid)\n\n";
		$Db[3] = dbaddnull( @Db );
		dbputv( @Db,
         		"sta", $ph_sta,
         		"time", $arrival_time,
	        	"arid", $ph_arid,   
	        	"jdate", yearday( $arrival_time ),         
		#        "stassid", stassid,     
		#        "chanid", chanid,       
		#        "chan", chan,   
	        	"iphase", $ph_code,       
		#        "stype", stype,         
		#        "deltim", deltim,       
	        	"azimuth", $ph_az ne "" ? $ph_az : -1,     
		#        "delaz", delaz,         
	        	"slow", $ph_slow ne "" ? $ph_slow : -1,   
		#        "delslo", delslo,       
		#        "ema", ema,     
		#        "rect", rect,   
	        	"amp", $ph_amp_nm ne "" ? $ph_amp_nm : -1,     
	        	"per", $ph_per ne "" ? $ph_per : -1,     
		#        "logat", logat,         
		#        "clip", clip,   
	        	"fm", phase_fm(),       
	        	"snr", $ph_snr ne "" ? $ph_snr : -1,     
	        	"qual", phase_qual(),
	        	"auth", $arrival_auth . ( $ph_type eq "a" ? ":auto" : "" )
		#        "commid", commid,       
         		);
	} elsif( $format !~/IMS1.0:ARRIVAL/ ) {

#@Db = dblookup( @Db, "", "assoc", "", "" );
		@Db = @Db_assoc;

		dbaddv( @Db,
				"arid", $ph_arid,
				"orid", $orid,
				"sta", $ph_sta,     
				"phase", $ph_code,         
		#        "belief", belief,       
				"delta", $ph_delta ne "" ? $ph_delta : -1,         
		#        "seaz", seaz,   
				"esaz", $ph_esaz ne "" ? $ph_esaz : -1,   
				"timeres", $ph_timeres ne "" ? $ph_timeres : -999.0,     
				"timedef", $ph_timedef eq "T" ? "d" : "n",     
				"azres", $ph_azres ne "" ? $ph_azres : -999.0,         
				"azdef", $ph_azdef eq "A" ? "d" : "n",         
				"slores", $ph_slowres ne "" ? $ph_slowres : -999.0,
				"slodef", $ph_slowdef eq "S" ? "d" : "n"
		#        "emares", emares,       
		#        "wgt", wgt,     
		#        "vmodel", vmodel,       
		#        "commid", commid,       
				);


		if( $format eq "IMS1.0" || $format eq "IMS1.0:SHORT" ) {

			write_stamag_row( $ph_magtype, $ph_mag );

		} elsif( $format eq "GSE2.0" ) {

			write_stamag_row( $ph_magtype1, $ph_mag1 );
			write_stamag_row( $ph_magtype2, $ph_mag2 );
		}
	}
}

sub write_detection {
	if( $ph_sta eq "" ) { return; } 
	$arrival_time = str2epoch( "$ph_date $ph_time" );
	$srcid= sprintf("%s_%s\/%s", $default_net, $ph_sta, $default_filter ),
	eval {
		dbaddv( @Db_detection,
	        	"srcid", $srcid,
				"net", $default_net, 
         		"sta", $ph_sta,
				"chan", $default_chan,
         		"time", $arrival_time,
#				"arid", $ph_arid,   
				"state", $ph_code,
				"filter", $default_filter,
	        	"snr", $ph_snr ne "" ? $ph_snr : -1,     
         		);
	};

}

sub event {
	( $line ) = @_;

	local( $key );

	if( $line =~ /^\s*Date\s+Time/ ) { return; }
	if( $line =~ /^\s*rms\s+OT_Error/ ) { return; }
	if( $line =~ /^\s*\(.*\)\s*$/ ) { return; } # comment
	if( $format eq "GSE2.0" && $line !~ /\d/ ) { return; } # gregion name
	
		if( $format eq "GSE2.0" ) { 
		$key= $format . "-line" . ++$Event_line;
	} else {
		$key= $format . "-line1";
	}	
	
	eval "(" . join( ",", map { "\$"."$_" } @{$Event_names{$key}} ) .
		")" . "= unpack( \$Event_unpack{\$key}, \$line )";
	
	map { eval "\$$_ =~ s/^\\s+//;" } @{$Event_names{$key}};

	if( $format eq "IMS1.0" ||
	    $format eq "IMS1.0:SHORT" ||
	    ( $format eq "GSE2.0" && $Event_line == 2 ) ) {

		write_hypocenter;
	}
}

sub magnitude {
	( $line ) = @_;
	
	eval "(" . join( ",", map { "\$"."$_" } @{$Mag_names{$format}} ) .
		")" . "= unpack( \$Mag_unpack{\$format}, \$line )";
	
	map { eval "\$$_ =~ s/^\\s+//;" } @{$Mag_names{$format}};

	write_net_magnitude;
}

sub phase {
	( $line ) = @_;
	if( $line =~ /^\s*\(.*\)\s*$/ ) { return; } # comment
		
	eval "(" . join( ",", map { "\$"."$_" } @{$Phase_names{$format}} ) .
		")" . "= unpack( \$Phase_unpack{\$format}, \$line )";
	
	map { eval "\$$_ =~ s/^\\s+//;" } @{$Phase_names{$format}};

	write_phase;

	if ( $opt_d && $format=~/IMS1.0:ARRIVAL/i ) { write_detection };
	
}

sub searching {
	( $line ) = @_;
	# intentional null routine
}

sub startup {
	( $line ) = @_;
	# intentional null routine
}




if( $#ARGV < 1 ) {
	die "Usage: reb2db filename [filename ...] dbname\n";
} else {
	$dbname = pop( @ARGV );
	@Db = dbopen( $dbname, "r+" );
}

init_globals;

@Db_arrival= 	dblookup(@Db, "", "arrival", "", "");
@Db_assoc= 		dblookup(@Db, "", "assoc", "", "");
@Db_detection=	dblookup(@Db, "", "detection", "", "");
@Db_event=		dblookup(@Db, "", "event", "", "");
@Db_origin=		dblookup(@Db, "", "origin", "", "");
@Db_origerr=	dblookup(@Db, "", "origerr", "", "");
@Db_netmag=		dblookup(@Db, "", "netmag", "", "");
@Db_stamag=		dblookup(@Db, "", "stamag", "", "");

while( <ARGV> ) {
	chop;
	next if( /^\s*$/ );
	next if( /^\s*\(.*\)\s*$/ ); # comment
	last if ( /^STOP$/i ); #last line to be processed, avoid confuusing error with ISC data	
	next if stateswitch( $_ );
	&$State( $_ );
} 
