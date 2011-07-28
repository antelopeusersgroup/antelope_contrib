# pick2db
# Convert hypoellipse pickfiles to datascope format
# Kent Lindquist
# 11/95

use Datascope ; 
use Getopt::Std;
require "getcwd.pl";

sub clear_globals {
	$user = "";
	$auth = "-";
	$origin_lat = "";
	$origin_lon = "";
	$origin_depth = "";
	$origin_epoch = "";
	$evid = -1;
	$orid = -1;
	$ml_auth = "-";
	$ms_auth = "-";
	$mb_auth = "-";
	$md_auth = "-";
	$md = -999.00;
	$mdid = -1;
	$origin_mb = -999.00;
	$origin_mbid = -1;
	$origin_ms = -999.00;
	$origin_msid = -1;
	$origin_ml = -999.00;
	$origin_mlid = -1;
}

sub identify_summary_line {
        local($line) = @_;
	my( $is_summary_line );

	if( substr( $line, 80, 1 ) eq "/" ) {

		$is_summary_line = 1;
		$twodigit_year = 1;
		$time_format = "A14";

	} elsif( substr( $line, 82, 1 ) eq "/" ) {

		$is_summary_line = 1;
		$twodigit_year = 0;
		$time_format = "A16";

	} else {
		$is_summary_line = 0;
		$time_format = "";
	}

	$summary_unpack = "$time_format A7 A8 A5 A2 A3 A3 A3 A4 A3 A2 A4 A3 A2 A4 A2 A2 A1 A4 A1 A1 A2 x A4 A4 A1 A1 A5 A4 A2 A2 A4 A2 A5" ;
	
	return $is_summary_line;
}

sub unpack_summary {
        local($line) = @_;

        ( $sum_time, $sum_lat, $sum_lon, $sum_depth, $sum_mag, $sum_npha, $sum_gap, $sum_dist1, $sum_rms, $sum_azim1, $sum_dip1, $sum_stder1, $sum_azim2, $sum_dip2, $sum_stder2, $sum_xmag, $sum_fmag, $sum_p, $sum_stder, $sum_q, $sum_magtyp, $sum_ns, $sum_inst, $sum_moyr, $sum_t, $sum_f, $sum_seq_num, $sum_sptim, $sum_zup, $sum_zdn, $sum_vpvs, $sum_wto, $sum_extra_depth) = unpack($summary_unpack, $line);

        $sum_time =~ s/^\s+//;
	$sum_lat =~ s/^\s+//;
	$sum_lon =~ s/^\s+//;
	$sum_depth =~ s/^\s+//;
	$sum_mag =~ s/^\s+//;
	$sum_npha =~ s/^\s+//;
	$sum_gap =~ s/^\s+//;
	$sum_dist1 =~ s/^\s+//;
	$sum_rms =~ s/^\s+//;
	$sum_azim1 =~ s/^\s+//;
	$sum_dip1 =~ s/^\s+//;
	$sum_stder1 =~ s/^\s+//;
	$sum_azim2 =~ s/^\s+//;
	$sum_dip2 =~ s/^\s+//;
	$sum_stder2 =~ s/^\s+//;
	$sum_xmag =~ s/^\s+//;
	$sum_fmag =~ s/^\s+//;
	$sum_p =~ s/^\s+//;
	$sum_stder =~ s/^\s+//;
	$sum_q =~ s/^\s+//;
	$sum_magtyp =~ s/^\s+//;
	$sum_ns =~ s/^\s+//;
	$sum_inst =~ s/^\s+//;
	$sum_moyr =~ s/^\s+//;
	$sum_t =~ s/^\s+//;
	$sum_f =~ s/^\s+//;
	$sum_seq_num =~ s/^\s+//;
	$sum_sptim =~ s/^\s+//;
	$sum_zup =~ s/^\s+//;
	$sum_zdn =~ s/^\s+//;
	$sum_vpvs =~ s/^\s+//;
	$sum_wto =~ s/^\s+//;
	$sum_extra_depth =~ s/^\s+//;
}
 
$phase_unpack = "A4 A2 A1 A1 A1 A10 A5 A4 A3 A5 A3 A1 A3 A4 A3 A4 A3 A1 x2 A1 A1 A2 A1 A5 A5 A5 A3 A1 A5 A3 A3 A3 A2 A2 A2 A1 A1 A1 A1 A1 A1";

sub unpack_phase {
        local($line) = @_;

	($phs_sta, $phs_Pcode, $phs_fm, $phs_Weight, $phs_refrac_layer, $phs_basetime, $phs_Psec, $phs_dist, $phs_azim, $phs_Ssec, $phs_srmk, $phs_Sweight, $phs_takeoff, $phs_mxamp, $phs_per, $phs_ptt_cal, $phs_Pse, $phs_PR, $phs_Siemens_gain, $phs_gain_range, $phs_RMK, $phs_fm_corr, $phs_time_cor, $phs_FP_time, $phs_Presid, $phs_Sse, $phs_SR, $phs_Sresid, $phs_Pdly, $phs_Sdly, $phs_Edly, $phs_Sys, $phs_xmg, $phs_fmg, $phs_P_sc, $phs_P_sc, $phs_S_sc, $phs_Amp_sc, $phs_Coda_sc, $phs_hops ) = unpack($phase_unpack, $line);

	$phs_sta =~ s/^\s+//;
	$phs_Pcode =~ s/^\s+//;
	$phs_fm =~ s/^\s+//;
	$phs_Weight =~ s/^\s+//;
	$phs_refrac_layer =~ s/^\s+//;
	$phs_basetime =~ s/^\s+//;
	$phs_Psec =~ s/^\s+//;
	$phs_dist =~ s/^\s+//;
	$phs_azim =~ s/^\s+//;
	$phs_Ssec =~ s/^\s+//;
	$phs_srmk =~ s/^\s+//;
	$phs_Sweight =~ s/^\s+//;
	$phs_takeoff =~ s/^\s+//;
	$phs_mxamp =~ s/^\s+//;
	$phs_per =~ s/^\s+//;
	$phs_ptt_cal =~ s/^\s+//;
	$phs_Pse =~ s/^\s+//;
	$phs_PR =~ s/^\s+//;
	$phs_Siemens_gain =~ s/^\s+//;
	$phs_gain_range =~ s/^\s+//;
	$phs_RMK =~ s/^\s+//;
	$phs_fm_corr =~ s/^\s+//;
	$phs_time_cor =~ s/^\s+//;
	$phs_FP_time =~ s/^\s+//;
	$phs_Presid =~ s/^\s+//;
	$phs_Sse =~ s/^\s+//;
	$phs_SR =~ s/^\s+//;
	$phs_Sresid =~ s/^\s+//;
	$phs_Pdly =~ s/^\s+//;
	$phs_Sdly =~ s/^\s+//;
	$phs_Edly =~ s/^\s+//;
	$phs_Sys =~ s/^\s+//;
	$phs_xmg =~ s/^\s+//;
	$phs_fmg =~ s/^\s+//;
	$phs_P_sc =~ s/^\s+//;
	$phs_P_sc =~ s/^\s+//;
	$phs_S_sc =~ s/^\s+//;
	$phs_Amp_sc =~ s/^\s+//;
	$phs_Coda_sc =~ s/^\s+//;
	$phs_hops =~ s/^\s+//;
}

sub process_comment {
	local($comment_type) = pop(@_);
	local($comment) = @_;

	if($comment_type eq "T") {
		$comment =~ /USER = (\w+)/;
		$user = $1;
	}
}

sub summary_has_origin_keys {
	local($origin_time);
	my( $year, $month, $day, $hour, $min, $sec );

	if( $twodigit_year ) {
		$year_format = "\\d\\d";
	} else {
		$year_format = "\\d\\d\\d\\d";
	}

	$origin_time = $sum_time;
	$origin_time =~ tr/ /0/;
	$origin_time =~ /^($year_format)(\d\d)(\d\d)(\d\d)(\d\d)(\d\d\d\d)/ || 
		return 0;

	$year = $1;
	$month = $2;
	$day = $3;
	$hour = $4;
	$min = $5;
	$sec = $6;

	# Add the seconds on afterwards; $sec can be > 60 in a pickfile
	$sec = $sec / 100;
	$origin_epoch = str2epoch("$month/$day/$year $hour:$min");
	$origin_epoch += $sec;

	$sum_lat =~ /^([\d ]\d)([nsNS -])([\d ]*\d[\d ]*)/ || return 0;

	$origin_lat = ($1 + ($3 / 6000)) * ($2 =~ /[sS-]/ ? -1 : 1);

	$sum_lon =~ /^([\d ][\d ]\d)([ewEW -])([\d ]*\d[\d ]*)/ || return 0;

	$origin_lon = ($1 + ($3 / 6000)) * ($2 =~ /[wW-]/ ? -1 : 1);

	# Negative-depth pickfiles have a "-0" in the standard depth 
	# column, indicating the real depth is in the extra depth column

	(($sum_depth =~ /^([\d ]*\d[\d ]*)/) ||
	($sum_depth =~ /^-[\d]/ && $sum_extra_depth =~ /^(-\d[\d ]*)/)) ||
	return 0;

	$origin_depth = $1 / 100;

	return 1;

}

sub Mb {
	local($mbid,$mb_auth);

	if($sum_magtyp =~ /[Bb]/) {
		# B PDE Mb

		$mb_auth = "PDE";
		$mbid = dbnextid(@db,"magid");
		return $sum_mag/10,$mbid,$mb_auth;

	} else {

		return -999.00,-1,"";

	}
}
 
sub Ms {
        local($msid,$ms_auth);
 
        if($sum_magtyp =~ /[Ss]/) {
		# S PDE Ms

		$ms_auth = "PDE";
                $msid = dbnextid(@db,"magid");
                return $sum_mag/10,$msid,$ms_auth;

        } else {

                return -999.00,-1,"";

        }
}

sub Ml {
	local($mlid,$ml_auth);

	if($sum_magtyp =~ /[Xx]/) {
		# X amplitude magnitude

		$ml_auth = $auth;
		$mlid = dbnextid(@db,"magid");
		return $sum_mag/10,$mlid,$ml_auth;

	} elsif($sum_magtyp =~ /[Cc]/) {
		# C Canadian Ml

		$ml_auth = "CANADA";
		$mlid = dbnextid(@db,"magid");
		return $sum_mag/10,$mlid,$ml_auth;

	} elsif($sum_magtyp =~ /[Pp]/) {
		# P Palmer Ml

		$ml_auth = "Palmer";
		$mlid = dbnextid(@db,"magid");
		return $sum_mag/10,$mlid,$ml_auth;

	} elsif($sum_xmag !~ /^\s*$/) {

		$ml_auth = $auth;
		$mlid = dbnextid(@db,"magid");
		return $sum_xmag/10,$mlid,$ml_auth;
		
	} else {

		return -999.00,-1,"";

	}
}

sub Md {
	local($mdid,$md_auth);

	if($sum_fmag !~ /^\s*$/) {

		$md_auth = $auth;
		$mdid = dbnextid(@db,"magid");
		return $sum_fmag/10,$mdid,$md_auth;

	} else {

		return -999.00,-1,"";
	
	}

}

sub set_magnitudes {

	($origin_mb,$origin_mbid,$mb_auth) = &Mb();
	($origin_ms,$origin_msid,$ms_auth) = &Ms();
	($origin_ml,$origin_mlid,$ml_auth) = &Ml();
	($md,$mdid,$md_auth) = &Md();

}

sub assoc_wgt {
	local($h_wgt) = @_;

	if($h_wgt eq "") {
		return -1.000;
	} else {
		return $h_wgt/4 <= 1 ? $h_wgt/4 : 1;
	}
}

sub convert_time {
	local($YrMoDyHrMn,$sec) = @_;
	local($epoch,$jdate);

	$YrMoDyHrMn =~ /^(\d\d)([\d ]\d)([\d ]\d)([\d ]\d)([\d ]\d)/;
	$YrMoDyHrMn =~ tr/ /0/;

	# Add the seconds on afterwards; $sec can be > 60 in a pickfile
	$epoch = str2epoch("$2/$3/$1 $4:$5");
	$epoch += $sec;
	$jdate = join("",strydtime($epoch) =~ /\d+\/\d+\/(\d+)\s.(\d+)/);

	return($epoch,$jdate);
}

sub num_ass_phases {
	local($n);
	$n = scalar(grep(substr($_,19,5) !~ /^\s+$/,@phase_rows));
	$n += scalar(grep(substr($_,31,5) !~ /^\s+$/,@phase_rows));
	return $n;
}

sub num_def_phases {
	local($n);
	if($sum_npha) {
		return $sum_npha;
	} else {
		$n = scalar(grep(substr($_,7,1) =~ /[0123]/,@phase_rows));
		$n += scalar(grep(substr($_,39,1) =~ /[0123]/,@phase_rows));
		return $n;
	}
}

sub num_stas_with_ml {
	local($n);

	$n = scalar(grep(substr($_,100,2) !~ /^\s+$/,@phase_rows));

	return $n;
}

sub num_stas_with_md {
	local($n);

	$n = scalar(grep(substr($_,102,2) !~ /^\s+$/,@phase_rows));

	return $n;
}

sub dtype {
	unless($sum_f !~ /^\s*$/) { return "-";}

	if($sum_f == 0 || $sum_f == 8) {
		return "f";
	} elsif($sum_f == 1 || $sum_f == 9) {
		return "g";
	} else {
		print STDERR "$ARGV: unexpected dtype translation problem ";
		print STDERR "(\$sumf = $sum_f)";
		print STDERR "--continuing\n";
		return "-";
	}
}
sub timedef {
	local($weight) = @_;

	unless($weight !~ /^\s*$/) { return "-";}

	if($weight < 4) {
		return "d";
	} elsif($weight <= 8) {
		return "d";
	} else {
		print STDERR "$ARGV: unexpected timedef translation problem ";
		print STDERR "(phase weight = $weight)";
		print STDERR "--continuing\n";
		return "-";
	}
}
 
sub fake_chan {
	local($sta) = @_;
	local($chan,$period,$gain,$component,$sta_2,$sta_3);

	$sta_2 = substr($sta,2,1);
	$sta_3 = substr($sta,3,1);

	if($sta_3 =~ /[pP]/) {
		$period = "p";
		# yuck
	} else {
		$period = "s";
	}

	$gain = "h";

	if($sta_2 eq "-" || $sta_3 eq "-") {
		$gain = "l";
	} else {
		$gain = "h";
	}

	if($sta_3 =~ /[eE]/) {
		$component = "e";
	} elsif($sta_3 =~ /[nN]/) {
		$component = "n";
	} else {
		$component = "z";
	}

	$chan = "$period$gain$component";

	return $chan;
}

sub netmag {
	local($evid,$orid,$auth) = @_;
	local($nsta);

	$nsta = &num_stas_with_ml();
	&add_netmag_entry($evid,$orid,$ml_auth,$nsta,
			  $origin_mlid,"ml",$origin_ml);

	$nsta = -1;
	&add_netmag_entry($evid,$orid,$ms_auth,$nsta,
			  $origin_msid,"ms",$origin_ms);

	$nsta = -1;
	&add_netmag_entry($evid,$orid,$mb_auth,$nsta,
			  $origin_mbid,"mb",$origin_mb);

	$nsta = &num_stas_with_md();
	&add_netmag_entry($evid,$orid,$md_auth,$nsta,
			  $mdid,"md",$md);

}

sub event {
	local($evid,$prefor,$auth) = @_;
	@db = dblookup(@db,"","event","","");
	dbaddv(@db,
		 "evid",$evid,
		 "evname",$ARGV =~ m@([^/]+)$@,
		 "prefor",$prefor,
#		 "commid",commid,
		 "auth",$auth);
	elog_flush( 1, 0 );
}

sub origin {
	local($auth) = @_;
	local($evid,$orid,$jdate);

	$evid = dbnextid(@db, "evid");
	$orid = dbnextid(@db, "orid");
	$jdate = join("",strydtime($origin_epoch) =~ /\d+\/\d+\/(\d+)\s.(\d+)/);

	@db = dblookup(@db,"","origin","","");
	eval {
	dbaddv(@db,
         	"lat",$origin_lat,
         	"lon",$origin_lon,
         	"depth",$origin_depth,
         	"time",$origin_epoch,
         	"orid",$orid,
         	"evid",$evid,
         	"jdate",$jdate,
         	"nass",&num_ass_phases,
         	"ndef",&num_def_phases,
#         	"ndp",ndp,
#         	"grn",grn,
#         	"srn",srn,
         	"etype",$sum_t ? $sum_t : "-",
#         	"depdp",depdp,
         	"dtype",&dtype(),
         	"mb",$origin_mb,
         	"mbid",$origin_mbid,
         	"ms",$origin_ms,
         	"msid",$origin_msid,
         	"ml",$origin_ml,
         	"mlid",$origin_mlid,
#         	"algorithm",algorithm,
#         	"commid",commid,
         	"auth",$auth);
	};
	elog_flush( 1, 0 );

	return $orid,$evid;

}

sub origerr {
	local($orid) = @_;
	local($stder1,$stder2,$stder);
	local($conf,$smajax,$sminax,$strike,$sdepth);

	if(grep(/\d/,($sum_azim1,$sum_dip1,$sum_stder1,$sum_azim2,$sum_dip2,$sum_stder2,$sum_stder)) == 7) {
		$stder1 = $sum_stder1 / 100;
		$stder2 = $sum_stder2 / 100;
		$stder = $sum_stder / 100;

		open(P,"$Project_Ellipse $sum_azim1 $sum_dip1 $stder1 $sum_azim2 $sum_dip2 $stder2 $stder |");
		$_ = <P>;
		s/^\s+//;
		($smajax,$sminax,$sdepth,$strike)=split(" ",$_);
		close(P);
		$conf = 0.683;
	} else {
		$smajax = -1.0000;
		$sminax = -1.0000;
		$sdepth = -1.0000;
		$strike = -1.00;
		$conf = 0;
	}	


	@db = dblookup(@db,"","origerr","","");
	dbaddv(@db,
         	"orid",$orid,
#         	"sxx",sxx,
#         	"syy",syy,
#         	"szz",szz,
#         	"stt",stt,
#         	"sxy",sxy,
#         	"sxz",sxz,
#         	"syz",syz,
#         	"stx",stx,
#         	"sty",sty,
#         	"stz",stz,
         	"smajax",$smajax,
         	"sminax",$sminax,
         	"strike",$strike,
         	"sdepth",$sdepth,
#         	"stime",stime,
         	"conf",$conf,
#         	"commid",commid,
         	"sdobs",$sum_rms ? $sum_rms/100 : -1.0000);
	elog_flush( 1, 0 );
}

sub P_phase {
	local($auth,$orid) = @_;
	local($arid,$sec,$epoch,$jdate,$fm,$qual);

	return unless($phs_Psec);

	$arid = dbnextid(@db,"arid");

	($epoch,$jdate) = &convert_time($phs_basetime,$phs_Psec / 100);

	if($phs_fm_corr =~ /[cCuU+]/) {
		$fm = "c.";
	} elsif($phs_fm_corr =~ /[dD-]/) {
		$fm = "d.";
	} else {
		$fm = "-";
	}

	if($phs_Pcode =~ /[eE]./) {
		$qual = "e";
	} elsif($phs_Pcode =~ /[iI]./) {
		$qual = "i";
	} else {
		$qual = "-";
	}

	if( $opt_c ) {
		$chan = &fake_chan($phs_sta);
	} elsif( $opt_p ) {
		if($phs_Pcode =~ /.[eE]/) {
			$chan = "E";
		} elsif($phs_Pcode =~ /.[nN]/) {
			$chan = "N";
		} elsif($phs_Pcode =~ /.[zZ]/) {
			$chan = "Z";
		} else {
			$chan = "-";
		}
	} else {
		$chan = "-";
	}

	@db = dblookup(@db,"","arrival","","");
	eval {
	dbaddv(@db,
         	"sta",$phs_sta,
         	"time",$epoch,
         	"arid",$arid,
         	"jdate",$jdate,
#         	"stassid",stassid,
#         	"chanid",chanid,
         	"chan",$chan,
         	"iphase","P",
#         	"stype",stype,
         	"deltim",$phs_Pse ? $phs_Pse / 100 : -1.000,
#         	"azimuth",azimuth,
#         	"delaz",delaz,
#         	"slow",slow,
#         	"delslo",delslo,
         	"ema",$phs_takeoff ? $phs_takeoff : -1.00,
#         	"rect",rect,
#         	"amp",amp,
#         	"per",per,
#         	"logat",logat,
#         	"clip",clip,
         	"fm",$fm,
#         	"snr",snr,
	  	"qual",$qual,
#         	"commid",commid,
         	"auth",$auth);
	};
	elog_flush( 1, 0 );
	return unless(!$@);

	@db = dblookup(@db,"","assoc","","");
	dbaddv(@db,
         	"arid",$arid,
         	"orid",$orid,
         	"sta",$phs_sta,
		"phase","P",
#         	"belief",belief,
         	"delta",$phs_dist ? $phs_dist / 1111.95 : -1.000,
#         	"seaz",seaz,
         	"esaz",$phs_azim ? $phs_azim : -999.00,
 	     	"timeres",$phs_Presid ? $phs_Presid / 100 : -999.000,
         	"timedef",&timedef($phs_Weight),
#         	"azres",azres,
#         	"azdef",azdef,
#         	"slores",slores,
#         	"slodef",slodef,
#         	"emares",emares,
#         	"vmodel",vmodel,
#         	"commid",commid,
         	"wgt",&assoc_wgt($phs_Weight));
	elog_flush( 1, 0 );
}


sub S_phase {
	local($auth,$orid) = @_;
	local($arid,$sec,$epoch,$jdate);

	return unless($phs_Ssec);

	$arid = dbnextid(@db,"arid");

	($epoch,$jdate) = &convert_time($phs_basetime,$phs_Ssec / 100);

	if( $opt_c ) {
		$chan = &fake_chan($phs_sta);
	} elsif( $opt_p ) {
		if($phs_Pcode =~ /.[eE]/) {
			$chan = "E";
		} elsif($phs_Pcode =~ /.[nN]/) {
			$chan = "N";
		} elsif($phs_Pcode =~ /.[zZ]/) {
			$chan = "Z";
		} else {
			$chan = "-";
		}
	} else {
		$chan = "-";
	}
	@db = dblookup(@db,"","arrival","","");
	eval {
	dbaddv(@db,
         	"sta",$phs_sta,
         	"time",$epoch,
         	"arid",$arid,
         	"jdate",$jdate,
#         	"stassid",stassid,
#         	"chanid",chanid,
         	"chan",$chan,
         	"iphase","S",
#         	"stype",stype,
         	"deltim",$phs_Sse ? $phs_Sse/100 : -1.000,
#         	"azimuth",azimuth,
#         	"delaz",delaz,
#         	"slow",slow,
#         	"delslo",delslo,
#         	"ema",ema,
#         	"rect",rect,
#         	"amp",amp,
#         	"per",per,
#         	"logat",logat,
#         	"clip",clip,
#         	"fm",fm,
#         	"snr",snr,
#	  	"qual",qual,
#         	"commid",commid,
         	"auth",$auth);
	};
	elog_flush( 1, 0 );
	return unless(!$@);

	@db = dblookup(@db,"","assoc","","");
	dbaddv(@db,
         	"arid",$arid,
         	"orid",$orid,
         	"sta",$phs_sta,
         	"phase","S",
#         	"belief",belief,
         	"delta",$phs_dist ? $phs_dist / 1111.95 : -1.000,
#         	"seaz",seaz,
         	"esaz",$phs_azim ? $phs_azim : -999.00,
        	"timeres",$phs_Sresid ? $phs_Sresid / 100 : -999.000,
         	"timedef",&timedef($phs_Sweight),
#         	"azres",azres,
#         	"azdef",azdef,
#         	"slores",slores,
#         	"slodef",slodef,
#         	"emares",emares,
#         	"vmodel",vmodel,
#         	"commid",commid,
         	"wgt",&assoc_wgt($phs_Sweight));
	elog_flush( 1, 0 );
}

sub amp_phase {
	local($auth,$orid,$evid) = @_;
	local($arid,$epoch,$jdate,$magid);

	return unless($phs_mxamp);

	$arid = dbnextid(@db,"arid");

	($epoch,$jdate) = &convert_time($phs_basetime,0);

	@db = dblookup(@db,"","arrival","","");
	eval {
	dbaddv(@db,
         	"sta",$phs_sta,
         	"time",$epoch,
         	"arid",$arid,
         	"jdate",$jdate,
#         	"stassid",stassid,
#         	"chanid",chanid,
         	"chan",$opt_c ? &fake_chan($phs_sta) : "-",
         	"iphase","amp",
#         	"stype",stype,
#         	"deltim",deltim,
#         	"azimuth",azimuth,
#         	"delaz",delaz,
#         	"slow",slow,
#         	"delslo",delslo,
#         	"ema",ema,
#         	"rect",rect,
         	"amp",$phs_mxamp / 2,
         	"per",$phs_per / 100,
#         	"logat",logat,
#         	"clip",clip,
#         	"fm",fm,
#         	"snr",snr,
#	  	"qual",qual,
#         	"commid",commid,
         	"auth",$auth);
	};
	elog_flush( 1, 0 );
	return unless(!$@);

	@db = dblookup(@db,"","assoc","","");
	dbaddv(@db,
         	"arid",$arid,
         	"orid",$orid,
         	"sta",$phs_sta,
         	"phase","amp",
#         	"belief",belief,
         	"delta",$phs_dist ? $phs_dist / 1111.95 : -1.000,
#         	"seaz",seaz,
         	"esaz",$phs_azim ? $phs_azim : -999.00,
#        	"timeres",timeres,
#         	"azres",azres,
#         	"azdef",azdef,
#         	"slores",slores,
#         	"slodef",slodef,
#         	"emares",emares,
#         	"vmodel",vmodel,
#         	"commid",commid,
#         	"wgt",wgt,
         	"timedef","n");
	elog_flush( 1, 0 );

	if($phs_xmg && ($origin_mlid != -1) && ($ml_auth eq $auth)) {

		$magid = $origin_mlid;

		@db = dblookup(@db,"","stamag","","");
		dbaddv(@db,
         		"magid",$magid,
         		"sta",$phs_sta,
         		"arid",$arid,
         		"orid",$orid,
         		"evid",$evid,
         		"phase","amp",
         		"magtype","ml",
         		"magnitude",$phs_xmg / 10,
#      		   	"uncertainty",uncertainty,
#	         	"commid",commid,
         		"auth",$auth);
		elog_flush( 1, 0 );
	}

	if($phs_fmg && ($mdid != -1)) {

		$magid = $mdid;

		@db = dblookup(@db,"","stamag","","");
		dbaddv(@db,
         		"magid",$magid,
         		"sta",$phs_sta,
         		"arid",$arid,
         		"orid",$orid,
         		"evid",$evid,
         		"phase","amp",
         		"magtype","md",
         		"magnitude",$phs_fmg / 10,
#      		   	"uncertainty",uncertainty,
#	         	"commid",commid,
         		"auth",$auth);
		elog_flush( 1, 0 );
	}

	return;
}

sub add_netmag_entry {
	local($evid,$orid,$auth,$nsta,$magid,$magtype,$mag) = @_;

	return unless($magid != -1);

	@db = dblookup(@db,"","netmag","","");

	dbaddv(@db,
         	"magid",$magid,
#         	"net",net,
         	"orid",$orid,
         	"evid",$evid,
         	"magtype",$magtype,
         	"nsta",$nsta,
         	"magnitude",$mag,
#         	"uncertainty",uncertainty,
#         	"commid",commid,
         	"auth",$auth);
	elog_flush( 1, 0 );

	return;

}

sub build_descriptor {
	local($abspath,$cwd,$dbpath,$extra_dbs);

	if (! -e $database) {
		open(DESCRIPTOR,">$database");
	
		print DESCRIPTOR "css3.0\n";
	
		open(A,"$ENV{\"ANTELOPE\"}/bin/abspath $database |");
		chop($abspath = <A>);
		close(A);
	
		$cwd = getcwd();
		if($abspath =~ m@/$cwd/([^/]+)@) {
			$dbpath = "./{$1}";
		} else {
			$dbpath = $abspath;
			$dbpath =~ s@([^/]+)$@{$1}@;
		}

		if($opt_r) {
			$extra_dbs = $opt_r;
			$extra_dbs =~ s@([^/]+):@{$1}:@g;
			$extra_dbs =~ s@([^/]+)$@{$1}@g;

			$dbpath = "$dbpath:$extra_dbs";
		}
	
		print DESCRIPTOR "$dbpath\n";
	
		close(DESCRIPTOR);
		
		return;
}

}

$Project_Ellipse = "project_ellipse";

$Usage = "Usage: $0 [-cv] [-r reference_db[:reference_db...]] pickfile [pickfile...] dbname\n";

$opt_v = $opt_c = $opt_r = $opt_p = 0; # Kill "variable used once" error
if ( ! getopts('cpvr:') || $#ARGV < 1 ) {
	die ( "$Usage" );
} else {
	$database = pop(@ARGV);
	&build_descriptor();
	@db = dbopen ( $database, "r+" ) ; 
	if($db[0] == -102) {
		die("Couldn't open database $database\n");
	}
}

while(<>) {
       if ($. == 1) {

		if($opt_v) {
			print STDERR "Processing file $ARGV\n";
		}

		&clear_globals();

		unless(&identify_summary_line($_)) {
			close(ARGV);
			print STDERR "No summary line in pickfile ";
			print STDERR "$ARGV--skipping\n";
			next;
		}

                &unpack_summary($_);

		unless(&summary_has_origin_keys()) {
			close(ARGV);
			print STDERR "Insufficient summary info in pickfile ";
			print STDERR "$ARGV--skipping\n";
			next;
		}

	} elsif (eof) {
 
		close(ARGV);

		if($user) {
			$auth = $user;
		} else {
			($auth) = $0 =~ m@([^/]+)$@;
		}

		# calling order is important:

		&set_magnitudes();
		($orid,$evid) = &origin($auth);
		&origerr($orid);
		&event($evid,$orid,$auth);
		&netmag($evid,$orid,$auth);

		while(@phase_rows) {
			$row = shift(@phase_rows);
			&unpack_phase($row);

			&P_phase($auth,$orid);

			&S_phase($auth,$orid);

			&amp_phase($auth,$orid,$evid);
		}
		
 
	} elsif (/^[Cc]\*[<\(]([A-Z ])[>\)]/) {

		$comment_type = $1;
		&process_comment($_,$comment_type);
 
	} elsif (/^[Cc]\*/) {

		$comment_type = "Freeform";
		&process_comment($_,$comment_type);

	} elsif (substr($_,0,4) =~ /[a-zA-Z]/) {

		push(@phase_rows,$_); 

	} else {
		
		print STDERR "$ARGV: row $. not understood: $_\n";

	}

}
