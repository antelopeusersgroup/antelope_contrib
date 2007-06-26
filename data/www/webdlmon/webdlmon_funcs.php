<?php
/*
$Id$
Notes:    1. To get the list of functions, open up 
          the default dlmon.pf and use eval() 
          to output the list of functions on a page. 
          Then cut and paste, and clean up to make PHP compatible.
          This is preferred over doing eval() on the fly
          due to major security risks (injection)
          The command below will print the list of functions:

          eval( "function print_proc_$key( \$value ) { $body }" ) ;
          2. No sorting is done in this file - it is done in the XSLT stylesheet
          that creates this, and then in the Javascript file that does it on the fly.
          3. Add connection status to all functions, even though only a few need it.
          Keeps the application file cleaner, without all if/else statements

*/


function print_proc_dlstate( $value, $con = NULL ) {
	if ($value != '-') {
		$txt = $value ;
		if ($value == "stopped") {
			$color = "#808080" ;
		/*
		# REMANENT FROM DLMON.PF
		} elseif( defined $pfarray_ref->{$dlname}{dlip} && $pfarray_ref->{$dlname}{dlip} == "-") {
			$color = "#ffff00" ;
		*/
		} elseif ($value == "yes") {
			$color = "#00ff00" ;
		} elseif ($value == "waiting") {
			$color = "#00ffff" ;
		} elseif ($value == "hibernating") {
			$color = "#ff00ff" ;
		} elseif ($value == "sleeping") {
			$color = "#ff0000" ;
		} elseif ($value == "reg") {
			$color = "#ffd000" ;
		} elseif ($value == "su") {
			$color = "#ffa000" ;
		} elseif ($value == "nr") {
			$color = "#ffa0a0" ;
		} else {
			$color = "#ff0000" ;
		}
	}
	return array( $txt, $color ) ;
}


function print_proc_comt( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value == "cdma poc") {
			$txt = "cpoc"  ;
			$color = "#d0ffd0" ;
		} elseif ($value == "regular internet") {
			$txt = "rint"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "DSL") {
			$txt = "dsl"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "cable") {
			$txt = "cble"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "no comms") {
			$txt = "none"  ;
			$color = "#ffa0a0"  ;
		} elseif ($value == "vsat") {
			$txt = "vsat" ;
			$color = "#d0d0ff" ;
		} else {
			$txt = $value ;
			$color = "#ffa0a0"  ;
		}
	} else {
		$txt = "" ;
		$color = "" ;
	} 
	return array( $txt, $color) ;
}

function print_proc_prov( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value == "verizon") {
			$txt = "VZ"  ;
			$color = "#d0ffd0" ;
		} elseif ($value == "cingular") {
			$txt = "CG"  ;
			$color = "#d0ffd0" ;
		} elseif ($value == "hosted") {
			$txt = "I"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "starstream") {
			$txt = "SS"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "QWEST/CCS") {
			$txt = "QW"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "Siskiyou tel") {
			$txt = "SK"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "Volcano Telco") {
			$txt = "VT"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "Cal_Ore tel") {
			$txt = "CO"  ;
			$color = "#d0ffff" ;
		} elseif ($value == "Telus") {
			$txt = "Tl"  ;
			$color = "#d0ffff"  ;
		} elseif ($value == "none") {
			$txt = "none"  ;
			$color = "#ffa0a0"  ;
		} elseif ($value == "spacenet") {
			$txt = "SN"  ;
			$color = "#d0d0ff" ;
		} elseif ($value == "Wild Blue") {
			$txt = "WB"  ;
			$color = "#d0d0ff" ;
		} elseif ($value == "Hughes") {
			$txt = "H"  ;
			$color = "#d0d0ff" ;
		} elseif ($value == "DSL Frontier") {
			$txt = "Fr"  ;
			$color = "#d0d0ff" ;
		} elseif ($value == "charter") {
			$txt = "Ch"  ;
			$color = "#d0d0ff" ;
		} else {
			$txt = $value ;
			$color = "#ffa0a0"  ;
		}
	} else {
		$txt = "" ;
		$color = "" ;
	}
	return array( $txt, $color) ;
}

function print_proc_STATUS_LATENCY ( $value, $con = NULL ) {
	if ($value != '-') {
		$color = "#d0d0ff" ;
		$aval = abs($value) ;
		$d  = (int)(($aval+0.001)/86400.0) ;
		$aval -= $d*86400.0 ;
		$h  = (int)(($aval+0.001)/3600.0) ;
		$aval -= $h*3600.0 ;
		$m  = (int)(($aval+0.001)/60.0) ;
		$aval -= $m*60.0 ;
		$s  = (int)($aval+0.001) ;

		if ($d > 0) {
			$txt = sprintf ( "%3dd%2.2dh%2.2dm%2.2ds", $d, $h, $m, $s ) ;
		} elseif ($h > 0) {
			$txt = sprintf ( "    %2.2dh%2.2dm%2.2ds", $h, $m, $s ) ;
		} elseif ($m > 0) {
			$txt = sprintf ( "       %2.2dm%2.2ds", $m, $s ) ;
		} else {
			$txt = sprintf ( "          %2.2ds", $s ) ;
		}

		if ($value >= 60.0) {
			$color = "#c0ffc0" ;
		}
		if ($value >= 3600.0) {
			$color = "#ffff00" ;
		}
		if ($value >= 7200.0) {
			$color = "#ff0000" ;
		}
	} else {
		$txt = "" ;
		$color = "" ;
	}
	return array( $txt, $color ) ;
}

function print_proc_gp24( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 86400.0) {
			$color = "#ff0000" ;
			$txt = sprintf ( "%.0fd", $value/86400.0 ) ;
		} elseif ($value >= 3600.0) {
			$color = "#ff0000" ;
			$txt = sprintf ( "%.0fh", $value/3600.0 ) ;
		} elseif ($value >= 60.0) {
			$color = "#ff0000" ;
			$txt = sprintf ( "%.0fm", $value/60.0 ) ;
		} elseif ($value > 0.0) {
			$color = "#ffff00" ;
			$txt = sprintf ( "%.0fs", $value ) ;
		} else {
			$color  = "#d0d0ff" ;
			$txt = "0s" ;
		}
	} else {
		$txt = "-" ;
		$color = "" ;
	}
	return array( $txt, $color) ;
}

function print_proc_gp1( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 86400.0) {
			$color  = "#ff0000" ;
			$txt = sprintf ( "%.0fd", $value/86400.0 ) ;
		} elseif ($value >= 3600.0) {
			$color  = "#ff0000" ;
			$txt = sprintf ( "%.0fh", $value/3600.0 ) ;
		} elseif ($value >= 60.0) {
			$color  = "#ff0000" ;
			$txt = sprintf ( "%.0fm", $value/60.0 ) ;
		} elseif ($value > 0.0) {
			$color  = "#ffff00" ;
			$txt = sprintf ( "%.0fs", $value ) ;
		} else {
			$color  = "#d0d0ff" ;
			$txt = "0s" ;
		}
	} else {
		$txt = "" ;
		$color = "" ;
	}
	return array( $txt, $color) ;
}

function print_proc_nr24( $value, $con = NULL ) {
	if ($value != '-') {
		$txt = sprintf ( "%.0f", $value ) ;
		if ($value > 0.0) {
			$color = "#ff0000" ;
		} else {
			$color = "#d0d0ff" ;
		}
	} else {
		$txt = "" ;
		$color = "" ;
	}
	return array( $txt, $color) ;
}

function print_proc_meme( $value, $con = NULL ) {
	if ($value != '-') {
		$txt = sprintf ( "%.0f", $value ) ;
		if ($value > 0.0) {
			$color = "#ff0000" ;
		} else {
			$color = "#d0d0ff" ;
		}
	} else {
		$txt = "" ;
		$color = "" ;
	}
	return array( $txt, $color) ;
}

function print_proc_cale( $value, $con = NULL ) {
	if ($value != '-') {
		if( strpos( $value, 'ch' ) !== false ) {
			$txt = $value ;
			$color  = "#ff0000" ;
		} else {
			$txt = $value ;
		}
	} else {
		$txt = "" ;
		$color = "" ;
	}
	return array( $txt, $color) ;
}

function print_proc_opt( $value, $con = NULL ) {
	if ($value != '-') {
		$spi = 0 ;
		$spo = 0 ;
		if ( strpos( $value, "isp1" ) !== false ) {
			$spi = 1;
		}
		if ( strpos( $value, "isp2" ) !== false ) {
			$spo = 1;
		}
		if ($spi == 1 && $spo == 0) {
			$color = "#00ff00" ;
			$txt = "I" ;
		}
		if ($spi == 1 && $spo == 1) {
			$color = "#ff0000" ;
			$txt = "On" ;
		}
	}
	return array( $txt, $color) ;
}

function print_proc_dlt( $value, $con = NULL ) {
	if ($value != '-') {
		$color = "#d0d0ff" ;
		$aval = abs($value) ;
		$d  = (int)(($aval+0.001)/86400.0) ;
		$aval -= $d*86400.0 ;
		$h  = (int)(($aval+0.001)/3600.0) ;
		$aval -= $h*3600.0 ;
		$m  = (int)(($aval+0.001)/60.0) ;
		$aval -= $m*60.0 ;
		$s  = (int)($aval+0.001) ;
		if ($d > 0) {
			$txt = sprintf ( "%3dd%2.2dh%2.2dm%2.2ds", $d, $h, $m, $s ) ;
		} elseif ($h > 0) {
			$txt = sprintf ( "    %2.2dh%2.2dm%2.2ds", $h, $m, $s ) ;
		} elseif ($m > 0) {
			$txt = sprintf ( "       %2.2dm%2.2ds", $m, $s ) ;
		} else {
			$txt = sprintf ( "          %2.2ds", $s ) ;
		}

		if ($con == 'waiting') {
			$color  = "#a0ffa0" ;
			if ($h >= 3) {
				$color  = "#ff0000" ;
			} elseif ($h >= 2) {
				$color  = "#ff8000" ;
			} elseif ($h >= 1) {
				$color  = "#ffff00" ;
			}
		} else {
			if ($value >= 60.0) { $color = "#c0ffc0" ;}
			if ($value >= 3600.0) { $color = "#ffff00" ;}
			if ($value >= 7200.0) { $color = "#ff0000" ;}
		}
	} else {
		$txt = "" ;
		$color = "" ;
	}
	return array( $txt, $color) ;
}

function print_proc_rtm( $value, $con = NULL ) {
	if ($value != '-') {
		$sort = $value ;
		$subset = $value ;
		$color = "#d0d0ff" ;
		$aval = abs($value) ;
		$d  = (int)(($aval+0.001)/86400.0) ;
		$aval -= $d*86400.0 ;
		$h  = (int)(($aval+0.001)/3600.0) ;
		$aval -= $h*3600.0 ;
		$m  = (int)(($aval+0.001)/60.0) ;
		$aval -= $m*60.0 ;
		$s  = (int)($aval+0.001) ;
		if ($d > 0) {
			$txt = sprintf ( "%3dd%2.2dh%2.2dm%2.2ds", $d, $h, $m, $s ) ;
		} elseif ($h > 0) {
			$txt = sprintf ( "    %2.2dh%2.2dm%2.2ds", $h, $m, $s ) ;
		} elseif ($m > 0) {
			$txt = sprintf ( "       %2.2dm%2.2ds", $m, $s ) ;
		} else {
			$txt = sprintf ( "          %2.2ds", $s ) ;
		}

		if ($value >= 0.0) {
			$color = "#d0d0ff" ;
		} else {
			if ($con == 'waiting') {
				$color  = "#a0ffa0" ;
				if ($h >= 3) {
					$color  = "#ff0000" ;
				} elseif ($h >= 2) {
					$color  = "#ff8000" ;
				} elseif ($h >= 1) {
					$color  = "#ffff00" ;
				}
			} elseif ($con == 'su' && $value > -3600.0) {
				$color  = "#ffa0a0" ;
			} elseif ($con == 'no' && $value > -3600.0) {
				$color  = "#ffa0a0" ;
			} elseif ($con == 'yes' && $value < 0.0) {
				$color  = "#ffa0a0" ;
			} else {
				$color  = "#ff0000" ;
			}
		}
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_tput( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value == 'inf') {
			$txt  = 'inf' ;
			$color  = "#ff0000" ;
		} else {
			if ($value <= 0.5) {
				$color = "#ffa0a0" ;
			} elseif ($value <= 0.8) {
				$color = "#ff80a0" ;
			} elseif ($value <= 0.9) {
				$color = "#ffffa0" ;
			} elseif ($value < 1.1) {
				$color = "#d0ffd0" ;
			} else {
				 $color = "#d0d0ff" ; 
			}
			$txt = sprintf ( "%.2f", $value ) ;
		}
		if ($con == 'waiting') {
			$color  = "#d0ffd0" ;
		}
		if ($con == 'su') {
			$color  = "#d0ffd0" ;
		}
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_ce( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value == 'inf') {
			$txt  = 'inf' ;
			$color  = "#ff0000" ;
		} else {
			$color = "#d0d0ff" ;
			if ($value < 90.0) { $color  = "#d0ffd0" ;}
			if ($value <= 50.0) { $color  = "#ffff00" ;}
			if ($value <= 10.0) { $color  = "#ff0000" ;}
			if ($value < 10.0) { 
				$txt = sprintf ( "%.1f%%", $value ) ;
			} else {
				$txt = sprintf ( "%.0f%%", $value ) ;
			}
			if ($con == 'waiting') {
				$color  = "#a0ffa0" ;
			} elseif ($con == 'su') {
				$color  = "#ffa0a0" ;
			}
		}
	} else {
		$sort = -999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_pbr( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value == 'inf') {
			$txt  = 'inf' ;
			$color  = "#ff0000" ;
		} else {
			$color = "#ff0000" ;
			if ($value < 50.0) { $color = "#ffff00" ; } 
			if ($value < 10.0) { $color = "#a0ffa0" ; } 
			if ($value <= 1.0) { $color = "#d0d0ff" ; }
			if ($value < 10.0) {
				$txt = sprintf ( "%.1f%%", $value ) ;
			} else {
				$txt = sprintf ( "%.0f%%", $value ) ;
			}
		}
	} else {
		$sort = -999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_nl24( $value, $con = NULL ) {
	if ($value != '-') {
		$txt = sprintf ( "%.0f", $value ) ;
		if ($value >= 500.0) {
			$color  = "#ff0000" ;
		} elseif ($value >= 200.0) {
			$color  = "#ffff00" ;
		} elseif ($value > 20.0) {
			$color  = "#d0ffd0" ;
		} else {
			$color  = "#d0d0ff" ;
		}
	} else {
		$sort = 9999999999.999 ;
		$subset = 9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_np24( $value, $con = NULL ) {
	if ($value != '-') {
		$txt = sprintf ( "%.0f", $value ) ;
		if ($value >= 500.0) {
			$color  = "#ff0000" ;
		} elseif ($value >= 200.0) {
			$color  = "#ffff00" ;
		} elseif ($value > 20.0) {
			$color  = "#d0ffd0" ;
		} else {
			$color  = "#d0d0ff" ;
		}
	} else {
		$sort = 9999999999.999 ;
		$subset = 9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_ni24( $value ) {
	if ($value != '-') {
		$txt = sprintf ( "%.0f", $value ) ;
		if ($value >= 10.0) {
			$color  = "#ff0000" ;
		} elseif ($value >= 5.0) {
			$color  = "#ff8000" ;
		} elseif ($value > 1.0) {
			$color  = "#ffff00" ;
		} else {
			$color  = "#d0d0ff" ;
		}
	} else {
		$sort = 9999999999.999 ;
		$subset = 9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_dr( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 10000000000.0) {
			$color = "#d0d0ff" ;
			$txt = sprintf ( "%.0fg", $value/1000000000.0 ) ;
		} elseif ($value >= 1000000000.0) {
			$color = "#d0d0ff" ;
			$txt = sprintf ( "%.1fg", $value/1000000000.0 ) ;
		} elseif ($value >= 10000000.0) {
			$color = "#d0d0ff" ;
			$txt = sprintf ( "%.0fm", $value/1000000.0 ) ;
		} elseif ($value >= 1000000.0) {
			$color = "#d0d0ff" ;
			$txt = sprintf ( "%.1fm", $value/1000000.0 ) ;
		} elseif ($value >= 10000.0) {
			$color = "#d0d0ff" ;
			$txt = sprintf ( "%.0fk", $value/1000.0 ) ;
		} elseif ($value >= 1000.0) {
			$color = "#a0ffa0" ;
			$txt = sprintf ( "%.1fk", $value/1000.0 ) ;
		} elseif ($value >= 500.0) {
			$color = "#a0ffa0" ;
			$txt = sprintf ( "%.0f", $value ) ;
		} elseif ($value > 0.0) {
			$color = "#ffffa0" ;
			$txt = sprintf ( "%.0f", $value ) ;
		} else {
			$color = "#ffa0a0" ;
			$txt = sprintf ( "%.0f", $value ) ;
		}
		if ($con == 'waiting') {
			$color  = "#d0d0ff" ;
		}
		if ($con == 'su') {
			$color  = "#d0d0ff" ;
		}
	} else {
		$sort = -999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_br24( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 10000000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fg", $value/1000000000.0 ) ;
		} elseif ($value < 10000000000.0  && $value >= 1000000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.1fg", $value/1000000000.0 ) ;
		} elseif ($value < 1000000000.0 && $value >= 10000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fm", $value/1000000.0 ) ;
		} elseif ($value < 10000000.0  && $value >= 1000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.1fm", $value/1000000.0 ) ;
		} elseif ($value < 1000000.0  && $value >= 10000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fk", $value/1000.0 ) ;
		} elseif ($value < 10000.0  && $value >= 1000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.1fk", $value/1000.0 ) ;
		} elseif ($value < 1000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fk", $value ) ;
		}
	} else {
		$sort = -999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_bw24( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 10000000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fg", $value/1000000000.0 ) ;
		} elseif ($value < 10000000000.0  && $value >= 1000000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.1fg", $value/1000000000.0 ) ;
		} elseif ($value < 1000000000.0 && $value >= 10000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fm", $value/1000000.0 ) ;
		} elseif ($value < 10000000.0  && $value >= 1000000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.1fm", $value/1000000.0 ) ;
		} elseif ($value < 1000000.0  && $value >= 10000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fk", $value/1000.0 ) ;
		} elseif ($value < 10000.0  && $value >= 1000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.1fk", $value/1000.0 ) ;
		} elseif ($value < 1000.0) {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%.0fk", $value ) ;
		}
	} else {
		$sort = -999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_clt( $value, $con = NULL ) {
	if ($value != '-') {
		$color = "#d0d0ff" ;
		$aval = abs($value) ;
		$d  = (int)(($aval+0.001)/86400.0) ;
		$aval -= $d*86400.0 ;
		$h  = (int)(($aval+0.001)/3600.0) ;
		$aval -= $h*3600.0 ;
		$m  = (int)(($aval+0.001)/60.0) ;
		$aval -= $m*60.0 ;
		$s  = (int)($aval+0.001) ;
		if ($d > 0) {
			$txt = sprintf ( "%3dd%2.2dh%2.2dm%2.2ds", $d, $h, $m, $s ) ;
		} elseif ($h > 0) {
			$txt = sprintf ( "    %2.2dh%2.2dm%2.2ds", $h, $m, $s ) ;
		} elseif ($m > 0) {
			$txt = sprintf ( "       %2.2dm%2.2ds", $m, $s ) ;
		} else {
			$txt = sprintf ( "          %2.2ds", $s ) ;
		}
		if ($value >= 3600.0) {
			$color = "#c0ffc0" ;
		}
		if ($value >= 21600.0) {
			$color = "#ffff00" ;
		}
		if ($value >= 86400.0) {
			$color = "#ff0000" ;
		}
	} else {
		$sort = 999999999.999 ;
		$subset = 9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_lcq( $value, $con = NULL ) {
	$c ;
	if ($value != '-') {
		if ($value == 'inf') {
			$txt  = 'inf' ;
			$color  = "#ff0000" ;
		} else {
			$color = "#ff0000" ;
			if ($value >= 10.0 && $value <= 60.0) {
				$c = (int)(63.0*($value-10.0)/50.0+0.5) ;
				$color  = sprintf ( "\#ff%2.2xd0", 192+$c ) ;
			} elseif ($value > 60.0 && $value < 90.0) {
				$color = "#f0ffd0" ;
				if ($value < 80.0) {
					$color  = "#ffffd0" ;
				}
			} elseif ($value >= 90.0) {
				if ($value < 95.0) {
					$c  = (int)(63.0*(95.0-$value)/5.0+0.5) ;
					$color = sprintf ( "\#d0ff%2.2x", 255-$c ) ;
				} elseif ($value >= 95.0 && $value < 100.0) {
					$c  = (int)(63.0*(100.0-$value)/5.0+0.5) ;
					$color = sprintf ( "\#d0%2.2xff", 192+$c ) ;
				} elseif ($value >= 100.0) {
					$color = "#d0d0ff" ;
				}
			}
			if ($value < 10.0) {
				$txt = sprintf ( "%.1f%%", $value ) ;
			} else {
				$txt = sprintf ( "%.0f%%", $value ) ;
			}
		}
	}
	return array( $txt, $color) ;
}

function print_proc_cld( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 10000.0 || $value <= -10000.0) {
			$txt = sprintf ( "%.2fs", 0.000001*$value ) ;
			$color = "#ff0000" ;
		} elseif ($value >= 1000.0 || $value <= -1000.0) {
			$txt = sprintf ( "%.1fms", 0.001*$value ) ;
			$color = "#d0ffd0" ;
		} else {
			$color  = "#d0d0ff" ;
			$txt = sprintf ( "%dus", $value ) ;
		}
		if ($value > 2000.0 || $value < -2000.0) {
			$color = "#ffffd0" ;
		}
		if ($value > 5000.0 || $value < -5000.0) {
			$color = "#ffd0d0" ;
		}
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_m0( $value, $con = NULL ) {
	if ($value != '-') {
		$value  = (int)($value) ;
		if ($value >= 50  || $value <= -50 ) {
			$color = "#ff0000" ;
		} elseif ($value >= 35 || $value <= -35) {
			$color = "#ffff00" ;
		} elseif ($value >= 20 || $value <= -20) {
			$color = "#a0ffa0" ;
		} else {
			$color = "#d0d0ff" ;
		}
		$txt = sprintf ( "%d", $value ) ;
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_m1( $value, $con = NULL ) {
	if ($value != '-') {
		$value  = (int)($value) ;
		if ($value >= 50  || $value <= -50 ) { $color = "#ff0000" ;}
		elseif ($value >= 35 || $value <= -35) { $color = "#ffff00" ;}
		elseif ($value >= 20 || $value <= -20) { $color = "#a0ffa0" ;}
		else { $color = "#d0d0ff" ;}
		$txt = sprintf ( "%d", $value ) ;
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_m2( $value, $con = NULL ) {
	if ($value != '-') {
		$value  = (int)($value) ;
		if ($value >= 50  || $value <= -50 ) { $color = "#ff0000" ;}
		elseif ($value >= 35 || $value <= -35) { $color = "#ffff00" ;}
		elseif ($value >= 20 || $value <= -20) { $color = "#a0ffa0" ;}
		else { $color = "#d0d0ff" ;}
		$txt = sprintf ( "%d", $value ) ;
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_dt( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 50.0) { $color  = "#ff0000" ;}
		elseif ($value >= 40.0) { $color  = "#ffff00" ;}
		elseif ($value >= 3.0) { $color  = "#a0ffa0" ;}
		elseif ($value >= -10.0) { $color  = "#d0d0ff" ;}
		elseif ($value >= -20.0) { $color  = "#0000ff" ;}
		else { $color  = "#ffd0ff" ;}
		$txt = sprintf ( "%.0fC", $value ) ;
	} else {
		$sort = 9999999999.999 ;
		$subset = 9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_dv( $value, $con = NULL ) {
	if ($value != '-') {
		if  ($value >= 12 || $value <= 14 )  {
			 $color = "#a0ffa0"; 
		} elseif ( ($value > 14 && $value <= 14.5) || ($value > 11.8 && $value < 12 ) ) {
			 $color = "#f0ffd0"; 
		} elseif ($value > 14.5 || $value <= 11.8 ) {
			 $color = "#ff8080"; 
		} else {
			$color = "#d0d0ff" ;
		}
		$txt = sprintf ( "%.1fV", $value ) ;
	} else {
		$sort = 9999999999.999 ;
		$subset = 9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_da( $value, $con = NULL ) {
	if ($value != '-') {
		if ($value >= 1.00) {
			$color  = "#ff0000" ;
		} elseif ($value >= 0.2) {
			$color  = "#a0ffa0" ;
		} else {
			$color = "#d0d0ff" ;
		}
		$txt = sprintf ( "%.0fmA", $value*1000.0 ) ;
	} else {
		$sort = 9999999999.999 ;
		$subset = 9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_gpss( $value, $con = NULL ) {
	if ($value == "-")	{
	} elseif ( $value == "on" )	{
		$color  = "#d0d0ff" ;
		$txt = $value ;
	} elseif ( $value == "ona" )	{
		$color  = "#d0d0ff" ;
		$txt = $value ;
	} elseif ( $value == "onc" )	{
		$color  = "#d0d0ff" ;
		$txt = $value ;
	} elseif ( $value == "off" )	{
		$color  = "#ffd0aa" ;
		$txt = $value ;
	} elseif ( $value == "offg" )	{
		$color  = "#d0ffd0" ;
		$txt = $value ;
	} elseif ( $value == "offp" )	{
		$color  = "#d0ffd0" ;
		$txt = $value ;
	} elseif ( $value == "offt" )	{
		$color  = "#ffd0aa" ;
		$txt = $value ;
	} elseif ( $value == "offc" )	{
		$color  = "#ffd0aa" ;
		$txt = $value ;
	} elseif ( $value == "cs" )	{
		$color  = "#ff0000" ;
		$txt = $value ;
	} else{
		$color  = "#ffd0d0" ;
		$txt = $value ;
	}
	return array( $txt, $color) ;
}

function print_proc_gps( $value, $con = NULL ) {
	$fr = 0 ;
	$el = 0 ;
	$d = 0 ;
	if ( strpos( $value, "elck" ) !== false ) {
		$el = 1;
	}
	if ( strpos( $value, "fr" ) !== false ) {
		$fr = 1;
	}
	if ( strpos( $value, "1d" ) !== false ) {
		$d = 1;
	}
	if ( strpos( $value, "2d" ) !== false ) {
		$d = 2;
	}
	if ( strpos( $value, "3d" ) !== false ) {
		$d = 3;
	}
	if ($fr != 0 || $el != 0 || $d != 0) {
		if ($d == 0)	{
			$value  = "" ;
			if ($el != 0) {
				$value .= 'l' ;
			}
			if ($fr != 0) {
				$value .= 'f' ;
			}
		} elseif ($d == 1)	{
			$value  = "1D" ;
			if ($fr != 0) {
				$value .= 'f' ;
			}
		} elseif ($d == 2)	{
			$value  = "2D" ;
			if ($fr != 0) {
				$value .= 'f' ;
			}
		} elseif ($d == 3)	{
			$value  = "3D" ;
			if ($fr != 0) {
				$value .= 'f' ;
			}
		}
	}
	if ($value == "-")	{
	} elseif ($value == "L")	{
		$color = "#d0d0ff" ;
		$txt = "L" ;
	} elseif ($value == "3D")	{
		$color = "#d0d0ff" ;
		$txt = "3D" ;
	} elseif ($value == "3Df")	{
		$color = "#aaaaff" ;
		$txt = "3Df" ;
	} elseif ($value == "2D")	{
		$color = "#d0ffd0" ;
		$txt = "2D" ;
	} elseif ($value == "2Df")	{
		$color = "#aaffaa" ;
		$txt = "2Df" ;
	} elseif ($value == "1D")	{
		$color = "#ffffD0" ;
		$txt = "1D" ;
	} elseif ($value == "1Df")	{
		$color = "#ffffaa" ;
		$txt = "1Df" ;
	} elseif ($value == "lf")	{
		$color = "#d0ffd0" ;
		$txt = "lf" ;
	} elseif ($value == "off")	{
		$color = "#ffd0aa" ;
		$txt = $value ;
	} elseif ($value == "U")	{
		$color = "#ffd0d0" ;
		$txt = $value ;
	} elseif ($value == "u")	{
		$color = "#ffd0d0" ;
		$txt = $value ;
	} elseif ($value == "nb")	{
		$color = "#ffd0d0" ;
		$txt = $value ;
	} elseif ($value == "uf")	{
		$color = "#ffaaaa" ;
		$txt = $value ;
	} else {
		$color = "#ff0000" ;
		$txt = $value ;
	}
	return array( $txt, $color) ;
}

function print_proc_clq( $value, $con = NULL ) {
	if ($value == "-") {
	} elseif ($value == "5")	{
		$color = "#d0d0ff" ;
		$txt = "L" ;
	} elseif ($value == "l")	{
		$color = "#d0d0ff" ;
		$txt = "L" ;
	} elseif ($value == "ex")	{
		$color = "#d0d0ff" ;
		$txt = "EX" ;
	} elseif ($value == "g")	{
		$color = "#d0d0ff" ;
		$txt = "IG" ;
	} elseif ($value == "4")	{
		$color = "#d0ffd0" ;
		$txt = "T" ;
	} elseif ($value == "t")	{
		$color = "#d0ffd0" ;
		$txt = "T" ;
	} elseif ($value == "k")	{
		$color = "#d0ffd0" ;
		$txt = "K" ;
	} elseif ($value == "3")	{
		$color = "#f0ffd0" ;
		$txt = "H" ;
	} elseif ($value == "h")	{
		$color = "#f0ffd0" ;
		$txt = "H" ;
	} elseif ($value == "2")	{
		$color = "#ff8000" ;
		$txt = "2" ;
	} elseif ($value == "1")	{
		$color = "#ff8000" ;
		$txt = "1" ;
	} elseif ($value == "0")	{
		$color = "#ff8000" ;
		$txt = "0" ;
	} elseif ($value == "cs")	{
		$color = "#ff0000" ;
		$txt = "IC" ;
	} else{
		$color = "#ff0000" ;
		$txt = "IC" ;
	}
	return array( $txt, $color) ;
}

function print_proc_lat( $value, $con = NULL ) {
	if ($value != '-') {
		$color = "#d0d0ff" ;
		$txt = sprintf ( "%.3f", $value ) ;
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_lon( $value, $con = NULL ) {
	if ($value != '-') {
		$color = "#d0d0ff" ;
		$txt = sprintf ( "%.3f", $value ) ;
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}

function print_proc_elev( $value, $con = NULL ) {
	if ($value != '-') {
		$color = "#d0d0ff" ;
		$txt = sprintf ( "%.0fm", $value*1000.0 ) ;
	} else {
		$sort = -9999999999.999 ;
		$subset = -9999999999.999 ;
	}
	return array( $txt, $color) ;
}
