<?php
/*
$Id$
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

function processDlsiteStash( $stasharr ) {
	$fname = "/tmp/stash" . getmypid() . ".dlsite" ;
	$fp = fopen( $fname, 'w+' ) ;
	fprintf( $fp, "%s", $stasharr ) ;
	fclose( $fp ) ;
	$db = ds_dbopen_table( $fname, 'r+' ) ;
	$n = dbquery( $db, 'dbRECORD_COUNT' ) ;
	for( $db[3]=0; $db[3]<$n; $db[3]++ ) {
		list( $dlname, $comt, $prov, $lat, $lon ) = dbgetv( $db, "dlname", "commtype", "provider", "lat", "lon" ) ;
		$dataloggers_stash[$dlname]['comt'] = $comt ;
		$dataloggers_stash[$dlname]['prov'] = $prov ;
		$dataloggers_stash[$dlname]['lat'] = $lat ;
		$dataloggers_stash[$dlname]['lon'] = $lon ;
		$dataloggers_stash[$dlname]['latdl'] = $lat ;
		$dataloggers_stash[$dlname]['londl'] = $lon ;
	}

	dbdestroy( $db ) ;
	return $dataloggers_stash ;
}

function elapsed_time( $time ) { /* Local function for this application */
	global $xml_file ;
	$my_time = $time - filemtime( $xml_file ) ;

	$aval = abs( $my_time ) ;
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

	 return $txt ;
}

if( !extension_loaded( 'Datascope' ) ) {
	dl( 'Datascope.so' ) or die( '<b><u>Webdlmon Error:</u></b> 
		Cannot load Datascope.so extension. Goodbye.' ) ;
}

$webdlmon_pf = 'webdlmon.pf' ;

pfupdate( $webdlmon_pf ) ;

$pf_change_time = "1182297600" ;

if( pfrequire( $webdlmon_pf, $pf_change_time ) < 0 ) {
	die( "<b><u>Webdlmon Error:</u></b>
		The parameter file $webdlmon_pf.pf is out of date. 
		Please upgrade to the latest version. Goodbye." ) ;
}

$sources = pfget( $webdlmon_pf, 'sources' ) ;
$dlmon_pf = trim( $sources['dlmon_pf'], "'\"" );
$xml_file = trim( $sources['xml_src'], "'\"" ) ;
$xml_stash = trim( $sources['xml_stash'], "'\"" ) ;

$dataloggers = pfget( $webdlmon_pf, 'dataloggers' ) ;
$dlmodel = trim( $dataloggers['model'], "'\"" ) ;
$dl_both_sides = trim( $dataloggers['both_sides'], "'\"" ) ;

$customization = pfget( $webdlmon_pf, 'customization' ) ;
$customize = $customization['customize'] ;
$customize_text = trim( $customization['customize_text'], "'\"" ) ;

$optional = pfget( $webdlmon_pf, 'optional' ) ;
$table_caption = trim( $optional['table_caption'], "'\"" ) ;
$show_legend = $optional['show_legend'] ;

$rrdtool_setup = pfget( $webdlmon_pf, 'rrdtool_setup' ) ;

if( $customize == 'true' ) {
	$cookie_name = $customization['cookie_name'] ;
}

if( $rrdtool_setup['rrdtool'] == "true" ) {
	$inclusion_array = $rrdtool_setup['rrdtool_graph_inclusion'] ;
	$inclusion_url = $rrdtool_setup['rrdtool_graph_url'] ;
}

$station_status_list = pfget( $dlmon_pf, 'station_status_lists' ) ;
$station_status_defs = pfget( $dlmon_pf, 'station_status_defs' ) ;

if( !array_key_exists( $dlmodel, $station_status_list ) ) {
	die( "<b><u>Webdlmon Error:</u></b> Your webdlmon.pf must have the datalogger model that you 
		defined in webdlmon_config.pf (<b>$dlmodel</b>) defined. Goodbye." ) ;
}

$params = $station_status_list[$dlmodel]['single'] ;
$dl_desc = explode( '\n', $station_status_defs['dlname']['description'] ) ;

$doc = new DOMDocument() ;

if( !method_exists( $doc, 'getElementsByTagName' ) ) {
	die( '<b><u>Webdlmon Error:</u></b> You must compile PHP with the 
		<a href="http://www.php.net/dom">DOMDocument class</a>. Goodbye.' ) ;
}

if( !file_exists( $xml_file ) ) {
	die( '<b><u>Webdlmon Error:</u></b> You must have a valid XML file to parse. Goodbye.' ) ;
}

$doc->preserveWhiteSpace = FALSE ;
$doc->load( $xml_file ) ;
$dataloggers = $doc->getElementsByTagName('datalogger') ;

$stash = new DOMDocument() ;
$stash->preserveWhiteSpace = TRUE ;
$stash->load( $xml_stash ) ;

$stash_pfstring = $stash->getElementsByTagName('pfstring') ;

$dlsite = "" ;
for( $i = 0; $i < $stash_pfstring->length; $i++ ) {
	if( $stash_pfstring->item($i)->getAttribute( 'name' ) == 'dlsite' ) {
    	 $dlsite .= $stash_pfstring->item($i)->nodeValue . "\n" ;
	}
}
$dlarray = processDlsiteStash( $dlsite ) ;

if( !empty( $_COOKIE[$cookie_name] ) ) {
	$preset_fields = explode( '|', $_COOKIE[$cookie_name] ) ;
}

if( !empty( $preset_fields ) ) {
	echo "<script language=\"javascript\" type=\"text/javascript\">\n" ;
	for( $i=0; $i<count($preset_fields); $i++ ) {
		echo "myCookie[$i] = '" . $preset_fields[$i] . "' ;\n" ;
	}
	echo "</script>\n" ;
}


?>

<div id="webdlmon">
	<?php
	if( strtolower( $customize ) == 'true' ) {
		?>
		<div id="customize">
			<?php
			if( !empty( $preset_fields ) ) {
				echo "<p>Your customized webdlmon view contains the following fields::</p>\n" ;
				echo "<p class=\"preset\">" ;
				$cols = 1 ;
				foreach( $preset_fields as $p_f ) {
					$title = $station_status_defs[$p_f]['title'] ;
					echo "$title&nbsp;&nbsp;" ;
					if( $cols % 10 == 0 ) echo "<br/>" ;
					$cols++ ;
				}
				echo "</strong></p>\n" ;
			}
			?>
			<p><a title="Customize webdlmon" href="webdlmoncustomize.php" target="_blank"><?= $customize_text ?></a></p>
		</div>
		<?php
	}
	?>

	<a name="top"></a>
	<p>Snapshot of dlmon. Sortable by column heading, arrow direction
	signifying forward or reverse sort. 
	<?php if( $show_legend == 'true' ) echo '<a href="#legend">View the legend for the table</a>.' ?></p>
	<p>Click a value to observe the trends for this parameter over the last day.</p>
	<p>Table is initially sorted based on <strong>data latency (dltncy column)</strong>.
	The data in this table is <strong><?= elapsed_time( time() ) ?></strong> old. Data is regenerated every 5m.
	<strong>Refresh the page to see the latest data</strong>.</p>

	<table class="sortable">
		<caption><?= $table_caption ?></caption>
		<thead>
			<tr>
				<th id="dlname" title="<?= $dl_desc[0] ?>">dlname</th>
				<?php
				if( !empty( $preset_fields ) ) {
					unset( $params ) ;
					$params = $preset_fields ;
				}
				foreach( $params as $p ) {
					$title = $station_status_defs[$p]['title'] ;
					$desc = explode( "\n", $station_status_defs[$p]['description'] ) ;
	
					if( !empty( $preset_fields ) ) {
						/* Cookie present */
						if( in_array( $p, $preset_fields ) ) {
							echo "<th id=\"$title\" title=\"" . $desc[0] . "\">$title</th>\n" ;
						}
					} else {
						echo "<th id=\"$title\" title=\"" . $desc[0] . "\">$title</th>\n" ;
					}
				}
				if( strtolower( $dl_both_sides ) == 'true' ) {
					?>
					<th id="dlname" title="<?= $dl_desc[0] ?>">dlname</th>
					<?php
				}
				?>
			</tr>
		</thead>
		<tbody>
		<?php
			foreach( $dataloggers as $dls ) {
				$dlname = $dls->getAttribute('name') ;
				echo "<tr>\n" ;
	
				/* dlname always separate */
				$con_key = $dls->getElementsByTagName( 'con' ) ;
				$con_val = $con_key->item(0)->nodeValue ;
				$dlstate_proc = "print_proc_dlstate" ; /* connection status */
				list( $dltxt, $dlcolor ) = $dlstate_proc( $con_val ) ;
				echo "<td id=\"$dlname\" style=\"background-color:$dlcolor\"><a id=\"$dlname\" title=\"View station location for $dlname\">" . $dlname . "</a></td>\n" ;
	
				if( !empty( $preset_fields ) ) {
					unset( $params ) ;
					$params = $preset_fields ;
				}
	
				foreach( $params as $p ) {
					if( $p == 'comt' || $p == 'prov' || $p == 'lat' || $p == 'lon' ) {
						$my_val = $dlarray[$dlname][$p] ;
					} else {
						$my_key = $dls->getElementsByTagName( $p ) ;
						$my_val = $my_key->item(0)->nodeValue ;
					}
	
					$print_proc = "print_proc_$p" ;
					list( $txt, $color ) = $print_proc( $my_val, $con_val ) ;
	
					$customkey = " sorttable_customkey=\"$my_val\" " ;

					if( isset( $inclusion_array ) && in_array( $p, $inclusion_array ) ) {
						echo "<td style=\"background-color:$color\"$customkey><a href=\"$inclusion_url?sta=$dlname&period=day&var=$p\" class=\"lbOn\">" . $txt . "</a></td>\n" ;
					} else {
						echo "<td style=\"background-color:$color\"$customkey>" . $txt . "</td>\n" ;
					}

					unset( $p, $txt, $color ) ;
				}

				if( strtolower( $dl_both_sides ) == 'true' ) {
					echo "<td id=\"$dlname\" style=\"background-color:$dlcolor\"><a id=\"$dlname\" title=\"View station location for $dlname\">" . $dlname . "</a></td>\n" ;
				}
			echo "</tr>\n" ;
			unset( $dlname ) ;
		}
		?>
		</tbody>
	</table>

	<?php
	if( strtolower( $show_legend ) == 'true' ) {
		echo "<div id=\"legend\">\n" ;
		echo "<h3><a name=\"legend\"></a>Legend</h3>\n" ;
		foreach( $params as $p ) {
			$title = $station_status_defs[$p]['title'] ;
			$desc = nl2br( $station_status_defs[$p]['description'] ) ;
			echo "<fieldset>\n" ;
			echo "\t<legend>$title</legend>\n" ;
			echo "\t<p>$desc</p>\n" ;
			echo "</fieldset>\n" ;
			echo "<p align=\"right\"><a href=\"#top\">back to top&nbsp;&nbsp;&uarr;</a></p>\n" ;
		}
		echo "</div>\n" ;
	}
	?>
</div>
