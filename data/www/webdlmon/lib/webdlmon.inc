<?php
/*
$Id$
*/

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

	<a name="top" />
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
