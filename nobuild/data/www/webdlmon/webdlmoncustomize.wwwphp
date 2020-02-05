<?php
/*
$Id$
*/

function get_relative_path( $path ) {
	$npath = str_replace( '\\', '/', $path ) ; /* For Windows boxes */
	return str_replace( $_SERVER['DOCUMENT_ROOT'], '', $npath ) ;
}

if( ! extension_loaded( "Datascope" ) ) {
	die( '<b><u>Webdlmon Error:</u></b> 
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
$dlmon_pf = $sources['dlmon_pf'] ;

$optional = pfget( $webdlmon_pf, 'optional' ) ;
$table_caption = trim( $optional['table_caption'], "'\"" ) ;

$dataloggers = pfget( $webdlmon_pf, 'dataloggers' ) ;
$dlmodel = trim( $dataloggers['model'], "'\"" ) ;

$customization = pfget( $webdlmon_pf, 'customization' ) ;

if( $customization['customize'] == 'true' ) {
	$cookie_name = $customization['cookie_name'] ;
	$cookie_domain = get_relative_path( dirname( __FILE__ ) ) ;

	if( !empty( $_POST['submit'] )  && !empty( $_POST['cookie'] ) ) {
		ksort( $_POST['cookie'] ) ;
		$cookie_val = implode( '|', $_POST['cookie'] ) ;
		$cookie_expire = time() +  ( 60*60*24*365 ) ; // One year from now
		setcookie( $cookie_name, $cookie_val, $cookie_expire, $cookie_domain ) ;
	} else {
		// Make sure we delete the old cookie, browser specific
		// See http://www.php.net/setcookie
		if( eregi( 'MSIE', $_SERVER['HTTP_USER_AGENT'] ) ) { // MS IE issue
			setcookie( $cookie_name, "", time()+3600, $cookie_domain ) ;
		} elseif( eregi( 'Firefox', $_SERVER['HTTP_USER_AGENT'] ) ) {
			setcookie( $cookie_name, $COOKIE[$cookie_name], time()-3600, $cookie_domain ) ;
		} else {
			setcookie( $cookie_name, "", time()-3600, $cookie_domain ) ;
		}
	}

	if( !empty( $_COOKIE[$cookie_name] ) ) {
		$preset_fields = explode( '|', $_COOKIE[$cookie_name] ) ;
	}
}

$station_status_list = pfget( $dlmon_pf, 'station_status_lists' ) ;
$station_status_defs = pfget( $dlmon_pf, 'station_status_defs' ) ;

$params = $station_status_list[$dlmodel]['single'] ;

?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title><?= $table_caption ?>: Customize webdlmon</title>
<style type="text/css" media="all">@import "webdlmon.css";</style>
<script language="javascript" type="text/javascript" src="webdlmon.js"></script>
<script language="javascript "type="text/javascript">
<?php
if( !empty( $preset_fields ) ) {
	for( $i=0; $i<count($preset_fields); $i++ ) {
		echo "myCookie[$i] = '" . $preset_fields[$i] . "' ;\n" ;
	}
}
?>
</script>
</head>

<body>
	<div id="webdlmoncustomize">
		<h1><?= $table_caption ?>: Customize your view</h1>
		<?php
		if( !empty( $_POST['submit'] )  && !empty( $_POST['cookie'] ) ) {
			?>
			<div id="status">
				<p><strong>We have recorded your preferences. Thank you.</strong></p>
			</div>
			<div id="footer">
					<input type="button" 
						name="close" id="close" 
						title="Close window"
						value="Close window" 
						onclick="closePopup()" />
			</div>
			<?php
		} elseif( !empty( $_POST['submit'] )  && empty( $_POST['cookie'] ) ) {
			?>
			<div id="status">
				<p><strong>You did not specify any fields &ndash; the default view (all fields) will be applied. Thank you.</strong></p>
			</div>
			<div id="footer">
					<input type="button" 
						name="close" id="close" 
						title="Close window"
						value="Close window" 
						onclick="closePopup()" />
			</div>
			<?php
		} else {
			?>
			<div id="status">
				<p>Check the boxes of all the fields (columns) you wish to see in your personalized version of webdlmon.
				To learn what a particular field is, place the cursor over the hyperlink and an information box
				will appear. If some fields are already checked, your browser has stored your pre-existing preferences.
				The number next to the field in parentheses refers to the display order in the webdlmon table.</p>
				<p>To revert to the default view &ndash; all fields visible &ndash; make sure no fields
				are selected and click the customize button.</p>
				<form name="webdlmonCookie" 
					id="webdlmonCookie" 
					method="post"
					action="<?= $_SERVER['PHP_SELF'] ?>" >
					<table>
					<tr>
					<?php
					$i = 1 ;
					foreach( $params as $p ) {
						$title = $station_status_defs[$p]['title'] ;
						$desc = explode( "\n", $station_status_defs[$p]['description'] ) ;
						if( !empty( $preset_fields ) ) {
							$checked = in_array( $p, $preset_fields ) ? "checked" : "" ;
						} else {
							$checked = "" ;
						}
						?>
						<td>
							<input type="checkbox" 
								name="cookie[]" 
								value="<?= $p ?>" 
								id="<?= $p ?>" 
								onclick="setCookieOrder(this.value);"
								<?= $checked ?> />
							<label for="<?= $title ?>">
								<a href="#" title="<?= $desc[0] ?>">
									<strong><?= $title ?></strong>
								</a>
								<span id="co_<?= $p ?>"></span>
							</label>
						</td>
						<?php
						if( $i % 8 == 0 ) {
							echo "</tr>\n<tr>\n" ;
						}
						$i++ ;
					}
					?>
					</tr>
					</table>
					<p><strong>Important Note: Your browser MUST be set to accept cookies.</strong></p>
			</div>
			<div id="footer">
					<input type="button" 
						name="reset" id="reset" 
						title="Clear selected fields"
						value="Clear selected fields" 
						onclick="clearFields()" />
					<input type="submit" 
						name="submit" id="submit" 
						title="Click to customize webdlmon"
						value="Customize webdlmon" />
			</div>
			</form>
			<?php
		}
	?>
	</body>
</html>

