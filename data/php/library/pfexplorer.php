<?php
#
# Copyright (c) 2005 The Regents of the University of California
# All Rights Reserved
#
# Permission to use, copy, modify and distribute any part of this software for
# educational, research and non-profit purposes, without fee, and without a
# written agreement is hereby granted, provided that the above copyright
# notice, this paragraph and the following three paragraphs appear in all
# copies.
#
# Those desiring to incorporate this software into commercial products or use
# for commercial purposes should contact the Technology Transfer Office,
# University of California, San Diego, 9500 Gilman Drive, La Jolla, CA
# 92093-0910, Ph: (858) 534-5815.
#
# IN NO EVENT SHALL THE UNIVESITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
# LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE, EVEN IF THE UNIVERSITY
# OF CALIFORNIA HAS BEEN ADIVSED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE SOFTWARE PROVIDED HEREIN IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
# CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
# ENHANCEMENTS, OR MODIFICATIONS.  THE UNIVERSITY OF CALIFORNIA MAKES NO
# REPRESENTATIONS AND EXTENDS NO WARRANTIES OF ANY KIND, EITHER IMPLIED OR
# EXPRESS, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, OR THAT THE USE OF THE
# SOFTWARE WILL NOT INFRINGE ANY PATENT, TRADEMARK OR OTHER RIGHTS.
#
#   This code was created as part of the ANF project.
#   See http://anf.ucsd.edu/
#
#   Prototype parameter file explorer
#   Written By: Rob Newman 2005-03-02
#
# Note: This code requires that Datascope.so is installed
# in the Antelope php area, that Javascript is turned on,
# and the client is using a DOM 1.0 compliant browser
# ( http://www.w3.org/ ).

dl( 'Datascope.so' ) ; # Dynamic load Datascope shared object
$ANT = "/opt/antelope/4.6p/data/pf/" ; # Path to antelope
$self = $_SERVER['PHP_SELF'] ; # Shorthand reference to self
?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" 
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>Paramter File Explorer - PROTOTYPE</title>
<script type="text/javascript">
<!--
// Function to show and hide div layers
// and change plus/minus signs
function show( which, num ) {
   // START: CSS STYLES - must match those defined in the stylesheet
   var buttonsBkgrd = "#ff9" ; // .button.background-color
   var buttonsBorder = "none" ; // .button.background-color
   var buttonsTexDec = "none" ; // .button.background-color
   // END: CSS STYLES

   var main = document.getElementById("main");

   var divs = main.getElementsByTagName("div");
   for( i=0; i<divs.length; i++ ) {
      if( divs[i].className=="information" ) {
         divs[i].style.visibility="hidden";
      }
   }
   document.getElementById( which ).style.visibility="visible";

   var tree = document.getElementById( "tree" );
   var listItem = tree.getElementsByTagName("li");
   for( j=0; j<listItem.length; j++ ) {
      listItem[j].style.backgroundColor=buttonsBkgrd;
      listItem[j].style.border=buttonsBorder;
      listItem[j].style.textDecoration=buttonsTexDec;
      var highlighttext="-";
      var normaltext="+";
      var status = listItem[j].getElementsByTagName("a").item(0).firstChild;
      status.nodeValue=status.nodeValue.replace( highlighttext,normaltext);
   }
   var myStatus = listItem.item( num ).getElementsByTagName("a").item(0).firstChild;
   myStatus.nodeValue = myStatus.nodeValue.replace( normaltext,highlighttext );
   listItem.item( num ).style.backgroundColor="white";
   listItem.item( num ).style.border="2px solid black";
}
// -->
</script>
<style type="text/css" media="all">
/* Edit the positions of the positionable elements at your own peril! */
/* DO NOT CHANGE DIV OR CLASS NAMES - THEY ARE REFERENCED IN THE JAVASCRIPT ABOVE */
   body {
      margin:10px 10px 0px 10px;
      padding:0px;
      font-family: "Trebuchet MS", Helvetica, sans-serif ;
      font-size: 12px ;
   }
   #header {
      height: 100px ;
      padding: 3px ;
   }
   #header h1 {
      font-size: 14px ;
   }
   #header h2 {
      font-size: 13px ;
   }
   #main {
      top: 150px ;
      width: 100% ;
      padding: 3px ;
   }
   .button {
      position: absolute;
      left: 10px ;
      top: 150px ;
      width: 20% ;
      border: 2px solid black;
      cursor: pointer ;
      padding: 2px ;
      background-color: #ff9;
      display:inline;
   }
   .information {
      visibility: hidden ;
      position: absolute ;
      left: 22%;
      top: 150px ;
      width: 50% ;
      float: left;
      border: 2px solid black;
      padding: 3px ;
      background-color: #ff9;
   }
   .information h3 {
      border: 2px solid black;
      color: #fff;
      background-color: #900;
      padding: 3px ;
   }
   .highlight {
      color: blue ;
      text-decoration: underline ;
   }
   ul {
      margin-left: 0;
      padding-left: 1em;
   }
   li {
      font-weight: bold ;
      list-style: none ;
      margin: 0;
      padding: 0;
   }
   .grandchild {
      padding: 1.0em ;
      margin: 1.0em ;
      border: 2px solid black ;
      background-color: white ;
   }
   .grandchildkey {
      font-size: 1.0em ;
      color: blue ;
   }
</style>
</head>
<body>
	<div id="header">
		<h1>Parameter File Explorer<a href="#" title="This requires a W3C DOM 1.0 compliant browser and Javascript turned ON">*</a></h1>

<?php
if( isset( $_POST['pfname'] ) OR isset( $_POST['pfname2'] ) ) {
	if( isset( $_POST['pfname'] ) && $_POST['pfname2'] != "" ) {
		echo "<p>You cannot view more than one parameter file at a time!</p>" ;
		echo "<p><a href=\"$self\">Go back</a> to the parameter file explorer</p>\n" ;
		exit ;
	} elseif( isset( $_POST['pfname'] ) && file_exists( $ANT . $_POST['pfname'] ) ) {
		$pf = $ANT . $_POST['pfname'] ;
	} elseif( isset( $_POST['pfname2'] ) && file_exists( $_POST['pfname2'] ) ) {
		$pf = $_POST['pfname2'] ;
		return;
	} else {
		echo "<p>The selected parameter file does not exist!</p>\n" ;
		echo "<p><a href=\"$self\">Go back</a> to the parameter file explorer</p>\n" ;
		exit;
	}
	echo "<h2>Currently exploring: <span class=\"highlight\">" . basename( $pf ) . "</span></h2>\n" ;
	echo "<p><a href=\"$self\">Go back and choose</a> a new parameter file to explore</p>\n" ;
	$mypf =  pfget( $pf, "" ) ;
	?>
	</div>
	<div id="main">
		<div id="tree" class="button">
			<ul>
			<?php
			$i = 0 ;
			foreach( $mypf as $key=>$value ) {
				echo "<li><a href=\"#$key\" onclick=\"show('" . $key . "'," . $i . ");\">+ $key</a></li>\n" ;
				$i++ ;
			}
			?>
			</ul>
	</div>
	<?php
	foreach( $mypf as $key=>$value ) {
		echo "<div id=\"$key\" class=\"information\">\n" ;
		echo "\t<h3>$key</h3>\n" ;
		echo "\t<ul>\n" ;
		if( is_array( $value ) ) {
			foreach( $mypf[$key] as $keyChild=>$valueChild ) {
				if( is_array( $valueChild ) ) {
					echo "\t\t<li>$keyChild:\n" ;
					echo "\t\t\t<ul>\n" ;
					foreach( $mypf[$key][$keyChild] as $keyChildChild=>$valueChildChild ) {
						echo "\t\t\t\t<li class=\"grandchild\"><span class=\"grandchildkey\">$keyChildChild:</span><br/>$valueChildChild</li>\n" ;
					}
					echo "\t\t\t</ul>\n" ;
					echo "</li>\n" ;
				} else {
					echo "\t\t<li>$keyChild&nbsp;&nbsp;:&nbsp;&nbsp;$valueChild</li>\n" ;
				}
			}
		} elseif( $key == "pf_revision_time" ) {
			echo "\t\t<li>" . date( "r", $value ) . "</li>" ;
		} else {
			echo "\t\t<li>$value</li>" ;
		}
		echo "\t</ul>\n" ;
		echo "</div>\n" ;
	}
	?>
	</div>
	<?php
} else { // Define the PF file to use
	$pffiles = array() ;
	if( $handle = opendir( $ANT ) ) {
		while( false !== ( $file = readdir( $handle ) ) ) { 
			if( $file != "." && $file != ".." && $file != ".dbpickrc" ) {
				array_push( $pffiles, $file ) ;
			} 
		}
		closedir($handle); 
	}
	natcasesort( $pffiles ) ;
	?>
	</div>
	<div id="main">
		<div id="form" class="button">
			<p>The default Datascope parameter file list:</p>
			<form id="pfexplorer" name="pfexplorer" action="<? echo $self ; ?>" method="post" onchange="document.forms['pfexplorer'].submit()">
				<select name="pfname">
				<?php
				foreach( $pffiles as $p ) {
					echo "\t<option value=\"$p\">$p</option>\n"; 
				}
				?>
				</select>
				<p>or location of non-distributed parameter file:</p>
				<input type="text" name="pfname2" /><br/>
				<input type="submit" value="Submit" />
			</form>
		</div>
	</div>
	<?php
}
?>
</body>
</html>
