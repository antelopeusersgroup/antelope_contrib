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
#   pfexplore.inc  : parameter file explore function/class
#   Written By     : Rob Newman 2005-03-03
#   Manpage        : pfexplore.3h
#

function pfexplore( $mypf, $key ) {
	$mypfname = $mypf ;
	$mypf =  pfget( $mypf, $key ) ;
	?>
	<div id="parent">
		<div id="pfsDiv" class="button">
			<div id="buttonTitle" class="highlight">
				<a href="<?php $_SERVER['HTTP_REFERER'] ; ?>" title="Explore another parameter file">
					<span class="backButtonFormat">&larr;</span></a> &nbsp; <?php echo basename( $mypfname ) ; ?>
			</div>
			<ul>
			<?php
			$i = 0 ;
			foreach( $mypf as $key=>$value ) {
				# echo "<li><a href=\"#$key\" onclick=\"show('" . $key . "'," . $i . ");\">+ $key</a></li>\n" ;
				echo "<li><a href=\"javascript:void(0);\" onclick=\"show('" . $key . "'," . $i . ");\">+ $key</a></li>\n" ;
				$i++ ;
			}
			?>
			</ul>
	</div>
	<?php
	foreach( $mypf as $key=>$value ) {
		echo "<div id=\"$key\" class=\"pfContentClass\">\n" ;
		echo "\t<h3>$key</h3>\n" ;
		if( is_array( $value ) ) {
			echo "\t<table class=\"keyValTable\">\n" ;
			echo "\t\t<tr><th>Key</th><th>Value</th></tr>\n" ;
			foreach( $mypf[$key] as $keyChild=>$valueChild ) {
				if( is_array( $valueChild ) ) {
					echo "\t\t<tr><td class=\"tblKey\">$keyChild</td>\n" ;
					echo "\t\t\t<td class=\"tblVal\">\n" ;
					echo "\t\t\t<table class=\"keyValTable\">\n" ;
					echo "\t\t\t<tr><th>Key</th><th>Value</th></tr>\n" ;
					foreach( $mypf[$key][$keyChild] as $keyChildChild=>$valueChildChild ) {
						echo "\t\t\t\t<tr><td class=\"tblKey\">$keyChildChild</td><td class=\"tblVal\">$valueChildChild</td></tr>\n" ;
					}
					echo "\t\t\t<tr><th>Key</th><th>Value</th></tr>\n" ;
					echo "\t\t\t</table>\n" ;
					echo "\t\t\t</td>\n" ;
					echo "\t\t\t</tr>\n" ;
				} else {
					echo "\t\t<tr><td class=\"tblKey\">$keyChild</td><td class=\"tblVal\">$valueChild</td></tr>\n" ;
				}
			}
			echo "\t<tr><th>Key</th><th>Value</th></tr>\n" ;
			echo "\t</table>\n" ;
		} elseif( $key == "pf_revision_time" ) {
			echo "\t\t<p>" . date( "r", $value ) . "</p>\n" ;
		} else {
			echo "\t\t<p>$value</p>\n" ;
		}
		echo "</div>\n" ;
	}
	?>
	</div>
	<?php
}
