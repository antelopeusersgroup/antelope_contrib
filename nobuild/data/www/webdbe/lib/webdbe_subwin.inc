<?php
// PHP Code here
$help = $_GET['help'] ;
$mytable = $_GET['mytable'] ;
?>

<html>
<head>
  <title>Webdbe Help Pages</title>
  <link rel="stylesheet" href="webdbe_style.css" type="text/css" />
</head>
<body>
<img src="webdbe_title.gif" alt="webdbe logo" />
<div id="main">
<?php
if ( $help == "about" ) {
    echo "<div id=\"tablename\">About webdbe</div>";
    echo "<div id=\"menubar\">Latest version: " . date ("F d Y.", getlastmod() ) . "<br/>WEBDBE 1.5</div>";
    echo "<p>Developed by Rob Newman and  Kent Lindquist</p>";
    echo "<p>rlnewman@ucsd.edu<br/>(858) 822-1333</p>";

} elseif( $help == "schema" ) {
    echo "<div id=\"tablename\">On Schema</div>\n";
    echo "<div id=\"menubar\">Schema: Center for Seismic Studies Schema Version 3.0</div>\n";
    echo "<p>Modifications from original CSS documentation:<br/><br/>\n";
    echo "\t1) Null values corrected for certain attributes.<br/>\n";
    echo "\t2) offdate added to primary keys for tables in which it occurs.<br/>\n";
    echo "\t3) endtime added to primary keys for tables in which it occurs.<br/>\n";
    echo "\t4) time made first primary key in origin for sorting.<br/>\n";
    echo "\t5) arid and orid added to foreign keys in assoc.<br/>\n";
    echo "\t6) made range values expression for automated testing<br/>\n";
    echo "\t7) added wfedit relation 12/3/93<br/>\n";
    echo "\t8) changed the primary key in sitechan to chanid, and added chanid as a foreign key in sensor to force joins of sitechan to go through sensor table.<br/>\n";
    echo "\t9) changed primary keys in moment and centryd table to orid.<br/>\n";
    echo "\t10) added calibration and stage tables 1/31/94<br/>\n";
    echo "\t11) changed primary keys in stamag to arid, magtype, sta, orid<br/>\n";
    echo "\t12) changed primary key in site to sta (no ondate, offdate)<br/>\n";
    echo "\t13) changed null values for origerr's covariant matrix<br/>\n";
    echo "\t14) changed definition of ndef for origins included from other catalogs<br/>\n";
    echo "\t15) added beam, fkgrid and stgrid tables to accomodate array processing 12/15/94<br/>\n";
    echo "\t16) added wftar table to accomodate tar tape waveform archiving 1/9/95<br/>\n";
    echo "\t17) changed all NONULL null values to reasonable values<br/>\n";
    echo "\t18) added wfrms table<br/>\n";
    echo "\t19) added wfmeas table for holding generic waveform measurements<br/>\n";
    echo "\t20) segment origin and stassoc etype field into two fields, etype and review, so that analyst review status can be kept in origin table<br/>\n";
    echo "\t21) add snetsta, anetsta, schanloc and achanaux tables to translate between foreign volumes of SEED or autoDRM into local databases.<br/>\n";
    echo "\t22) add specdisc table and associated attributes to support spectral estimation processing<br/>\n";
    echo "\t23) add rsprm to specdisc table<br/>\n";
    echo "\t24) added tables dmcseed and dmcwf to database to support all the DMC requirements for building DMC seed volumes.  Changed default value of fileno to -1. <br/>\n";
    echo "\t25) changed format of chksum from Integer %15d to Real %12.0lf to make sure table is written properly.  Previous format would wrap to negative numbers in some cases which would corrupt the database.<br/>\n";
    echo "\t26) added fields calib, calper, samprate, timever to table dmcwf and field totbytes to table dmcseed.<br/>\n";
    echo "\t27) changed primary keys of stamag from arid magtype sta and orid to magid magtype sta and orid.<br/>\n";
    echo "\t28) changed fields dmcseedfile to dfile and jdate to yearday in tables dmcwf and dmcseed.<br/>\n";
    echo "\t29) added focal mechanism calculation related tables and emodel table <br/>\n";
    echo "\t30) extensive changes to wfrms table to make compatible with orbwfrms<br/>\n";
    echo "\t40) made instrument table alternate key all fields except inid and lddate<br/>\n";
    echo "\t41) introduced new Calibration table, some version of which will ultimately replace calibration.<br/>\n";
    echo "</p>" ;

} elseif( $help == 'table' ) {
    echo "<div id=\"tablename\">On $mytable</div>\n";
    echo "<div id=\"menubar\">Background information for each table</div>\n";
    if( $mytable == 'affiliation' ) {
      echo "<p align=\"center\">This is an intermediate relation by which seismic stations may be clustered into networks.</p>\n";
    } elseif( $mytable == 'arrival' ) { 
      echo "<p align=\"center\">Information characterizing a 'seismic phase' observed at a particular station is saved here.</p>\n";
    } elseif( $mytable == 'assoc' ) { 
      echo "<p align=\"center\">This table has information that connects arrivals (i.e., entries in the arrival relation) to a particular origin.</p>\n";
    } elseif( $mytable == 'instrument' ) { 
      echo "<p align=\"center\">This table serves three purposes: (1) it holds nominal one-frequency calibration factors for each instrument, (2) it holds pointers to the nominal frequency-dependent calibration for an instrument, (3) it holds pointers to the exact calibrations obtained by direct measurement on a particular instrument.</p>\n";
    } elseif( $mytable == 'lastid' ) { 
      echo "<p align=\"center\">This relation is a reference table from which programs may retrieve the last sequential value of one of the numeric keys.</p>\n";
    } elseif( $mytable == 'netmag' ) { 
      echo "<p align=\"center\">This table summarizes estimates of network magnitudes of different types for an event.</p>\n";
    } elseif( $mytable == 'network' ) { 
      echo "<p align=\"center\">This relation gives general information about seismic networks.</p>\n";
    } elseif( $mytable == 'origin' ) { 
      echo "<p align=\"center\">Information describing a derived or reported origin for a particular event is stored in this table.</p>\n";
    } elseif( $mytable == 'sensor' ) { 
      echo "<p align=\"center\">This table provides a record of updates in the calibration factor or clock error of each instrument, and links a sta/chan/time to a complete instrument response in the relation instrument.</p>\n";
    } elseif( $mytable == 'site' ) { 
      echo "<p align=\"center\">Site names and describes a point on the earth where seismic measurements are made.</p>\n";
    } elseif( $mytable == 'sitechan' ) { 
      echo "<p align=\"center\">This relationship describes the orientation of a recording channel at the site referenced by sta.</p>\n";
    } elseif( $mytable == 'stamag' ) { 
      echo "<p align=\"center\">This table summarizes station magnitude estimates based upon measurements made on specific seismic phases, with a specified distance.</p>\n";
    } elseif( $mytable == 'wfdisc' ) { 
      echo "<p align=\"center\">This relation provides a pointer (or index) to waveforms stored on disk.</p>\n";
    } else {
      echo "<p align=\"center\">$mytable is not a valid tablename</p>";
    }

} else {
  echo "<p>You did not define what you wanted to see!</p>";
  exit ;
}
?>
</div>
</body>
</html>
