<?php

function webdbe( $db ) {
  // Get form parameters
  $webdbe = $_GET['webdbe'] ;
  $dbname = $_GET['dbname'] ;
  $showall = $_GET['showall'] ;
  $sort = $_GET['sort'] ;
  $mytable = $_GET['mytable'] ;
  $graphics = $_GET['graphics'] ;
  $start = $_GET['start'] ;
  $r = $_GET['r'] ;

  // Get the database name by truncating with basename
  $dbname = basename( dbquery( $db, "dbDATABASE_NAME" ) );


  if( !$webdbe ) {

    $tables = dbquery( $db, "dbSCHEMA_TABLES" );
    echo "<div id=\"main\">\n\n";
    echo "\t<div id=\"tablename\">webdbe: $dbname</div>\n\n";
    echo "\t<table frame=\"border\" align=\"center\" rules=\"all\">\n";
    echo "\t\t<tr>\n";
    foreach( $tables as $table ) {
      $db = dblookup( $db, "", $table, "", "" );
      if( ! dbquery( $db, "dbTABLE_PRESENT" ) ) { continue; }
      if( dbquery( $db, "dbTABLE_IS_VIEW" ) ) { continue; }
      ?>
      <td>
      <form action="<?php echo $_SERVER['PHP_SELF'] ?>" method="GET">
      <input type="hidden" name="webdbe" value="1" />
      <input type="hidden" name="showall" value="0" />
      <input type="hidden" name="mytable" value="<?php echo $table?>" />
      <input type="submit" class="button" value="<?php echo $table?>" />
      </form>
      </td>
      <?php
    }
    echo "\n\t\t</tr>\n\t</table>\n";
    echo "</div>\n";


  } elseif( $webdbe && !$graphics ) {

    // Do you want to see all the records?
    if( $showall == 0 ) {
      // Next / previous page functions
      if( !$start ) {
        $start = '0' ;
      } 
    }
    // Look up the table
    $db = dblookup( $db, "", $mytable, "", "" );
    // Check for a sort function
    if( isset( $sort ) ) {
      $db = dbprocess( $db, "dbsort $sort" ) ;
    } 
    // Get the pointer to the database table that contains the field names
    $table_fields = dbquery( $db, "dbTABLE_FIELDS" );
    // Make new arrays
    $fields = array();
    $r = array();

    foreach( $table_fields as $field ) {
      $dbt = dbprocess( $db, "dbsubset $field != NULL" );
      if( dbnrecs( $dbt ) > 0 ) {
        array_push( $fields, $field );
      }
    }
    $num_fields = ( count( $fields ) +1 ) ;
		
    echo "<form name=\"webdbe\" action=\"" . $_SERVER['PHP_SELF'] . "\" method=\"GET\">\n" ;
    echo "<input type=\"hidden\" name=\"dbname\" value=\"$dbname\" />\n" ;
    echo "<input type=\"hidden\" name=\"webdbe\" value=\"1\" />\n" ;
    echo "<input type=\"hidden\" name=\"dbname\" value=\"$dbname\" />\n" ;
    echo "<input type=\"hidden\" name=\"mytable\" value=\"$mytable\" />\n"; ;
    echo "<input type=\"hidden\" name=\"graphics\" value=\"\" />\n"; ;
    echo "<input type=\"hidden\" name=\"sort\" value=\"$sort\" />\n"; ;
    include( 'webdbe_pages.php' ) ;
    echo "<div id=\"main\">\n";
    echo "<div id=\"tablename\">$dbname.$mytable</div>\n";
    include( 'webdbe_menubar.php' ) ;
    echo "<div id=\"formField\">\n<input type='submit' value='ok'>\n<input type='reset' value='X'>\n<input type='text' name='data'>\n</div>\n\n" ;
    echo "<div id=\"results\">$count_results database entries found. $records_per_page shown per page</div>\n";

    if( $showall == 2 ) {
      echo "<div id =\"pagenumsTop\">Showing ALL records</div>\n";
    } elseif( $showall == 1 ) {
      echo "<div id =\"pagenumsTop\">Showing ALL records&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=$mytable&showall=0&start=0\">Paginate</a></div>\n";
    } else {
      echo "<div id =\"pagenumsTop\">Showing records $start to $finish</div>\n";
    }

    if( $showall == 0 ) {
      echo "\t<div id=\"pagenumsTop\" class=\"pagenums\">\n\t\t" ;
      include( 'webdbe_pagenums.php' ) ;
      echo "\t</div>\n\n";
    }

    echo "<table width=\"100%\" align=\"center\" frame=\"border\" rules=\"groups|cols\">\n";
    echo "<tr>\n";
    echo "<td class=\"menu\">&nbsp;</td>\n";
    foreach( $fields as $field ) {
      echo "<td class=\"menu\">\n";
      echo "<p class=\"fieldTitle\">$field</p>\n";
      echo "<a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=$mytable&showall=0&sort=$field\">Sort</a>\n";
      echo "</td>\n" ;

      $dbt = dblookup( $db, "", $mytable, $field, "dbNULL" );
      $types[$field] = dbquery( $dbt, "dbFIELD_TYPE" );
      $formats[$field] = dbquery( $dbt, "dbFIELD_FORMAT" );
      $null[$field] = dbgetv( $dbt, $field );
    }
    echo "</tr>\n</thead>\n";

    $nrecs = dbnrecs( $db );
    echo "<tbody align='right'>\n";
    if( $showall == 1 | $showall == 2 ) {
      for( $db[3]=0; $db[3]<$nrecs; $db[3]++ ) {
        echo "\t\t\t<tr style=\"originalColor:white;\">\n";
        echo "\t\t\t\t<td nowrap><input name=\"r[]\" type=\"checkbox\" id=\"r\" value=\"$db[3]\" onClick=\"setParentTR(event, 'background', '#ADAE86', 'white')\" /></td>";
        foreach( $fields as $field ) {
          $val = dbgetv( $db, "$field" );
            if( $val == $null[$field] ) {
              echo "\t\t\t\t<td></td>\n";
            } elseif( $types[$field] == "dbTIME" ) {
              echo "\t\t\t\t<td nowrap> " . dbex_eval( $db, "strtime($field)" ) . " </td>\n";
            } elseif( $types[$field] == "dbSTRING" ) {
              printf( "\t\t\t\t<td nowrap align='left'> $formats[$field] </td>\n", $val );
            } else {
              printf( "\t\t\t\t<td nowrap> $formats[$field] </td>\n", $val );
            }
          }
          echo "\t\t\t</tr>\n\n";
        }
      } else {
      for( $db[3]=intval( $start ); $db[3]<=intval( $finish ); $db[3]++ ) {
        echo "\t\t\t<tr style=\"originalColor:white;\">\n";
        echo "\t\t\t\t<td nowrap><input name=\"r[]\" type=\"checkbox\" id=\"r\" value=\"$db[3]\" onClick=\"setParentTR(event, 'background', '#ADAE86', 'white')\" /></td>";
        foreach( $fields as $field ) {
          $val = dbgetv( $db, "$field" );
          if( $val == $null[$field] ) {
            echo "\t\t\t\t<td></td>\n";
          } elseif( $types[$field] == "dbTIME" ) {
            echo "\t\t\t\t<td nowrap> " . dbex_eval( $db, "strtime($field)" ) . " </td>\n";
          } elseif( $types[$field] == "dbSTRING" ) {
            printf( "\t\t\t\t<td nowrap align='left'> $formats[$field] </td>\n", $val );
          } else {
            printf( "\t\t\t\t<td nowrap> $formats[$field] </td>\n", $val );
          }
        }
        echo "\t\t\t</tr>\n\n";
      }
    }
    echo "\t\t</table>\n\n";
    echo "\t</div>\n" ;
    if( $showall == 0 ) {
      echo "\t<div id=\"pagenumsBottom\" class=\"pagenums\">\n\t\t" ;
      include( 'webdbe_pagenums.php' ) ;
      echo "\n\t</div>\n\n";
    }
    echo "\t<div id=\"dismiss\">\n" ;
    echo "\t\t<a href=\"" . $_SERVER['PHP_SELF'] . "\">Dismiss</a>\n" ;
    echo "\t</div>\n" ;
    echo "</div>\n" ;
    echo "</form>\n" ;


  } elseif ( isset( $_GET['graphics'] ) ) {

    $db = dblookup( $db, "", $mytable, "", "" ) ;
    if ( strlen( $sort ) > 1 ) {
      $db = dbprocess( $db, "dbsort $sort" ) ;
    }
    ?>
    <div id="main">
      <div id="tablename"><?php echo "$dbname.$mytable"; ?></div>
      <div id="table">
          <table align="center" frame="border" rules="groups|cols" >
            <tr>
    <?php
    function cool_stuff( $db, $mytable, $graphics, $r ) {
      if( $mytable == "wfdisc" && $graphics == "map" ) {
        echo "<td class=\"menu\"><strong>Filename:</strong><br/>" ;
        foreach( $r as $rec ) {
          $db[3] = intval( $rec ) ;
          $filename = dbextfile( $db ) ;
          echo "\t\t<a href=\"$filename\">$filename</a><br/>\n" ;
        }
        echo "</td>" ;
      } else {
        echo "<td class=\"menu\"><strong>Record Number:</strong><br/>" ;
        echo "You asked me to do Graphics->$graphics<br/>";
        foreach( $r as $rec ) {
          echo "\t\t<a href=\"$rec\">$rec</a><br/>\n" ;
        }
        echo "</td>" ;
      }
    }
    cool_stuff( $db, $mytable, $graphics, $r ) ;
    ?>
          </tr>
          <tr>
            <td align="center"><a href="javascript:void(0)" onClick="history.go(-1)">Back to webdbe</a></td>
          </tr>
        </table>
      </div>
    </div>
    <?php

  } else {
    die( "webdbe only works with GET requests" ) ;
  }
}
?>
