<?php

$self = $_SERVER['PHP_SELF'] ;

function webdbeQueryBuilder( $webdbeQueryField, $webdbeQueryOperator, $webdbeQueryVal ) {
  if( $webdbeQueryOperator == 0 ) {
    $webdbeQueryOperator = "==" ;
  } elseif( $webdbeQueryOperator == 1 ) {
    $webdbeQueryOperator = "!=" ;
  } elseif( $webdbeQueryOperator == 2 ) {
    $webdbeQueryOperator = "<" ;
  } elseif( $webdbeQueryOperator == 3 ) {
    $webdbeQueryOperator = "<=" ;
  } elseif( $webdbeQueryOperator == 4 ) {
    $webdbeQueryOperator = ">" ;
  } elseif( $webdbeQueryOperator == 5 ) {
    $webdbeQueryOperator = ">=" ;
  } else {
    $webdbeQueryOperator = "==" ;
  }
  return array( $webdbeQueryField, $webdbeQueryOperator, $webdbeQueryVal ) ;
}

function webdbe( $db ) {
  // GLOBAL variables from local webdbe.php
  $recs_per_page = $GLOBALS['recs_per_page'] ;
  // Form parameters
  $webdbe = $_GET['webdbe'] ;
  # $dbview = $_GET['db'] ;
  $dbname = $_GET['dbname'] ;
  $showall = $_GET['showall'] ;
  $sort = $_GET['sort'] ;
  $mytable = $_GET['mytable'] ;
  $graphics = $_GET['graphics'] ;
  $start = $_GET['start'] ;
  $r = $_GET['r'] ;
  $webdbeQueryField = $_GET['webdbeQueryField'] ;
  $webdbeQueryOperator = $_GET['webdbeQueryOperator'] ;
  $webdbeQueryVal = $_GET['webdbeQueryVal'] ;

  $dbname = basename( dbquery( $db, "dbDATABASE_NAME" ) ); // Database name

  if( !$webdbe ) { // Open basic db view, no specific table
    $tables = dbquery( $db, "dbSCHEMA_TABLES" );
    echo "<table id=\"webdbe\">\n";
    echo "\t<thead><tr><td colspan=\"". count( $tables ) . 
         "\" class=\"webdbeHeader\">$dbname Database</td></tr></thead>\n";
    echo "\t<tfoot><tr><td colspan=\"". count( $tables ) . 
         "\" class=\"webdbeFooter\"><a href=\"javascript:void(0)\" " . 
         "onclick=\"history.go(-1)\">Dismiss</td></tr></tfoot>\n";
    echo "\t<tbody>\n\t\t<tr>\n";
    foreach( $tables as $table ) {
      $db = dblookup( $db, "", $table, "", "" );
      if( ! dbquery( $db, "dbTABLE_PRESENT" ) ) { continue; }
      if( dbquery( $db, "dbTABLE_IS_VIEW" ) ) { continue; }
      ?>
      <td class="webdbeTable">
      <form id="<? echo $dbname ; ?>" name="<? echo $dbname ; ?>" action="<?php echo $self ; ?>" method="get">
      <input type="hidden" name="webdbe" value="1" />
      <input type="hidden" name="showall" value="0" />
      <input type="hidden" name="mytable" value="<?php echo $table?>" />
      <input type="submit" class="button" value="<?php echo $table?>" />
      </form>
      </td>
      <?php
    }
    echo "\n\t\t</tr>\n\t</tbody>\n";
    echo "</table>\n";


  } elseif( $webdbe && !$graphics ) {

    $db = dblookup( $db, "", $mytable, "", "" ); // Look up the table

    if( isset( $webdbeQueryVal ) ) { // Do we have a subset query to perform first?
      list( $qu, $op, $val ) = webdbeQueryBuilder( $webdbeQueryField, $webdbeQueryOperator, $webdbeQueryVal ) ;
      $db = dbprocess( $db, "dbsubset $qu $op \"$val\"" ) ;
    }

    if( $showall == 0 ) { // Do you want to see all the records?
      if( !$start ) { // Next / previous page functions
        $start = '0' ;
      } 
    }

    if( isset( $sort ) ) { // Check for a sort function
      $db = dbprocess( $db, "dbsort $sort" ) ;
    } 

    $nrecs = dbnrecs( $db ); // Number of records in result set
    $npages = ceil( $nrecs / $recs_per_page ) ; // Number of pages in result set

    if( intval( $nrecs ) < intval( $recs_per_page ) ) { // Show all records?
        $showall = 2 ;
    }

    if( $nrecs < intval( $start + $recs_per_page ) || $showall == 2 ) { // Upper limit of records returned ($finish)
        $finish = intval( $nrecs - 1 ) ;
    } else {
        $finish = ( intval( $start + $recs_per_page ) - 1 ) ;
    }

    if( $finish > $nrecs ) { // Ensure no empty rows if trying to grab too many rows
        $finish = $nrecs ;
    }

    # $dbview = implode( ",", $db ) ;
    $table_fields = dbquery( $db, "dbTABLE_FIELDS" ); // Get field names

    // Make new arrays
    $fields = array();
    $r = array();

    foreach( $table_fields as $field ) { // Get the fields for the table
      $dbt = dbprocess( $db, "dbsubset $field != NULL" ); // Get rid of the null fields
      if( dbnrecs( $dbt ) > 0 ) { // Create array $fields
        array_push( $fields, $field );
      }
    }
    echo "<form name=\"webdbeForm\" action=\"$self\" method=\"get\">\n" ;
    echo "<input type=\"hidden\" name=\"webdbe\" value=\"1\" />\n" ;
    echo "<input type=\"hidden\" name=\"dbname\" value=\"$dbname\" />\n" ;
    echo "<input type=\"hidden\" name=\"mytable\" value=\"$mytable\" />\n"; ;
    # echo "<input type=\"hidden\" name=\"dbview\" value=\"$dbview\" />\n" ;
    # echo "<input type=\"hidden\" name=\"graphics\" value=\"\" />\n"; ;
    echo "<input type=\"hidden\" name=\"sort\" value=\"$sort\" />\n"; ;

    $num_fields = count( $fields ) + 1 ;

    echo "<table id=\"webdbe\">\n";
    echo "\t<thead>\n" ;
    echo "\t\t<tr>\n\t\t\t<td colspan=\"". $num_fields . "\" class=\"webdbeHeader\">$dbname.$mytable</td>\n" ;
    echo "\t\t</tr>\n\n" ;
    echo "\t\t<tr>\n\t\t\t<td colspan=\"". $num_fields . "\" class=\"webdbeMenu\">WEBDBE MENUBAR [CURRENTLY INACTIVE]</td>\n\t\t</tr>\n\n" ;
    # echo "\t\t<tr>\n\t\t\t<td colspan=\"". $num_fields . "\" class=\"webdbeQuery\">SUBSET MENUBAR:&nbsp;&nbsp;&nbsp;$webdbeQuery\n" ;
    echo "\t\t<tr>\n\t\t\t<td colspan=\"". $num_fields . "\" class=\"webdbeQuery\">SUBSET MENUBAR:&nbsp;&nbsp;&nbsp;\n" ;
    echo "\t\t\t\t\t<select name=\"webdbeQueryField\">\n" ;
    foreach( $fields as $field ) { // Dynamically create the drop down subset query form
      echo "\t\t\t\t\t\t<option>$field</option>\n" ;
    }
    echo "\t\t\t\t\t</select>&nbsp;&nbsp;\n" ;
    echo "\t\t\t\t\t<select name=\"webdbeQueryOperator\">\n" ;
    echo "\t\t\t\t\t\t<option value=\"0\">==</option>\n" ;
    echo "\t\t\t\t\t\t<option value=\"1\">!=</option>\n" ;
    echo "\t\t\t\t\t\t<option value=\"2\">&lt;</option>\n" ;
    echo "\t\t\t\t\t\t<option value=\"3\">&le;</option>\n" ;
    echo "\t\t\t\t\t\t<option value=\"4\">&gt;</option>\n" ;
    echo "\t\t\t\t\t\t<option value=\"5\">&ge;</option>\n" ;
    echo "\t\t\t\t\t</select>&nbsp;&nbsp;\n" ;
    echo "\t\t\t\t\t<input type=\"text\" name=\"webdbeQueryVal\" value=\"\">&nbsp;&nbsp;\n";
    echo "\t\t\t\t\t<input type=\"submit\" value=\"subset\">&nbsp;&nbsp;\n" ;
    echo "\t\t\t\t\t<input type=\"reset\" value=\"X\">\n" ;
    echo "</td>\n\t\t</tr>" ;
    echo "\t\t<tr>\n\t\t\t<td colspan=\"". $num_fields . "\" class=\"webdbePages\">Records: $nrecs.&nbsp;&nbsp;&nbsp;$recs_per_page per page.&nbsp;&nbsp;&nbsp;Pages: ($npages).&nbsp;&nbsp;&nbsp;" ;

    if( $showall != 2 ) { // Show all or paginate?
      echo "<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=2&start=0\">Show All</a>&nbsp;&nbsp;&nbsp;" ;
    } elseif( $showall == 2 && $nrecs > $recs_per_page ) {
      echo "<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=0&start=0\">Paginate</a>&nbsp;&nbsp;&nbsp;" ;
    }

    if( $nrecs > $recs_per_page && $showall != 2 ) {
      if( $start != 0 ) { # which page are we on?
        $page = ( $start / $recs_per_page ) ;
      } else {
        $page = 0 ; // check at $start=0
      }
      $page_num = $page + 1 ; // $page starts at 0, we need to start at 1

      if( $page_num > 3 ) { // Add a link to the first page of results
        echo "<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=0&start=0\" title=\"Page: 1\">&laquo; First</a>&nbsp;...&nbsp;\n";
      }
      if( $page_num > 2 ) { // Add links to the previous two pages of results
        $page_minus_two = $page_num - 2 ;
        $start_minus_two = ( $page - 2 ) * $recs_per_page ;
        echo "<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . $start_minus_two . "\">$page_minus_two</a>&nbsp;\n" ;
      }
      if( $page_num > 1 ) { // Add links to the previous two pages of results
        $page_minus_one = $page_num - 1 ;
        $start_minus_one = ( $page - 1 ) * $recs_per_page ;
        echo "<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . $start_minus_one . "\">$page_minus_one</a>&nbsp;\n" ;
      }
      echo "<strong>[$page_num]</strong>";
      if( $page_num <= ( $npages - 1 ) ) { // Add links to the next page of results
        $page_plus_one = $page_num + 1 ;
        $start_plus_one = ( $page + 1 ) * $recs_per_page ;
        echo "&nbsp;<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . $start_plus_one . "\">$page_plus_one</a>\n" ;
      }
      if( $page_num <= ( $npages - 2 ) ) { // Add links to the next two pages of results
        $page_plus_two = $page_num + 2 ;
        $start_plus_two = ( $page + 2 ) * $recs_per_page ;
        echo "&nbsp;<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . $start_plus_two . "\">$page_plus_two</a>\n" ;
      }
      if( $page_num <= ( $npages - 3 ) ) { // Add a link to the last page of results
        $start_last = ( $npages - 1 ) * $recs_per_page ;
        echo "&nbsp;...&nbsp;<a href=\"$self?webdbe=1&mytable=" . $mytable . "&showall=0&start=" . $start_last . "\" title=\"Page: $npages\">Last &raquo;</a>\n";
      }
    }
    echo "</td>\n" ;
    echo "\t\t</tr>\n\n" ;
    echo "\t\t<tr>\n";
    echo "\t\t\t<td class=\"webdbeFieldFirst\">record #</td>\n";
    foreach( $fields as $field ) {
      $dbt = dblookup( $db, "", $mytable, $field, "dbNULL" );
      $types[$field] = dbquery( $dbt, "dbFIELD_TYPE" );
      $formats[$field] = dbquery( $dbt, "dbFIELD_FORMAT" );
      $null[$field] = dbgetv( $dbt, $field );
      $descrip[$field] = wordwrap( dbquery( $dbt, "dbFIELD_DESCRIPTION" ), 20, "<br/>" ) ;
      $detail[$field] = wordwrap( dbquery( $dbt, "dbFIELD_DETAIL" ), 20, "<br/>" ) ;
      echo "<td class=\"webdbeField\">\n" ;
      echo "\t<ul id=\"menus\">\n";
      echo "\t\t<li><a href=\"#\">$field</a>\n";
      echo "\t\t\t\t<ul>\n";
      echo "\t\t\t\t\t<li><a href=\"#\">" . $descrip[$field] . "&nbsp;&nbsp;&raquo;</a>\n";
      echo "\t\t\t\t\t\t<ul><li>" . $detail[$field] . "</li></ul>\n";
      echo "\t\t\t\t\t</li>\n";
      # echo "\t\t\t\t\t<li><a href=\"" . $_SERVER['PHP_SELF'] . "?webdbe=1&mytable=$mytable&showall=0&start=$start&sort=$field\">Sort</a></li>\n";
      # echo "\t\t\t\t\t<li><a href=\"#\">Subset</li>\n";
      echo "\t\t\t\t</ul>\n";
      echo "\t\t</li></ul></td>\n";
    }
    echo "\t</tr>\n</thead>\n";
    echo "\t<tfoot>" ;
    echo "\t\t<tr>\n";
    echo "\t\t\t<td class=\"webdbeFieldFirst\">record #</td>\n";
    foreach( $fields as $field ) {
      echo "\t\t\t<td class=\"webdbeField\">$field</td>\n";
    }
    echo "\t\t</tr>\n" ;
    echo "\t\t<tr><td colspan=\"". $num_fields . "\" class=\"webdbeFooter\"><a href=\"$self?webdbe=0\">Dismiss</td></tr>\n";
    echo "\t</tfoot>\n\n";

    echo "\t<tbody>\n";
    if( $nrecs == 0 ) {
      echo "<tr><td colspan=\"". $num_fields . "\" class=\"webdbeNoRecords\">Resulting view has no records</td></tr>\n";
    } else {
      for( $db[3]=intval( $start ); $db[3]<=intval( $finish ); $db[3]++ ) {
        echo "\t\t\t<tr>\n";
        echo "\t\t\t\t<td class=\"webdbeRecordFirst\"><input name=\"r[]\" type=\"checkbox\" id=\"r\" value=\"$db[3]\" />$db[3]</td>\n";
        foreach( $fields as $field ) {
          $val = dbgetv( $db, "$field" );
          if( $val == $null[$field] ) {
            echo "\t\t\t\t<td class=\"webdbeRecord\">&nbsp;</td>\n";
          } elseif( $types[$field] == "dbTIME" ) {
            echo "\t\t\t\t<td class=\"webdbeRecord\"><a href=\"javascript:void(0)\" onClick=\"queryInput('$val');\"> " . dbex_eval( $db, "strtime($field)" ) . "</a></td>\n";
          } elseif( $types[$field] == "dbSTRING" ) {
            printf( "\t\t\t\t<td class=\"webdbeRecord\"><a href=\"javascript:void(0)\" onClick=\"queryInput('$val');\">$formats[$field]</a></td>\n", $val );
          } else {
            printf( "\t\t\t\t<td class=\"webdbeRecord\"><a href=\"javascript:void(0)\" onClick=\"queryInput('$val');\">$formats[$field]</a></td>\n", $val );
          }
        }
        echo "\t\t\t</tr>\n\n";
      }
    }
    echo "\t</tbody>\n";
    echo "\t</table>\n\n";
    echo "\t</div>\n" ;
    echo "</form>\n" ;


  } elseif ( isset( $_GET['graphics'] ) ) {
    // THIS SECTION DOES NOT CURRENTLY WORK AS IT SHOULD - NEEDS WORK
    // rlnewman@ucsd.edu 2004-11-17

    # $dbview = explode( ",", $dbview ) ;
    # echo "<p>$dbview[0], $dbview[1], $dbview[2]</p>" ;
    // $db = dblookup( $db, "", $mytable, "", "" ) ;
    // if ( strlen( $sort ) > 1 ) {
    //   $db = dbprocess( $db, "dbsort $sort" ) ;
    // }
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
