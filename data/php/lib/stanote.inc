<?php

function stanote_times( $time, $range ) {
	if( isset( $time ) ) {
		$time = str2epoch( $time ) ;
	} else {
		$time = intval( time()/86400 ) * 86400 ;
	}
	if( isset( $range ) ) {
		if( $range == "-" ) {
			$endtime = "9999999999.99900" ;
		} else {
			$range = str2epoch( $range ) ;
			$endtime = $time + $range -1 ;
		}
	} else {
		$range = 86400 ;
		$endtime = $time + $range -1 ;
	}
	return array( $time, $endtime ) ;
}

function stanote_add( $database, $sta, $time, $range, $format, $note, $auth ) {
	if( empty( $database ) || empty( $sta ) || empty( $time ) || empty( $range ) || empty( $format ) || empty( $note ) || empty( $auth ) ) {
		error_log( "Datascope_PHP: stanote_add error: Usage: stanote_add( database, sta, time, range, format, note, auth", 0 ) ;
		return -1 ;
	} else {
		$notes_database = ds_dbopen( $database, "r+" ) ;
		$notes_table = dblookup( $notes_database, "", "stanotes", "", "" ) ;
		if( $notes_table[0] < 0 || $notes_table[1] < 0 ) {
			error_log( "Datascope_PHP: stanote_add error: Can't open database $database", 0 ) ;
			return -1 ;
		} else {
			if( $format == "gif" || $format == "jpg" || $format == "png" || $format == "tif" || $format == "txt"  ) { # check note format
				if( $format == "txt" ) {
					$note = addslashes( htmlentities( $note ) ) ; # make txt string 'safer'
				}
				list( $time, $endtime ) = stanote_times( $time, $range ) ; # create time strings
				$dir = dbquery( $notes_table, "dbTABLE_DIRNAME" ) ; # find where the database resides
				$dir .= "/notes" ;
				if( !$dir ) { # create notes directory if none exists
					mkdir( $dir, 644 ) ;
				}
				$yday = epoch2str( $time, "%Y-%j" ) ;
				$file = $sta . "_" . $yday ;
				if( file_exists( "$dir/$file" ) ) { # ensure a unique file name
					$sequence = 1 ;
					while( file_exists( "$dir/$file.$sequence" ) ) { $sequence++ ; }
					$file = $sta . "_" . $yday . ".$sequence" ;
				}
				if( !$handle = fopen( "$dir/$file", "w+" ) ) { # open file for writing
					error_log( "Datascope_PHP: stanote_add error: Can't open file $dir/$file for writing", 0 ) ;
					return -1 ;
				}
				if( flock( $handle, LOCK_EX ) ) { # exclusively lock the file
					if( fwrite( $handle, $note ) === FALSE ) { # write note to file
						error_log( "Datascope_PHP: stanote_add error: Can't write to file $dir/$file", 0 ) ;
						return -1 ;
					}
					flock( $handle, LOCK_UN ) ; # unlock file
				} else {
					error_log( "Datascope_PHP: stanote_add error: Can't lock file $dir/$file with flock", 0 ) ;
					return -1 ;
				}
				fclose( $handle ) ; # close file for writing
				dbaddv( $notes_table, "sta", $sta, "time", $time, "endtime", $endtime, "auth", $auth, "format", $format, "dir", $dir, "dfile", $file ); # add entry to db
				return 0 ;
			} else { 
				error_log( "Datascope_PHP: stanote_add error: Format of comment is $format. It must be either gif, jpg, png, tif or txt", 0 ) ;
				return -1 ;
			}
		}
	}
}

function stanote_edit( $database, $sta, $time, $range, $format, $note, $auth, $lddate ) {
	if( empty( $database ) || empty( $sta ) || empty( $time ) || empty( $range ) || empty( $format ) || empty( $note ) || empty( $auth ) || empty( $lddate ) ) {
		error_log( "Datascope_PHP: stanote_edit error: Usage: stanote_edit( database, sta, time, range, format, note, auth, lddate", 0 ) ;
		return -1 ;
	} else {
		$notes_database = ds_dbopen( $database, "r+" ) ;
		$notes_table = dblookup( $notes_database, "", "stanotes", "", "" ) ;
		if( $notes_table[0] < 0 || $notes_table[1] < 0 ) {
			error_log( "Datascope_PHP: stanote_edit error: Can't open database $database", 0 ) ;
			return -1 ;
		} else {
			if( $format == "gif" || $format == "jpg" || $format == "png" || $format == "tif" || $format == "txt"  ) { # check note format
				list( $time, $endtime ) = stanote_times( $time, $range ) ; # create time strings
				$nrecs_notes_table = dbnrecs( $notes_table ) ; # number of records in db
				$expr = "sta==\"$sta\" && lddate == $lddate" ; # subset expression
				$notes_table[3] = dbfind( $notes_table, $expr, $nrecs_notes_table, 1 ) ; # retrieve the unique lddate record
				switch( $notes_table[3] ) {
					case -1:
						error_log( "Datascope_PHP: stanote_edit error: Compilation error when using expression $expr", 0 ) ; # compilation error
						return -1 ;
						break ;
					case -2:
						error_log( "Datascope_PHP: stanote_edit error: Search failed - went past last record using expression $expr", 0 ) ; # search failed - past last record
						return -1 ;
						break ;
					case -3:
						error_log( "Datascope_PHP: stanote_edit error: Search failed - went past first record using expression $expr", 0 ) ; # search failed - past first record
						return -1 ;
						break ;
					default:
						break ;
				}
				# insert error catch for no record found

				$dir = dbquery( $notes_table, "dbTABLE_DIRNAME" ) ;  # find where the database resides
				$dir .= "/notes" ;
				$file = dbgetv( $notes_table, "dfile" ) ;
				if( !unlink( "$dir/$file" ) ) { # delete file
					error_log( "Datascope_PHP: stanote_edit error: Can't delete file $dir/$file for replacement", 0 ) ;
					return -1 ;
				}
				if( !$handle = fopen( "$dir/$file", "w+" ) ) { # open file for writing
					error_log( "Datascope_PHP: stanote_edit error: Can't open file $dir/$file for writing", 0 ) ;
					return -1 ;
				}
				if( flock( $handle, LOCK_EX ) ) { # exclusively lock the file
					if( fwrite( $handle, $note ) === FALSE ) { # write note to file
						error_log( "Datascope_PHP: stanote_edit error: Can't write to file $dir/$file", 0 ) ;
						return -1 ;
					}
					flock( $handle, LOCK_UN ) ; # unlock file
				} else {
					error_log( "Datascope_PHP: stanote_edit error: Can't lock file $dir/$file with flock", 0 ) ;
					return -1 ;
				}
				fclose( $handle ) ; # close file for writing
				dbputv( $notes_table, "sta", $sta, "time", $time, "endtime", $endtime, "auth", $auth, "format", $format, "dir", $dir, "dfile", $file ) ; # edit entry in db
				return 0 ;
			} else { 
				error_log( "Datascope_PHP: stanote_edit error: Format of comment is $format. It must be either gif, jpg, png, tif or txt", 0 ) ;
				return -1 ;
			}
		}
	}
}

function stanote_delete( $database, $sta, $lddate ) {
	if( empty( $database ) || empty( $sta ) || empty( $lddate ) ) {
		error_log( "Datascope_PHP: stanote_delete error: Usage: stanote_delete( database, sta, lddate", 0 ) ;
		return -1 ;
	} else {
		$notes_database = ds_dbopen( $database, "r+" ) ;
		$notes_table = dblookup( $notes_database, "", "stanotes", "", "" ) ;
		if( $notes_table[0] < 0 || $notes_table[1] < 0 ) {
			error_log( "Datascope_PHP: stanote_delete error: Can't open database $database", 0 ) ;
			return -1 ;
		} else {
			$nrecs_notes_table = dbnrecs( $notes_table ) ; # number of records in db
			$expr = "sta==\"$sta\" && lddate == $lddate" ; # subset expression
			$notes_table[3] = dbfind( $notes_table, $expr, $nrecs_notes_table, 1 ) ; # retrieve the unique lddate record
			switch( $notes_table[3] ) {
				case -1:
					error_log( "Datascope_PHP: stanote_delete error: Compilation error when using expression $expr", 0 ) ; # compilation error
					return -1 ;
					break ;
				case -2:
					error_log( "Datascope_PHP: stanote_delete error: Search failed - went past last record using expression $expr", 0 ) ; # search failed - past last record
					return -1 ;
					break ;
				case -3:
					error_log( "Datascope_PHP: stanote_delete error: Search failed - went past first record using expression $expr", 0 ) ; # search failed - past first record
					return -1 ;
					break ;
				default:
					break ;
			}
			# insert error catch for no record found

			$dir = dbquery( $notes_table, "dbTABLE_DIRNAME" ) ;  # find where the database resides
			$dir .= "/notes" ;
			$file = dbgetv( $notes_table, "dfile" ) ;
			if( !unlink( "$dir/$file" ) ) { # delete file
				error_log( "Datascope_PHP: stanote_delete error: Can't delete file $dir/$file for replacement", 0 ) ;
				return -1 ;
			}
			dbdelete( $notes_table ) ; # delete entry in db
			return 0 ;
		}
	}
}

?>
