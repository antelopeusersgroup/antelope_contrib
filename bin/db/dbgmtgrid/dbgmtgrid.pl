#   Copyright (c) 2005 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use POSIX qw(ceil floor);

sub repetend {
	my( $approx ) = @_;

	$approx =~ s/333$/33333333333333333333/;
	$approx =~ s/667$/66666666666666666666/;

	return $approx;
}

sub dbgmtgrid_next_lower {
	my( $in, $incr ) = @_;

	$incr = repetend( $incr );

	return floor( $in / $incr ) * $incr;
}

sub dbgmtgrid_next_higher {
	my( $in, $incr ) = @_;

	$incr = repetend( $incr );

	return ceil( $in / $incr ) * $incr;
}

sub check_executable {
	my( $helper ) = @_;

	if( ! defined( my( $helperpath ) = datafile( "PATH", "$helper" ) ) ) {

		elog_complain( "dbgmtgrid: can't find $helper" );
		return 0;

	} elsif( ! -x "$helperpath" ) {

		elog_complain( "dbgmtgrid: $helperpath not executable" );
		return 0;
	}

	return 1;
}

sub uniq_tempfile {
	
	$DBGMTGRID_uniq_tempfile++;

	return "dbgmtgrid_$<_$$\_$DBGMTGRID_uniq_tempfile";
}

sub eastwest_coverage {
	my( @db ) = splice( @_, 0, 4 );
	my( $west, $east ) = @_;
	
	my( $rowwest, $roweast ) = dbgetv( @db, "west", "east" );

	if( $rowwest <= $west && $east <= $roweast ) {

		return( $west, $east );

	} elsif( $west <= $rowwest && $roweast <= $east ) {

		return( $rowwest, $roweast );

	} elsif( $rowwest <= $west && $west < $roweast ) {

		return( $west, $roweast );

	} elsif( $rowwest < $east && $east <= $roweast ) {

		return( $rowwest, $east );

	} else {

		return ();
	}
}

sub northsouth_coverage {
	my( @db ) = splice( @_, 0, 4 );
	my( $south, $north ) = @_;
	
	my( $rowsouth, $rownorth ) = dbgetv( @db, "south", "north" );

	if( $rowsouth <= $south && $north <= $rownorth ) {

		return( $south, $north );

	} elsif( $south <= $rowsouth && $rownorth <= $north ) {

		return( $rowsouth, $rownorth );

	} elsif( $rowsouth <= $south && $south < $rownorth ) {

		return( $south, $rownorth );

	} elsif( $rowsouth < $north && $north <= $rownorth ) {

		return( $rowsouth, $north );

	} else {

		return ();
	}
}

sub tempgrid_rowremove {
	my( @db ) = splice( @_, 0, 4 );

	my( $myfile ) = dbextfile( @db );
	unlink( "$myfile" );

	my( $name ) = dbgetv( @db, "name" );

	@db = dblookup( @db, "", "grids", "name", $name );

	dbmark( @db );

	return;
}

sub tempgrid_destroy {
	my( @db ) = splice( @_, 0, 4 );

	$nrecs = dbquery( @db, dbRECORD_COUNT );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

		tempgrid_rowremove( @db );
	}

	dbdestroy( @db );

	return;
}

sub asplice {
	my( @db ) = splice( @_, 0, 4 );
	my( $common1, $common2, $low, $high ) = @_;
	my( %Options ) = @_;
	my( $V, $dir, $dfile, $name );

	if( $Options{verbose} ) {
		$V = "-V";
	} else {
		$V = "";
	}

	@db = dbprocess( @db, "dbopen grids",
			      "dbsort $common1 $common2 $low",
			      "dbgroup $common1 $common2",
			      "dbsubset $common1 != NULL && $common2 != NULL",
			      "dbsubset count() > 1" );

	if( dbquery( @db, dbRECORD_COUNT ) <= 0 ) {
		return 0;
	}

	$db[3] = 0;
	my( $dbbundle ) = dbgetv( @db, "bundle" );
	my( @dbbundle ) = split( ' ', $dbbundle );
	my( @db1 ) = @dbbundle;
	$db1[2] = -501;

	for( $db1[3] = $dbbundle[3]; $db1[3] < $dbbundle[2]-1; $db1[3]++ ) {
		my( @db2 ) = @db1;
		$db2[3]++;

		my( $file1 ) = dbextfile( @db1 );
		my( $c1, $c2, $low1, $high1 ) =
			dbgetv( @db1, $common1, $common2, $low, $high );

		my( $file2 ) = dbextfile( @db2 );
		my( $low2, $high2 ) =
			dbgetv( @db2, $low, $high );

		if( $high1 == $low2 ) {

			$dir = $Workdir;
			$name = uniq_tempfile();
			$dfile = "$name.grd";
			$result = concatpaths( $dir, $dfile );

			$cmd = "grdpaste $V -G$result $file1 $file2";

			if( $Options{verbose} ) {
				print STDERR "Executing: $cmd\n";
			}
			system( $cmd );

			dbaddv( @dbtemp, "name", $name,
					 $common1, $c1,
				 	 $common2, $c2,
				 	 $low, $low1,
				 	 $high, $high2,
				 	 "dir", $dir,
				 	 "dfile", $dfile );
			
			tempgrid_rowremove( @db1 );
			tempgrid_rowremove( @db2 );
			last;
		} 
	}

	@db = dblookup( @db, "", "grids", "", "" );
}

sub splicegrids {
	my( @db ) = splice( @_, 0, 4 );
	my( %Options ) = @_;

	@db = dblookup( @db, "", "grids", "", "" );
	@db = dbsubset( @db, "name != NULL" );

	while( dbquery( @db, dbRECORD_COUNT ) > 1 ) {

		asplice( @db, "west", "east", "south", "north", %Options );
		asplice( @db, "south", "north", "west", "east", %Options );

		@db = dblookup( @db, "", "grids", "", "" );
		@db = dbsubset( @db, "name != NULL" );
	}

	return 0;
}

sub addgrid {
	my( @dbsource ) = splice( @_, 0, 4 );
	my( @dbtemp ) = splice( @_, 0, 4 );
	my( $wc, $ec, $sc, $nc ) = splice( @_, 0, 4 );
	my( %Options ) = @_;
	my( $V, @cmd );

	if( $Options{verbose} ) {
		$V = "-V";
	} else {
		$V = "";
	}

	my( $source ) = dbextfile( @dbsource );

	my( $shift ) = $Options{shift} ? $Options{shift} : 0;

	my( $dir ) = $Workdir;
	my( $name ) = uniq_tempfile();
	my( $dfile ) = "$name.grd";
	my( $result ) = concatpaths( $dir, $dfile );

	if( $Options{spacing} ) {

		my( $reg ) = dbgetv( @dbsource, "registration" );
		my( $F ) = $reg eq "pixel" ? "-F" : "";

		@cmd = ( "grdsample", "$F", "-I$Options{spacing}" );

	} else {

		@cmd = ( "grdcut" );
	}

	push( @cmd, "$V", "-G$result", "$source", "-R$wc/$ec/$sc/$nc" );

	if( $Options{verbose} ) {
		print STDERR "Executing: " . join( " ", @cmd ) . "\n";
	}
	system( @cmd );

	if( $shift != 0 ) {

		$wc -= $shift;
		$ec -= $shift;

		my( $cmd ) = "grdedit $V $result -R$wc/$ec/$sc/$nc";

		if( $Options{verbose} ) {
			print STDERR "Executing: $cmd\n";
		}

		system( "$cmd" );
	}

	dbaddv( @dbtemp, "name", $name,
		         "west", $wc,
		         "east", $ec,
		         "south", $sc,
		         "north", $nc, 
		         "dir", $dir,
		         "dfile", $dfile );
	return;
}

sub dms_convert {
	my( $val ) = @_;

	my( @parts ) = split( /:/, $val );

	$val = $parts[0];

	if( defined( $parts[1] ) ) {

		$val += $parts[1] / 60.;
	}

	if( defined( $parts[2] ) ) {

		$val += $parts[2] / 3600.;
	}

	return $val;
}

sub dbgmtgrid {
	my( @db ) = splice( @_, 0, 4 );
	my( $rectangle, $outfile ) = splice( @_, 0, 2 );
	my( %Options ) = @_;

	my( $dx, $dy, $V, $cmd, $source );

	if( $Options{verbose} ) {
		$V = "-V";
	} else {
		$V = "";
	}

	if( defined( $Options{workdir} ) && $Options{workdir} ne "" ) {

		$Workdir = abspath( $Options{workdir} );

	} else {

		$Workdir = "/tmp";
	}

	if( ! check_executable( "grdcut" ) ||
	    ! check_executable( "grdedit" ) ||
	    ! check_executable( "grdsample" ) ||
	    ! check_executable( "grdpaste" ) ) {
		return -1;
	}

	if( dbquery( @db, dbRECORD_COUNT ) <= 0 ) {

		elog_complain( "dbgmtgrid: no grids in input database\n" );
		return -1;

	} else {

		@dbt = dbgroup( @db, "dx" );
		if( dbquery( @dbt,  dbRECORD_COUNT ) != 1 ) {

			elog_complain( "dbgmtgrid: inconsistent dx values " .
				       "in input database\n" );
			return -1;

		} else {
			$dbt[3] = 0;
			$dx = dbgetv( @dbt, "dx" );
		}

		@dbt = dbgroup( @db, "dy" );
		if( dbquery( @dbt,  dbRECORD_COUNT ) != 1 ) {

			elog_complain( "dbgmtgrid: inconsistent dy values " .
				       "in input database\n" );
			return -1;

		} else {
			$dbt[3] = 0;
			$dy = dbgetv( @dbt, "dy" );
		}
	}

	$rectangle =~ s/^-R//;
	my( $west, $east, $south, $north ) = split( "/", $rectangle );

	$west = dms_convert( $west );
	$east = dms_convert( $east );
	$south = dms_convert( $south );
	$north = dms_convert( $north );

	if( $west > $east ) {
		$east += 360;
	}

	# prevent pathological rounding errors (mismatch of request 
	# bounds with grid-point spacing) during grid cut:

	$west = dbgmtgrid_next_lower( $west, $dx );
	$south = dbgmtgrid_next_lower( $south, $dy );
	$east = dbgmtgrid_next_higher( $east, $dx );
	$north = dbgmtgrid_next_higher( $north, $dy );

	$tempdbname = "$Workdir/dbgmtgrid_$<_$$";
	dbcreate( $tempdbname, "gmt1.0" );
	@dbtemp = dbopen( $tempdbname, "r+" );
	@dbtemp = dblookup( @dbtemp, "", "grids", "", "" );

	$nsources = dbquery( @db, dbRECORD_COUNT );

	for( @db[3] = 0; $db[3] < $nsources; $db[3]++ ) {

		($sc, $nc) = northsouth_coverage( @db, $south, $north );
		next unless( defined( $sc ) && defined( $nc ) );

		foreach $shift ( -720, -360, 0, 360, 720 ) {

			my( $wshift ) = $west + $shift;
			my( $eshift ) = $east + $shift;

			($wc, $ec) = 
			  eastwest_coverage( @db, $wshift, $eshift );

			next unless( defined( $wc ) && defined( $ec ) );

			$Options{shift} = $shift;

			addgrid( @db, @dbtemp,
				 $wc, $ec, $sc, $nc,
				 %Options );
		}
	}

	splicegrids( @dbtemp, %Options );

	@dbtemp = dbsubset( @dbtemp, "name != NULL" );

	if( dbquery( @dbtemp, dbRECORD_COUNT ) != 1 ) {

		elog_complain( "dbgmtgrid: grid splice failed\n" );

	} else {

		$dbtemp[3] = 0;
		$cmd = "cp " . dbextfile( @dbtemp ) . " $outfile";

		if( $Options{verbose} ) {
			print STDERR "Executing: $cmd\n";
		}
		system( $cmd );
	}

	tempgrid_destroy( @dbtemp );

	return 0;
}

1;
