# rrr2db for CTBT RN Bulletins
#
# Nikolaus Horn
# ZAMG / Vienna
# 2009-09-23

use Datascope ;


if ( @ARGV < 2 ) {

	die( "Usage: $0 filename database\n" );

}

sub init_globals {

	$State = "startup";

	use vars qw(
			$creation_date,$arrival_time,$sta, $chan, $stachan, $type,
			$collection_start,$collection_stop,$acquisition_start,$acquisition_stop,
			$sample_id,
			@db_ARR,@db_RRR,@db_ARR_nuclide,@db_RRR_nuclide, @n_table
			);
}

sub stateswitch {
	( $line ) = @_;

	if( $line =~ /^\s*DATA_TYPE\s+(\S+)\s+(\S+)/i ) {
		$type = uc( $1 );
		if( $type ne "ARR" &&
		    $type ne "RRR" ) {
			die( "File $ARGV Not in ARR, " .
			     "or RRR format\n" ); 
		}
		$format = uc( $2 );
		if( $format ne "IMS1.0" &&
		    $format ne "IMS2.0" ) {
			die( "File $ARGV Not in IMS1.0, " .
			     "or IMS2.0 format\n" ); 
		}
		if ($type eq "ARR") {
			@n_table=@db_ARR_nuclide;
		} elsif ($type eq "RRR") {
			@n_table=@db_RRR_nuclide;
		}
		$State = "header";
		return 1;
	}
	if( $line =~ /^\s*SAMPLE INFORMATION ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "sample_information";
		return 1;
	}
	if( $line =~ /^\s*ACTIVITY SUMMARY ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "activity_summary";
		return 1;
	}
	if( $line =~ /^\s*MINIMUM DETECTABLE CONCENTRATION FOR KEY NUCLIDES ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "mdc";
		return 1;
	}
	if( $line =~ /^\s*MEASUREMENT CATEGORIZATION ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "categorization";
		return 1;
	}
	if( $line =~ /^\s*PEAK SEARCH RESULTS ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "searching";
		return 1;
	}
	if( $line =~ /^\s*PEAK SEARCH NOTES ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "searching";
		return 1;
	}
	if( $line =~ /^\s*SPECTRAL-REGION_OF_INTEREST (SROI) EDITING ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "searching";
		return 1;
	}
	if( $line =~ /^\s*PEAK SEARCH NOTES ==/i ) {
		if( $State eq "startup" ) {
			print STDERR "No DATA_TYPE line--\n";
		}
		$State = "searching";
		return 1;
	}

	return 0;
}

sub searching {
	( $line ) = @_;
	# intentional null routine
}

sub startup {
	( $line ) = @_;
	# intentional null routine
}

sub categorization {
	( $line ) = @_;
	if ( $line =~ /^\s*Spectrum Category \((\d)\) -- (.*)/i ) {
		$category=$1,$remark=$2;
	}
}
sub activity_summary {
	( $line ) = @_;
	if ( $line =~ /^\s*NATURAL RADIOACTIVITY:\s*/i ) {
	} elsif ( $line =~ /^\s*Nuclides Identified and not Quantified:\s*/i ) {
		$unquantified=1;
	} elsif ( $line =~ /^\s*Nuclides Quantified:\s*/i ) {
		$unquantified=0;
	}elsif ($unquantified) {
		@nuclides=split /,/, $line;
		foreach $nuclide (@nuclides) {
			$nuclide=~ s/^\s*//g;
			$nuclide=~ s/\s*$//g;
			eval {
		$recno=dbaddv(@n_table,"sta",$sta,"chan",$chan,"sample_id",$sample_id,
				"sample_id",$sample_id,"nuclide","$nuclide",
				"quantified","n"
			  );
			};
			if ($@) {
				$recno=undef;
				if ($@=~ /.*record #(\d+).*/) {
					$row=$1;
					$n_table[3]=$row;
					dbputv(@n_table,"nuclide",$nuclide,"quantified","n");
				}
			}
		}
	} elsif ( $line =~ /^\s*(\S+-\S+)\s+(\S+)\s+(\S+)/i ) {
		($nuclide,$halflife,$hlunits,$concentration,$relerr,$notes)=split ' ',$line,6;
		eval {
		$recno=dbaddv(@n_table,"sta",$sta,"chan",$chan,"sample_id",$sample_id,
				"nuclide","$nuclide",
				"activity",$concentration,"relerr",$relerr,"quantified","y"
			  );
		};
		if ($@) {
			$recno=undef;
			if ($@=~ /.*record #(\d+).*/) {
				$row=$1;
				$n_table[3]=$row;
				dbputv(@n_table,"nuclide","$nuclide",
				"activity",$concentration,"relerr",$relerr,"quantified","y"
				);
			}
		}

	}
}
sub mdc {
	( $line ) = @_;
	if ( $line =~ /^\s*(\D+-\d+)\s+(\S+)\s+(\S+)\s+(\S+)/i ) {
		$nuclide=$1;$halflife=$2;$hlunits=$3;$mdc=$4;
		eval {
		$recno=dbaddv(@n_table,"sta",$sta,"chan",$chan,"sample_id",$sample_id,
				"nuclide",$nuclide, "mdc",$mdc
			  );
		};
		if ($@) {
			$recno=undef;
			if ($@=~ /.*record #(\d+).*/) {
				$row=$1;
				$n_table[3]=$row;
				dbputv(@n_table,"mdc",$mdc);
			}
		}

	}
}
sub header {
	( $line ) = @_;
	if ( $line =~ /^\s*Creation Date:\s+(.*)/i ) {
		$creation_date=str2epoch($1);
	} elsif ( $line =~ /^\s*Sample Arrival Time:\s+(.*)/i ) {
		$arrival_time=str2epoch($1);
	}
}

sub sample_information {
	( $line ) = @_;
	if ( $line =~ /^\s*Station ID:\s+(\S+)\s+Detector ID:\s+(\S+)/i ) {
		$sta=uc($1);
		$stachan=uc($2);
		($s1,$chan) = split /_/,$2;
		
	} elsif ( $line =~ /^\s*Sample ID:\s+(\S+)\s+Sample geometry:\s+(\S+)/i ) {
		$sample_id=$1;$sample_geometry=$2;
	} elsif ( $line =~ /^\s*Sample Quantity:\s+(\S+)\s+(\S+)\s+Sample Type:\s+(\S+)/i ) {
		$quantity=$1;$units=$2;$sample_type=$3;
	} elsif ( $line =~ /^\s*Collection Start:\s+(\S+)\s+(\S+)\s+Sampling Time:\s+(\S+)\s+(\S+)/i ) {
		$collection_start=str2epoch("$1 $2");
		($sampling_time,$sunits)=&units_convert("$3 $4","hour");
	} elsif ( $line =~ /^\s*Collection Stop:\s+(\S+)\s+(\S+)\s+Decay Time:\s+(\S+)\s+(\S+)/i ) {
		$collection_stop=str2epoch("$1 $2");
		$collection_stop = $collection_stop - 0.001;
		($decay_time,$dunits)=&units_convert("$3 $4","hour");
	} elsif ( $line =~ /^\s*Acquisition Start:\s+(\S+)\s+(\S+)\s+Acquisition Time:\s+(\S+)\s+(\S+)/i ) {
		$acquisition_start=str2epoch("$1 $2");
		($acquisition_time,$dunits)=&units_convert("$3 $4","hour");
	} elsif ( $line =~ /^\s*Acquisition Stop:\s+(\S+)\s+(\S+)\s+Avg Flow Rate:\s+(\S+)\s+(\S+)/i ) {
		$acquisition_stop=str2epoch("$1 $2");
		$acquisition_stop = $acquisition_stop - 0.001;
		($flow_rate,$funits)=&units_convert("$3 $4","m");
	}
}




if( $#ARGV < 1 ) {
	die "Usage: ctbtrb2db filename [filename ...] dbname\n";
} else {
	$dbname = pop( @ARGV );
	@Db = dbopen( $dbname, "r+" );
	$dbfilename=dbquery(@Db,"dbDATABASE_FILENAME");
	if (! -f $dbfilename) {
		dbclose(@Db);
		dbcreate($dbname,"ctbto1.1");
		@Db = dbopen( $dbname, "r+" );
	}
	@db_ARR=dblookup(@Db,"","arr","","");
	@db_RRR=dblookup(@Db,"","rrr","","");
	@db_ARR_nuclide=dblookup(@Db,"","arr_nuclide","","");
	@db_RRR_nuclide=dblookup(@Db,"","rrr_nuclide","","");
}

init_globals;
	$category=0;
	$product_id=0;

@Db_arr= 	dblookup(@Db, "", "arr", "", "");

while( <ARGV> ) {
	chop;
	next if( /^\s*$/ );
	next if( /^\s*\(.*\)\s*$/ ); # comment
	last if ( /^STOP$/i ); #last line to be processed, avoid confusing error with ISC data	
	next if stateswitch( $_ );
	&$State( $_ );
} 

if ($type=~/ARR/i) {
	dbaddv(@db_ARR,"sta",$sta,"chan",$chan,"sample_id",$sample_id,"quantity",$quantity,
				"collection_start",$collection_start,"collection_stop",$collection_stop,
				"acquisition_start", $acquisition_start, "acquisition_stop", $acquisition_stop,
				"sample_id",$sample_id,
			  );
} elsif ($type=~/RRR/i) {
	dbaddv(@db_RRR,"sta",$sta,"chan",$chan,"sample_id",$sample_id,"quantity",$quantity,
				"collection_start",$collection_start,"collection_stop",$collection_stop,
				"acquisition_start", $acquisition_start, "acquisition_stop", $acquisition_stop,
				"sample_id",$sample_id,"category",$category,
			  );
}
dbclose (@Db);
