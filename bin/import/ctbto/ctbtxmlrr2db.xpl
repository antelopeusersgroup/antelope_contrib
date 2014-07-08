use feature ":5.10" ; 
use strict ; 
use warnings ;

use Getopt::Std ;
 
our($opt_n, $opt_v) ;
if ( ! &getopts('nv') || @ARGV != 2 ) { 
    my $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    die ( "Usage: $pgm [-nv] file database\n" ) ; 
}

use Datascope ;
use XML::Parser;

my  $fname=shift;
my $dbname=shift;

our @Db = dbopen( $dbname, "r+" );
my $dbfilename=dbquery(@Db,"dbDATABASE_FILENAME");
if (! -f $dbfilename) {
	dbclose(@Db);
	dbcreate($dbname,"ctbto1.1");
	@Db = dbopen( $dbname, "r+" );
}
our @db_ARR=dblookup(@Db,"","arr","","");
our @db_RRR=dblookup(@Db,"","rrr","","");
our @db_ARR_nuclide=dblookup(@Db,"","arr_nuclide","","");
our @db_RRR_nuclide=dblookup(@Db,"","rrr_nuclide","","");


our $x;
$x->{"field"}="";
$x->{"MeasurementType"}="unk";

our $n;
$n->{"field"}="";

our $tmps;

#my $obj= new XML::Parser(Style => "Debug",
my $obj= new XML::Parser;
$obj->setHandlers(
			Start => \&start_element,
	   		End => \&end_element,
			Char => \&data_element	
		);


$obj->parsefile($fname);


sub start_element {
	my( $me, $element, %attrs ) = @_;
	if ($element eq "StationCode") {
		$x->{"field"}="code";
	} elsif ($element eq "DetectorCode") {
		$x->{"field"}="det_code";
	} elsif ($element eq "SampleID") {
		$x->{"field"}="SampleID";
	} elsif ($element eq "AcquisitionStart") {
		$x->{"field"}="AcquisitionStart";
	} elsif ($element eq "AcquisitionStop") {
		$x->{"field"}="AcquisitionStop";
	} elsif ($element eq "CollectionStart") {
		$x->{"field"}="CollectionStart";
	} elsif ($element eq "CollectionStop") {
		$x->{"field"}="CollectionStop";
	} elsif ($element eq "AirVolume") {
		$x->{"field"}="AirVolume";
		$x->{"AirVolumeUnits"}=$attrs{"unit"};
	} elsif ($element eq "XeVolume") {
		$x->{"field"}="XeVolume";
		$x->{"XeVolumeUnits"}=$attrs{"unit"};
	} elsif ($element eq "SpectrumGroup") {
		;
	} elsif ($element eq "IdedNuclides") {
		$n->{"field"}="search";
	} elsif ($element eq "Nuclide") {
		if ($attrs{"quantifiable"} && $attrs{"quantifiable"} eq "true") {
			
			if (( $attrs{"method"} &&  $attrs{"method"} eq "Peak Fit Method")|| (!$attrs{"method"})) {
				$x->{"field"}="nuclide";
			}
		}
	} elsif ($element eq "Name") {
		if ($x->{"field"} eq "nuclide") {
			$n->{"field"}="Name";
		}
	} elsif ($element eq "Activity") {
		if ($x->{"field"} eq "nuclide") {
			$n->{"field"}="Activity";
		}
	} elsif ($element eq "Concentration") {
		if ($x->{"field"} eq "nuclide") {
			$n->{"field"}="Concentration";
		}
	} elsif ($element eq "RelativeActivityError") {
		if ($x->{"field"} eq "nuclide") {
			$n->{"field"}="RelativeActivityError";
		}
	} elsif ($element eq "RelativeConcentrationError") {
		if ($x->{"field"} eq "nuclide") {
			$n->{"field"}="RelativeConcentrationError";
		}
	} elsif ($element eq "MDC") {
		if ($x->{"field"} eq "nuclide") {
			$n->{"field"}="MDC";
		}
	} elsif ($element eq "NuclideIdentificationIndicator") {
		if ($x->{"field"} eq "nuclide") {
			if ($attrs{"numericVal"} > 0) {
				$n->{"quantified"}="y";
			} else {
				$n->{"quantified"}="n";
			}
		}
	} elsif ($element eq "MeasurementType") {
		$x->{"field"} = "MeasurementType";
	}
}
sub data_element {
	my( $me, $data ) = @_;
	if ($x->{"field"} eq "code") {
		$x->{"code"} = $data;
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "det_code") {
		$x->{"detector_code"} = $data;
		my ($s1,$chan)=split /_/,$data;
		$x->{"chan"} = $chan;
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "SampleID") {
		$x->{"SampleID"} = $data if (!defined($x->{"SampleID"}));
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "AcquisitionStart") {
		$tmps=$data;
		$tmps =~ s/T/ /;
		$x->{"AcquisitionStart"} = str2epoch($tmps);
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "AcquisitionStop") {
		$tmps=$data;
		$tmps =~ s/T/ /;
		$x->{"AcquisitionStop"} = str2epoch($tmps);
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "CollectionStart") {
		if ($data ne "N/A") {
			$tmps=$data;
			$tmps =~ s/T/ /;
			$x->{"CollectionStart"} = str2epoch($tmps);
		}
		$x->{"field"} = "";
		
	} elsif ($x->{"field"} eq "CollectionStop") {
		if ($data ne "N/A") {
			$tmps=$data;
			$tmps =~ s/T/ /;
			$x->{"CollectionStop"} = str2epoch($tmps);
		}
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "AirVolume") {
		#$tmps=sprintf("%s %s",$data,$x->{"AirVolumeUnits"}); 
		#my ($airvolume,$funits)=&units_convert($tmps, "m3");
		$x->{"AirVolume"} = $data;
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "XeVolume") {
		#$tmps=sprintf("%s %s",$data,$x->{"AirVolumeUnits"}); 
		#my ($airvolume,$funits)=&units_convert($tmps, "m3");
		$x->{"XeVolume"} = $data;
		$x->{"field"} = "";
	} elsif ($x->{"field"} eq "AcquisitionStart") {
		$x->{"AcquisitionStart"} = $data;
		$x->{"field"} = "";
	} elsif ($n->{"field"} eq "Name") {
		$n->{"nuclide"} = $data;
		$n->{"field"} = "";
	} elsif ($n->{"field"} eq "Activity") {
		$n->{"Activity"} = $data * 1000.0;
		$n->{"field"} = "";
	} elsif ($n->{"field"} eq "Concentration") {
		$n->{"Concentration"} = $data * 1000.0;
		$n->{"field"} = "";
	} elsif ($n->{"field"} eq "RelativeActivityError") {
		$n->{"RelativeActivityError"} = $data;
		$n->{"field"} = "";
	} elsif ($n->{"field"} eq "RelativeConcentrationError") {
		$n->{"RelativeConcentrationError"} = $data;
		$n->{"field"} = "";
	} elsif ($n->{"field"} eq "MDC") {
		if ($data eq "N/A") {
			$n->{"MDC"} = -89999999999;
		} else {
			$n->{"MDC"} = $data * 1000.0;
		}
		$n->{"field"} = "";
	} elsif ($x->{"field"} eq "MeasurementType") {
		$x->{"MeasurementType"} = $data;
		$x->{"field"} = "";
	}
}
sub end_element {
	my( $me, $element ) = @_;
	if ($element eq "Nuclide") {
		#if ($x->{"MeasurementType"} eq "S") {
		my $recno=dbaddv(@db_ARR_nuclide, "sta",$x->{"code"},"chan",$x->{"chan"},"sample_id",$x->{"SampleID"},
				"nuclide",$n->{"nuclide"}, "activity",$n->{"Concentration"},
				"mdc",$n->{"MDC"},"relerr",$n->{"RelativeConcentrationError"},"quantified",$n->{"quantified"},
			  );
		$x->{"field"}="";

	} elsif ($element eq "SpectrumGroup") {
		#this is the right moment to add general info to the database
		if ($x->{"MeasurementType"} eq "S") {
		dbaddv(@db_ARR,"sta",$x->{"code"},"chan",$x->{"chan"},"sample_id",$x->{"SampleID"},"quantity",$x->{"AirVolume"},
				"collection_start",$x->{"CollectionStart"},"collection_stop",$x->{"CollectionStop"},
				"acquisition_start", $x->{"AcquisitionStart"}, "acquisition_stop", $x->{"AcquisitionStop"},
				"sample_id",$x->{"SampleID"});
		}
		#$x->{"MeasurementType"} = "unk";
	}	
}

