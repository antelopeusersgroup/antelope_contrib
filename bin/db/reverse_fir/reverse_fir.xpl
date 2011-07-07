use Getopt::Std ;

use Datascope ;

sub die_n_weep {
    die ( "Usage: $0 [-nv] [-p pffile] stage_dir\n" );
} #die

 
if ( ! getopts('nvp:') || @ARGV != 1 ) { 
    &die_n_weep(); 
}

# main

$stagedir = shift;
$opt_v = 1 if ($opt_n);
&die_n_weep() if ( ! -d "$stagedir" );

$pf=($opt_p) ? $opt_p : "reverse_fir";
$listref= pfget($pf,"filters");
@list = @$listref;
print "no. of filters: " . @list . "\n" if ($opt_v);

foreach $filter_name (@list) {
    if ($filter_name !~ /.*M$/ ) {
	print "  The filter $filter_name did not end with M. Is it really minimum phase / asymmetrical?\n";
    }

    open (FLT, "<$stagedir/$filter_name") or die "Could not open <$stagedir/$filter_name for reading - $!\n";            
    @filter = <FLT>;   # read in the filter
    close FLT;

    if ($filter[0] =~ /reversed/ ) {        # check if already reversed
	die "Is the filter $filter_name already reversed?\nIf not get rid of the \"reversed\" in its first line and rerun.\n";
    }

    if ($opt_v) {                           # print out some info for the curious
	 print "reversing coefficients for filter $filter_name\n";
    }
    
    undef @filter_out ;

    # leave a remark about reversing the Quanterra  FIR filter file, that can be checked for
    ($filter_out[0] = $filter[0]) =~ s/\# (.*) Quanterra/\# reversed $1 Quanterra/;

    $nlin = 1;
    $ncoef = 1;
    while ( ($filter[$nlin] =~ /^\#|^\s*\n/)  ||                # header with comments or empty lines
	    ($filter[$nlin] !~ /^\#/ && $#fir_check != 4) ) {   # needs to have exactly 5 fields, see response(5)
#	($filter[$nlin] !~ /theoretical|measured/) { # could just not match /theoretical|measured/
	    $filter_out[$nlin] = $filter[$nlin];                 
	    $nlin++;  
	    @fir_check = split ( '\s+', $filter[$nlin] );       # num. coef && check if really a fir filter
    }

    if ($fir_check[3] !~ /fir/ ) {   
	die "Is the filter $filter_name really a FIR filter?\nIts response type is not \"fir\".\n";
    }

    $info = $nlin +3;          # messy!
    while ( $nlin < $info) {   # theoretical, sampling decimal factor, num. of coef.
	 $filter_out[$nlin] = $filter[$nlin]; 
	 $nlin++;
    }

    while ($nlin <= ($#filter-1)){               # body, ie, filter coef, just nominators!
	 $filter_out[$nlin] = $filter[$#filter-$ncoef];     # invert it
	 $ncoef++;
	 $nlin++;
    }
    
    $filter_out[$nlin] = $filter[$#filter];  # last line, number of denominator coeff., ie, 0

    if ($opt_n) {
	$fh = STDOUT;
    }
    else {
	print "copying $stagedir/$filter_name to $stagedir/$filter_name.old\n" if ($opt_v);
	$cmd = "cp $stagedir/$filter_name $stagedir/$filter_name.old";  # make backup copies
	system( $cmd );
	$fh = NEWFLT;
	open ($fh, ">$stagedir/$filter_name") or die "Could not open <$stagedir/$filter_name for writing - $!\n";
    }

    for ($i=0 ; $i <= $#filter ; $i++) {
	print $fh $filter_out[$i];
    }

    close $fh unless ($opt_n) ;
}













