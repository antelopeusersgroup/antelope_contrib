
$makerfile = $ARGV[0];
$pdffile = $ARGV[1];

$xpdffile = $pdffile;
$xpdffile =~ s/pdf$/xpdf/;

$cmd = "fmbatch fmbatch_script";
print "$cmd\n";
$status = system( $cmd );

if( $status == 0 ) {

	#Successful: save a copy for distribution
	$cmd = "/bin/cp $pdffile $xpdffile";
	print "$cmd\n";
	system( $cmd );

	$exitcode = 0;

} elsif( -e "$xpdffile" ) {

	printf STDERR "Framemaker fmbatch command not available or failed: resorting to backup pdf file\n";

	$cmd = "/bin/cp $xpdffile $pdffile";
	print "$cmd\n";
	system( $cmd );

	$exitcode = 0;
	
} else {

	printf STDERR 
 	 "Framemaker fmbatch command not available or failed, and backup-copy pdf file is gone\n";
	$exitcode = 1;
}

exit $exitcode;
