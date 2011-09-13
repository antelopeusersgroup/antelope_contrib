
unlink( "correct/rtrmatlab" );

if( system( "localmake_config matlab" ) ) { 

	system( "cp correct/rtrmatlab_disabled correct/rtrmatlab" );

	exit( 0 );

} elsif( ! -e "$ENV{'ANTELOPE'}/setup.m" ) {

	system( "cp correct/rtrmatlab_blank correct/rtrmatlab" );

	exit( 0 );
} 

system( "cp correct/rtrmatlab_enabled correct/rtrmatlab" );

system( "rtrun_matlab" );

exit( 0 );
