if isempty( getenv( 'ANTELOPE' ) ) 

	display('Please set the ANTELOPE environment variable before running setup_antelope.m' );

else

	addpath( [getenv( 'ANTELOPE' ) , '/data/matlab/antelope/antelope'] );
	addpath( [getenv( 'ANTELOPE' ) , '/data/matlab/antelope/scripts'] );
	addpath( [getenv( 'ANTELOPE' ) , '/data/matlab/antelope/examples'] );

end
	



