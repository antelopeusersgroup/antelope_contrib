ATM_prefix = [getenv( 'ANTELOPE' ), '/data/matlab/', version( '-release' ), '/antelope/'];

if isempty( getenv( 'ANTELOPE' ) ) 

	display('Please set the ANTELOPE environment variable before running setup_antelope.m' );

else

	addpath( [ATM_prefix , 'antelope'] );
	addpath( [ATM_prefix , 'scripts'] );
	addpath( [ATM_prefix , 'examples'] );
	addpath( [ATM_prefix , 'user'] );

end
	
clear ATM_prefix;
