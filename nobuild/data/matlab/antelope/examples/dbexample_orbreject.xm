display('Running dbexample_orbreject')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

% Reject all parameter-file packets, all database-row packets, 
% and all waveform packets for the Alaska net whos station-names 
% start with A,B, or C:
% (return the number of sources still available on the connection)

orbreject( fd, '/db/.*|/pf/.*|AK_[A-C].*' )

echo off
