echo on

% This presumes that you have connect permission to a running 
%  orb called 'nordic' (you probably don't...)

fd = orbopen( 'nordic', 'r' );

% Reject all parameter-file packets, all database-row packets, 
% and all waveform packets for the Alaska net whos station-names 
% start with A,B, or C:
% (return the number of sources still available on the connection)

orbreject( fd, '/db/.*|/pf/.*|AK_[A-C].*' )

echo off
