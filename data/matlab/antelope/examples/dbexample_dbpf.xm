display('Running dbexample_dbpf')

echo on

% Get a 'handle', i.e. the pf object, with which to ask questions
%  about what's in the specified parameter file. 
%
% This example assumes that you have Antelope correctly installed, 
%  such that there is a dbloc2.pf parameter file in $ANTELOPE/data/pf,
%  and that your PFPATH environment variable was correctly set 
%  to contain $ANTELOPE/data/pf as one of the parameter-file search 
%  directories.

pf = dbpf( 'dbloc2' )

% Now as a contrived example, convert it to a string, then compile it into 
% A new parameter-file object:
string_version = pf2string( pf );

% Create an empty parameter-file object:
newpf = dbpf

% Compile the new string into the empty parameter-file object:
% (you can compile into parameter-file objects that aren't empty as well)
newpf = dbpf( newpf, string_version) 

echo off
