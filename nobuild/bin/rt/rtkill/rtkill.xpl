#
#   Copyright (c) 2007 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Ruth Rutter, Lindquist Consulting, Inc. 
#
#   This software is licensed under the New BSD license: 
#
#   Redistribution and use in source and binary forms,
#   with or without modification, are permitted provided
#   that the following conditions are met:
#   
#   * Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the
#   following disclaimer.
#   
#   * Redistributions in binary form must reproduce the
#   above copyright notice, this list of conditions and
#   the following disclaimer in the documentation and/or
#   other materials provided with the distribution.
#   
#   * Neither the name of Lindquist Consulting, Inc. nor
#   the names of its contributors may be used to endorse
#   or promote products derived from this software without
#   specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
#   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
#   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#

use Datascope;
use File::Copy;
use Getopt::Std;

sub retrieve_run_status {
	my( $key ) = @_;

	my( $run_val ) = pfget_boolean( $Pfname, "Run{$key}" );

	return $run_val;
}

sub inform {
	my( $msg ) = @_;

	if( $opt_v ) {
		
		elog_notify( "$msg\n" );
	}

	return;
}

sub set_run_status {
	my( $key, $newval ) = @_;
	
	my( $success ) = copy( $Pfname, $Tempfile );

	if( ! $success ) {
		
		elog_die( "Failed to create temporary copy '$Tempfile'\n" );
	}

	open( F, $Tempfile );
	my( @slurp ) = <F>;
	close(F);

	my( @result ) = ();

	while( $line = shift( @slurp ) ){

		if( $line =~ /^$key(\s+)([01tfyn]|yes|no|on|off|ok|true|false)\s*$/i ){

			push( @result,  "$key" . "$1" . "$newval\n" );

		}else{

			push( @result, $line );
		}
	}

	open( F, ">$Tempfile" );
	print F @result;
	close(F);

	system( "mv $Tempfile $Pfname" ); 

	return;
}

sub print_run_array {

	my( @keys ) = sort( keys(%Run) );

	print "\nAvailable tasks are:\n\n";

	foreach $key (@keys) {

		my( $onoff );
		
		if( retrieve_run_status( $key ) ) {
			
			$onoff = "on";

		} else {

			$onoff = "off";
		}
		
		print sprintf( "\t%-20s\t%s\n", $key, $onoff );
	}

	print "\n";

	return;
}
			
$usage = "Usage: rtkill [-rvlqs] [procname]";

$Program = $0;
$Program =~ s/.*\///;

elog_init( $Program, @ARGV);

if( ! getopts('rvlqs') ) {

	die( "$usage\n" );

} elsif( $opt_l ) {

	if( @ARGV != 0 ) {

		elog_complain( "Useless specification of a process" . 
				"name with -l option\n" );
	}

} elsif( @ARGV != 1 ) {

	die( "$usage\n" );

}else {

	$procname = shift(@ARGV);
}

$Pfname = "rtexec.pf";
$Tempfile = "$Pfname-";
$Restart_sleep_time_sec = pfget( "rtkill", "restart_sleep_time_sec" );

if( !-e "$Pfname" ){

	die( "Can't find ./$Pfname. Bye.\n" );
}

%Run = %{ pfget( $Pfname, "Run" ) };

if( ! defined( $Run{$procname} ) && ! $opt_l ) {

	elog_die( "Can't find process '$procname' in Run array of '$Pfname.' Bye.\n" );
}

if( $opt_r ) {

	$run_status = retrieve_run_status( $procname );

	if( $run_status == 0 && $opt_v ) {

		$reply = askyn( "Process '$procname' is already off. Start? (y/n) " );

		if( $reply == 0 ) {

			inform( "Leaving '$procname' untouched" );

			exit(0);
		}

	} elsif( $run_status != 0 ) {

		set_run_status( $procname, 0 );

		inform( "Process '$procname' turned off..." );

		sleep( $Restart_sleep_time_sec );
	}

	set_run_status( $procname, 1 );

	inform( "Process '$procname' restarted" );

} elsif( $opt_s ) {

	$run_status = retrieve_run_status( $procname );

	if( $run_status != 0 ) {

		inform( "Process '$procname' is already turned on" );

		exit(0);
	}

	set_run_status( $procname, 1 );

	inform( "Process '$procname' turned on" );

} elsif( $opt_q ) {

	$run_status = retrieve_run_status( $procname );

	if( $run_status == 0 ) {

		inform( "Process '$procname' is turned off" );

	} else {

		inform( "Process '$procname' is turned on" );

	}

	exit( $run_status );

} elsif( $opt_l ) {
	
	print_run_array();

} else {

	$run_status = retrieve_run_status( $procname );

	if( $run_status == 0 ) {

		inform( "Process '$procname' is already turned off" );

	} else {

		set_run_status( $procname, 0 );

		inform( "Process '$procname' turned off" );
	}
}

exit(0);
