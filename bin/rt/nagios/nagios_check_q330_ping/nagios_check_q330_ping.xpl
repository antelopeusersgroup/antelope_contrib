
# Copyright (c) 2005 The Regents of the University of California
# All Rights Reserved
#
# Permission to use, copy, modify and distribute any part of this software for
# educational, research and non-profit purposes, without fee, and without a
# written agreement is hereby granted, provided that the above copyright
# notice, this paragraph and the following three paragraphs appear in all
# copies.
#
# Those desiring to incorporate this software into commercial products or use
# for commercial purposes should contact the Technology Transfer Office,
# University of California, San Diego, 9500 Gilman Drive, La Jolla, CA
# 92093-0910, Ph: (858) 534-5815.
#
# IN NO EVENT SHALL THE UNIVESITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
# LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE, EVEN IF THE UNIVERSITY
# OF CALIFORNIA HAS BEEN ADIVSED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE SOFTWARE PROVIDED HEREIN IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
# CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
# ENHANCEMENTS, OR MODIFICATIONS.  THE UNIVERSITY OF CALIFORNIA MAKES NO
# REPRESENTATIONS AND EXTENDS NO WARRANTIES OF ANY KIND, EITHER IMPLIED OR
# EXPRESS, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, OR THAT THE USE OF THE
# SOFTWARE WILL NOT INFRINGE ANY PATENT, TRADEMARK OR OTHER RIGHTS.
#
#   This code was created as part of the ROADNet project.
#   See http://roadnet.ucsd.edu/
#
#   Written By: Chris Gross 7/2005

use integer;
use Socket;
use Getopt::Long;
use Q330pkt qw(qpkt_hdr_stuff qpkt_hdr_unstuff);
use vars qw($opt_version $opt_help $opt_verbose $opt_warn $opt_crit
            $opt_host $opt_num $VERBOSE);
use nagios_antelope_utils qw(&categorize_return_value
			     &parse_ranges
			     &print_version
			     &print_results
			     %ERRORS
			     $VERBOSE);
our $AUTHOR = "Chris Gross, UCSD ROADNet Project, cgross\@ucsd.edu";
our $VERSION = '$Revision$';
#our $PROGNAME = $0;
our $DEFAULT_NUM = 10;
our $SPECIAL_FUNCTIONS_PORT = 5331;
our $DOMAIN = PF_INET;
our $TYPE = SOCK_DGRAM;
our $PROTO = getprotobyname('udp');
our $NAGIOS_SERVICE_NAME = "QDPPING";
our $TIMEOUT = 8;

### PROTOTYPES
sub ping();
sub check_args();
sub print_help();
sub print_usage();

MAIN:
{
   my ($result_code, $result_perf);
   Getopt::Long::Configure("bundling"); 
   check_args();

   #if we return then everything checked out

   $result_code = ping();

   # Catch the odd case here 
   if( ($result_code > 100 ) ){

	print_results($NAGIOS_SERVICE_NAME,$ERRORS{'UNKNOWN'},$result_code,"% sucessful pings");
	exit $ERRORS{'UNKNOWN'};
   }

   ($result_code,$result_perf) = categorize_return_value($result_code, $warn_at, $warn_hi, $warn_low, $crit_at, $crit_hi, $crit_low );

   # so we have a good result so lets categorize it and return
   print_results($NAGIOS_SERVICE_NAME, $result_code, 
		$result_perf, "% ping packet loss");       

   exit $result_code;

} 


###Actual ping subroutine, sends and waits for pings
#
sub ping()
{
    my $counter = 0;
    my $packet = undef;
    my $rec = undef;
    my $flag = 0;   
 
    #open the connection
    socket(TARGET, $DOMAIN, $TYPE, $PROTO) || die "Can't create socket!";

    $port = 0;
    $sockaddr = sockaddr_in( $port, INADDR_ANY );
    bind(TARGET, $sockaddr) || die "Can't bind socket!";
    
    $port = $SPECIAL_FUNCTIONS_PORT;
    $ip_addr = inet_aton($opt_host);
    $connectaddr = sockaddr_in($port, $ip_addr);
    connect(TARGET,$connectaddr) || die "Can't connect!";

    #create the packet
    $command = 0x38;
    $payload = pack("n2",0,15);
    $ack = 0;

    $packet = qpkt_hdr_stuff($command,$ack,$payload);
    
    $SIG{ ALRM } = sub { $flag = 1; };

    #send here, the required number of times
    for( $i = 1; $i <= $opt_num; $i++)  
    {
	alarm $TIMEOUT;
	if( $VERBOSE ){	
		print "Sending ping packet $i to $opt_host\n";
	}
	send(TARGET,$packet,0);
        recv(TARGET,$rec,18,0);
	if( !$flag ){
		if( $VERBOSE ){
			print "Received Packet $i from $opt_host\n";
		}
		@unpack = qpkt_hdr_unstuff($rec);
 		@unpack2 = unpack("n2",$unpack[7]);
	
		if( ($unpack[2] == 0x38 ) && ($unpack2[0] == 1) ){
 		 	$counter++;
		} 
	
		$flag = 0;
	}

   }


    close(TARGET);

    return (($opt_num-$counter)/$opt_num)*100;
}

###prints the usage message to the console
#
sub print_usage()
{
   print "Usage: $0 [-v] [-h] -H <Q330 to ping> -n <# of pings> -w warn -c crit\n";
}

###check the arguments supplied
sub check_args()
{
    my ($fetching_params) = 0;

    GetOptions("V"     => \$opt_version,  "version"  => \$opt_version,
               "v"     => \$opt_verbose,  "verbose"  => \$opt_verbose,
               "h"     => \$opt_help,     "help"     => \$opt_help,
               "w=s"   => \$opt_warn,     "warn=s"   => \$opt_warn,
               "c=s"   => \$opt_crit,     "crit=s"   => \$opt_crit,
               "H=s"   => \$opt_host,     "host=s"   => \$opt_host,
	       "n=s"   => \$opt_num,      "number=s" => \$opt_num
               );
    # handle options here
    if ($opt_version)
    {
        print_version($VERSION, $AUTHOR);
        exit $ERRORS{'OK'};
    }

    if ($opt_verbose)
    {
        $VERBOSE = 1;
    }

    if ($opt_help)
    {
        print_help();
        exit $ERRORS{'OK'};
    }

    if (!defined $opt_num)
    {
	$opt_num = $DEFAULT_NUM;
    }

    # Gotta have warn, crit
    if ((!defined $opt_warn) || (!defined $opt_crit))
    {
        print_usage();
        exit $ERRORS{'UNKNOWN'};
    }

    # Make sure our source is a pforbstat source
    if (!defined $opt_host)
    {
	print "Must specify at Target to ping!\n";
	exit $ERRORS{'UNKNOWN'};
    }
     # Deal with our ranges
    ($warn_at, $warn_hi, $warn_low, $crit_at, $crit_hi, $crit_low) =
	parse_ranges($opt_warn, $opt_crit);
    if ((!defined $warn_at) || (!defined $warn_hi) || (!defined $warn_low)
	|| (!defined $crit_at) || (!defined $crit_hi)
	|| (!defined $crit_low))
    {
	print "Error in threshold ranges!\n";
	exit $ERRORS{'UNKNOWN'};
    }

}

###prints the Help for the script
#
sub print_help()
{
   print_version($VERSION,$AUTHOR);
   print_usage();
   print "\n";
   print "Check on the uptime of an orb based on a pforbstat packet coming";
   print " back from it. Use the warn and crit options to set thresholds";
   print " for alerting for recent downtime.";
   print "\n";
   print "-n  (--number)  = The number of packets to send (default: 10)\n";
   print "-H  (--host)    = The address of the Q330 to ping (addr)\n";
   print "-w  (--warn)    = Nagios % packet loss range ([@][min:]max) to "
       . "trigger warning\n";
   print "-c  (--crit)    = Nagios % packet loss range ([@][min:]max) to "
       . "trigger critical\n";
   print "-h  (--help)    = This help message\n";
   print "-V  (--version) = The version of this script\n";
   print "-v  (--verbose) = The verbosity of the output\n";
   print "\n";
}
