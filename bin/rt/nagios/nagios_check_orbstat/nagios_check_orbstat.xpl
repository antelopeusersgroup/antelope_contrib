
#
# Copyright (c) 2004 The Regents of the University of California
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
#   Written By: Steve Foley 5/5/2004
# 
# Based on some code in the check_mailq program from Nagios Plugins v1.3.1

use strict;
use Socket;
use Getopt::Long;
use File::Basename;
use vars qw($opt_version $opt_help $opt_verbose $opt_warn $opt_crit $opt_orb
	    $opt_source $opt_client $opt_type $opt_index $opt_param 
	    $opt_exists 
	    $warn_at $warn_low $warn_high $crit_at $crit_low $crit_high
	    $VERBOSE $PROGNAME $STATE);


use nagios_antelope_utils qw(&categorize_return_value
			     &parse_ranges
			     &print_results
			     &print_version
		  	     %ERRORS
			     $VERBOSE);

# antelope includes
use orb;
use Datascope;
use Socket;

# Prototypes
sub check_args();
sub print_help();
sub print_usage($);
sub get_latest_orbstat_packet($);
sub check_recent_packet_exists($$$$$);
sub check_recent_pforbstat_packet_value($$$$$$$$);
sub check_client_exists($);
sub convert_pforbstat_to_numbers($);

# Constants
our $VERSION = '$Revision$';
our $AUTHOR = "Steve Foley, UCSD ROADNet Project, sfoley\@ucsd.edu";
our $PROGNAME = $0;
our $SERVER_TYPE = "server";
our $CLIENT_TYPE = "clients";
our $SOURCE_TYPE = "sources";
our $TYPE_REGEX = "$SERVER_TYPE|$CLIENT_TYPE|$SOURCE_TYPE";
our $NAGIOS_SERVICE_NAME = "ORBSTAT";

# Defaults
our $VERBOSE = 0;
our $ORB = ":"; # orb and port, ie. "orbstat -s $ORB"

######
#
MAIN:
{

    my ($result_code, $result_perf, $status);
    Getopt::Long::Configure("bundling");
    $status = check_args();
    if ($status)
    {
	print "ERROR: processing arugments\n";
	exit $ERRORS{'UNKNOWN'};
    }

    if (defined $opt_source)
    {
	my ($pktid, $source_name, $packet_time, $raw_packet, $num_bytes)
	    = get_latest_orbstat_packet($opt_source);
	
	# check if you have a packet first
	if (!defined $pktid)
	{
	    print_results($NAGIOS_SERVICE_NAME, $ERRORS{'CRITICAL'}, 0,
			  "No orbstat packet found, age");
	    exit $ERRORS{'CRITICAL'};
	}
	elsif (defined $opt_type) 
	{
	    ($result_code, $result_perf)
		= check_recent_pforbstat_packet_value($pktid, $source_name,
						      $packet_time,
						      $raw_packet, $num_bytes,
						      $opt_type, $opt_index,
						      $opt_param);
	    if ($opt_type eq $SERVER_TYPE)
	    {
		print_results($NAGIOS_SERVICE_NAME, $result_code, $result_perf,
			      "$opt_type:$opt_param");
	    }
	    else
	    {
		print_results($NAGIOS_SERVICE_NAME, $result_code, $result_perf,
			      "$opt_type:$opt_index:$opt_param");
	    }
	    exit $result_code;
	}
	elsif (defined $opt_exists)
	{
	    ($result_code, $result_perf) 
		= check_recent_packet_exists($pktid, $source_name, 
					     $packet_time, 
					     $raw_packet, $num_bytes);
	    # pretty print if it isnt an error
	    if ($result_code != $ERRORS{'UNKNOWN'})
	    {
		print_results($NAGIOS_SERVICE_NAME, $result_code, 
			      strtdelta($result_perf), 
			      "last packet age");
	    }
	    else
	    {
		print_results($NAGIOS_SERVICE_NAME, $result_code, 
			      $result_perf, "last packet age");	    
	    }
	    exit $result_code;
	}
	else
	{
	    exit $ERRORS{'UNKNOWN'};
	}
    }
    elsif (defined $opt_client)
    {
	($result_code, $result_perf) = check_client_exists($opt_client);
	# pretty print if it isnt an error
	if ($result_code != $ERRORS{'UNKNOWN'})
	{
	    print_results($NAGIOS_SERVICE_NAME, $result_code, 
			  strtdelta($result_perf), 
			  "latency");
	}
	else
	{
	    print_results($NAGIOS_SERVICE_NAME, $result_code, 
			  $result_perf, "latency");	    
	}
	exit $result_code;
    }
    else
    {
	print "ERROR: No client or source defined\n";
	exit $ERRORS{'UNKNOWN'};
    }
}

######
# Check the arguments supplied 
sub check_args()
{
    my ($fetching_params) = 0;

    GetOptions("V"     => \$opt_version,  "version"  => \$opt_version,
	       "v"     => \$opt_verbose,  "verbose"  => \$opt_verbose,
	       "h"     => \$opt_help,     "help"     => \$opt_help,
	       "w=s"   => \$opt_warn,     "warn=s"   => \$opt_warn,
	       "c=s"   => \$opt_crit,     "crit=s"   => \$opt_crit,
	       "o=s"   => \$opt_orb,      "orb=s"    => \$opt_orb,
	       "s=s"   => \$opt_source,   "source=s" => \$opt_source,
	       "u=s"   => \$opt_client,   "client=s" => \$opt_client,
	       "e"     => \$opt_exists,   "exists"   => \$opt_exists,
	       "t=s"   => \$opt_type,     "type=s"   => \$opt_type,
	       "i=s"   => \$opt_index,    "index=s"  => \$opt_index,
	       "p=s"   => \$opt_param,    "param=s"  => \$opt_param
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
	$nagios_antelope_utils::VERBOSE = 1;
    }
    
    if ($opt_help)
    {
	print_help();
	exit $ERRORS{'OK'};
    }
    
    if ($opt_orb)
    {
	$ORB = $opt_orb;
    }
    
    # Gotta have warn, crit, and source options
    if ((!defined $opt_source) && (!defined $opt_client))
    {
	print_usage("No source or client specified!");
	exit $ERRORS{'UNKNOWN'};
    }

    # When looking for a source, we must have 
    # either "exists" or "type/index/param" info
    if (defined $opt_source)
    {
	if (defined $opt_type)
	{
	    # Yeah, its a clunky set of IFs, but it reads clearer than a biggie
	    if ($opt_type !~ /$TYPE_REGEX/)
	    {
		print_usage("Invalid type specified! " .
			    "Must match regex: $TYPE_REGEX");
		exit $ERRORS{'UNKNOWN'};	
	    }	
	    
	    # server doesnt need an index...there is only one server!
	    if (($opt_type ne $SERVER_TYPE) && (!defined $opt_index))
	    {
		print_usage("Missing index!");
		exit $ERRORS{'UNKNOWN'};	
	    }	
	    
	    # 
	    if (!defined $opt_param)
	    {
		print_usage("Missing Parameter");
		exit $ERRORS{'UNKNOWN'};		    
	    }
	}
	elsif (!defined $opt_exists) # no type, so must exist or an error
	{
	    print_usage("No type or exist flag specified!");
	    exit $ERRORS{'UNKNOWN'};	
	}
    }

    if ($opt_source)
    {
	$opt_source = &convert_pforbstat_to_numbers($opt_source);
    }

    # Either both defined or neither of them
    if (((!defined $opt_warn) && (defined $opt_crit))
	|| ((defined $opt_warn) && (!defined $opt_crit)))
    {
	print_usage("Warn and Crit must be defined or not defined as a pair.");
	exit $ERRORS{'UNKNOWN'};
    }

    # sanity check critical and warn flags
    if ((defined $opt_warn) && (defined $opt_crit))
    {
	($warn_at, $warn_high, $warn_low, $crit_at, $crit_high, $crit_low) =
	    parse_ranges($opt_warn, $opt_crit);
	if ((!defined $warn_at) || (!defined $warn_high) 
	    || (!defined $warn_low)
	    || (!defined $crit_at) || (!defined $crit_high) 
	    || (!defined $crit_low))
	{
	    print "Error in threshold ranges!\n";
	    exit $ERRORS{'UNKNOWN'};
	}
    }
}

######
# Print the usage for the command
# Param: Explanation for the problem, can be ""
sub print_usage($)
{
    my ($explanation) = shift;

    if (defined $explanation)
    {
	print "$explanation\n";
    }

    print "Usage: $0 (-u <client> | "
	. "(-s <src> (-e | (-t <type> (-i <index>) -p <param>)))) "
	. "[-w <warn> -c <crit>] [-o <orb>] [-v verbose]\n";
}

######
#
sub print_help()
{
    print_version($VERSION, $AUTHOR);
    print_usage("");
    print "\n";
    print " Check on ORB status values for the source at the specified ORB\n";
    print " Check is performed from the Nagios server, not remotely\n";
    print "\n";
    print "-u  (--client)  = The client identifier to look for\n";
    print "-s  (--source)  = The source description to fetch\n";
    print "-e  (--exists)  = Just check that the source exists and is\n";
    print "                  fresher than -w and -c (in minutes)\n";
    print "-o  (--orb)     = The ORB to look at (addr:port, default: $ORB)\n";
    print "-w  (--warn)    = Nagios range phrase ([@][min:]max) to " 
	. "trigger a warning\n";
    print "-c  (--crit)    = Nagios range phrase ([@][min:]max) to "
	. "trigger a critical\n";
    print "\n";
    print "Defining -t, -i and -p will attempt to parse a pforbstat packet.\n";
    print "-t  (--type)    = Type of the object to check\n";
    print "                  (one of \"$CLIENT_TYPE\",
 \"$SOURCE_TYPE\", \"$SERVER_TYPE\")\n";
    print "-i  (--index)   = The index of the object, specific to the type\n";
    print "                  (\"$CLIENT_TYPE\" requires a number,\n";
    print "                   \"$SERVER_TYPE\" ignores index,\n";
    print "                   \"$SOURCE_TYPE\" requires a source name/type)\n";
    print "-p  (--param)   = Parameter name to monitor\n";
    print "\n";
    print "-h  (--help)    = This help message\n";
    print "-V  (--version) = The version of this script\n";
    print "-v  (--verbose) = The verbosity of the output\n";
    print "\n";
}

######
# Get the latest orbstat packet for the given source. This involves
# opening the orb connection, getting a packet, closing the connection,
# and returning the packet.
#
# Param: source name
# Returns: orbstat packet contents and metadata 
#   ($pktid, $srcname, $time, $packet, $num_bytes) 
#   or null if no packet collected
sub get_latest_orbstat_packet($)
{
    my ($source_name) = shift;
    my ($orb);
    my ($pktid, $srcname, $time, $packet, $num_bytes);

    $orb = orbopen("$ORB", "r&");
    
    orbselect($orb, "$source_name");

    ($pktid, $srcname, $time, $packet, $num_bytes) =
    orbget($orb, "ORBNEWEST");

    if ($VERBOSE)
    {
	print "Requested source: $source_name, returned source: $srcname\n";
    }

    orbclose($orb);

    if ((defined $pktid) && ($source_name eq $srcname))
    {
	return ($pktid, $srcname, $time, $packet, $num_bytes);
    }
    else
    {
	return undef;
    }
}

######
# Check to see if the argument packet is within the recent window.
# Param: orbstat packet id, source name, time, packet, and num_bytes
#   (As returned by get_latest_orbstat_packet())
# Return: (error code from %ERRORS, Description)
sub check_recent_packet_exists($ $ $ $ $)
{
    my ($pktid, $source_name, $packet_time, $raw_packet, $num_bytes) = @_;
    my ($current_time, $time_difference); 
    my ($result, $packet);

    if ((!defined $pktid) || (!defined $source_name) ||
	(!defined $packet_time) || (!defined $raw_packet) ||
	(!defined $num_bytes))
    {
	return ($ERRORS{'CRITICAL'}, "ERROR");
    }

    # get current time
    $current_time = now();
   
    # compare with packet time
    $time_difference = $current_time - $packet_time;

    # return result
    return categorize_return_value($time_difference, $warn_at, $warn_high, 
				   $warn_low, $crit_at, $crit_high, $crit_low);
}

######
# Check to see if the value of the argument in the packet
# is within the recent window.
# Param: orbstat packet id, source name, time, packet, and num_bytes, 
#                  type, index, param
#   (As returned by get_latest_orbstat_packet())
# Return: (error code from %ERRORS, Description)
sub check_recent_pforbstat_packet_value($ $ $ $ $ $ $ $)
{
    my ($pktid, $source_name, $packet_time, $raw_packet, $num_bytes,
	$type, $index, $param) = @_;
    my ($packet_value, $pf, $pf_type, $pf_index); 
    my ($result, $packet);

    if ((!defined $pktid) || (!defined $source_name) ||
	(!defined $packet_time) || (!defined $raw_packet) ||
	(!defined $num_bytes) || (!defined $type) || (!defined $param))
    {
	return ($ERRORS{'CRITICAL'}, "ERROR");
    }
    
    if ($type !~ /$TYPE_REGEX/)
    {
	return ($ERRORS{'CRITICAL'}, "ERROR");
    }
    # verify packet really is an orbstat packet  
    unless (($result, $packet) = unstuffPkt($source_name, $packet_time, 
					  $raw_packet, $num_bytes))
    {
 	return ($ERRORS{'CRITICAL'}, "BAD PACKET");
    }

    if (!defined $packet)
    {
 	return ($ERRORS{'CRITICAL'}, "NO PACKET");
    }

    my ($packet_type, $packet_desc) = $packet->PacketType();

    if ($packet_type eq 'Pkt_pf')
    {
	return ($ERRORS{'CRITICAL'}, "BAD PACKET");
    }
    
    # get packet value
    $pf = $packet->pf;
    foreach $pf_type (pfget($pf, $type))
    {
	if (!defined $pf_type) # check sanity of type
	{
	    return ($ERRORS{'CRITICAL'}, "Type not found in packet");
	}
	if ($type eq $SERVER_TYPE)
	{
	    $packet_value = ${$pf_type}{$param};
	}
	else # go one layer deeper
	{
	    if (!defined ${$pf_type}{$index})
	    {
		return ($ERRORS{'CRITICAL'}, 
			"Index not found in type");		
	    }
	    $packet_value = ${$pf_type}{$index}{$param};
	}
    }
    # return result
    if (!defined $packet_value)
    {
	return ($ERRORS{'CRITICAL'}, "NO VALUE");
    }

    if ((defined $opt_warn) && (defined $opt_crit))
    {
	return categorize_return_value($packet_value, $warn_at, $warn_high, 
				       $warn_low, $crit_at, $crit_high, 
				       $crit_low);
    }
    else
    {
	return ($ERRORS{'OK'}, $packet_value);
    }
}

######
# Checks to see that a client program is running and what its latency is
# Param: The client program name
# Return: ($result_code, $result_perf)
#  result_code - result code to return to Nagios (from %ERRORS)
#  result_perf - Performance value (latency in this case) for Nagios.
sub check_client_exists($)
{
    my ($client_name) = shift;
    my ($orb, $client, $what_base, $packet_value);
    my ($when, @clients, $found_client);

    $orb = orbopen("$ORB", "r&");

    if ($orb < 0)
    {
	return ($ERRORS{'UNKNOWN'},"0");
    }

    ($when, @clients) = orbclients($orb);
    foreach $client (@clients)
    {
        ($what_base) = split /\s/, $client->what;
        $what_base = basename($what_base);

    # pick out the matches and return data
        if ($opt_client eq $what_base)
        {
            $found_client = 1;
            if (($client->lastpkt) <= 0)
            {
                return($ERRORS{'UNKNOWN'},"No lastpkt");
            }
            # Fix this to a latency!
            $packet_value = now() - $client->lastpkt;
            last;
        }
    }

    orbclose($orb);

    if (!$found_client)
    {
        return($ERRORS{'UNKNOWN'},"No client");
    }

    if ((defined $opt_warn) && (defined $opt_crit))
    {
	return categorize_return_value($packet_value, $warn_at, $warn_high, 
				       $warn_low, $crit_at, $crit_high,
				       $crit_low);
    }
    else
    {
	return ($ERRORS{'OK'}, $packet_value);
    }

}

######
# Converts a pforbsta source request including a name into a number, but
#   ONLY for pf/orbstat packets. All others just get echoed back normally.
# Param: The source being requested, possibly in the format:
#   mercali:roadnet/pf/orbstat
#   igpprt.ucsd.edu:anza/pf/orbstat
#   roadnet-orb:6580/pf/orbstat
#   1.2.3.4:6510/EXP/FOO
# Return: A source that can be used.
#   1.2.3.4:1234 (All numeric IP and port)
sub convert_pforbstat_to_numbers($)
{
    my ($orig_source) = shift;
    my ($orig_addr, $orig_port, $orig_chan);
    my ($new_addr, $new_port, $return_source);
    
    if (($orig_addr, $orig_port, $orig_chan) = 
	($orig_source =~ /(.*):(.+?)(\/.*)/))
    {
	if ($VERBOSE)
	{
	    print "Original source string: [$orig_source]\n";
	    print "Original source address, port, channel: "
		. "$orig_addr, $orig_port, $orig_chan\n";
	}

	# See if it is even worth translating	
	if ($orig_chan eq "/pf/orbstat")
	{
	    
	    # Look up the address
	    $new_addr = inet_ntoa(inet_aton($orig_addr));
	    
	    # Look up the port
	    if ($orig_port =~ /\D+/)
	    {
		$new_port = pfget("orbserver_names", $orig_port);
	    }
	    else
	    {
		$new_port = $orig_port;
	    }

	    $return_source = "$new_addr:$new_port$orig_chan";
	    
	    if ($VERBOSE)
	    {
		print "Translated source address, port, channel: "
		    . "$new_addr, $new_port, $orig_chan\n";
		print "Translated source string: [$return_source]\n";
	    }
	    return $return_source;
	}
    }

    # all other cases wind up here
    return $orig_source;
}
