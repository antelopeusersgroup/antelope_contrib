
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
#   This code was created as part of the USArray ANF project.
#   See http://anf.ucsd.edu/
#
#   Written By: Geoff Davis 17-Feb-2012
#
# Based on the nagios_check_orblag plugin

use strict;
use Socket;
use Getopt::Long;
use File::Basename;
use vars qw($opt_version $opt_help $opt_verbose $opt_warn $opt_crit
            $opt_orb $opt_match $opt_reject
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
sub valid_lag($);
sub print_help();
sub print_usage($);
sub get_max_laggard();
sub check_lag($);
sub valid_client($$$);

# Constants
our $VERSION = '1.0';
our $AUTHOR = "Geoff Davis, gadavis\@ucsd.edu";
our $PROGNAME = $0;
our $NAGIOS_SERVICE_NAME = "ORBLAG";

# Defaults
our $VERBOSE = 0;
our $ORB = ":"; # orb and port, ie. "orbstat -s $ORB"
our $MATCH = '';
our $REJECT = '';

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

    my ($max_lag, $max_who) = get_max_laggard();

    # check if we got a laggard back at all
    if (!defined $max_lag)
    {
      print_results($NAGIOS_SERVICE_NAME, $ERRORS{'CRITICAL'}, 0,
        "No orblag value found");
      exit $ERRORS{'CRITICAL'};
    }

    # Print the results and exit
    ($result_code,$result_perf) = check_lag($max_lag);
    print_results($NAGIOS_SERVICE_NAME, $result_code, $result_perf,
      "orblag" );
    exit $result_code;
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
	       "m=s"   => \$opt_match,    "match=s"  => \$opt_match,
	       "r=s"   => \$opt_reject,   "reject=s" => \$opt_match,
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

    if ($opt_match)
    {
      $MATCH = $opt_match;
      print "MATCH is $MATCH\n" if $VERBOSE;
    }

    if ($opt_reject)
    {
      $REJECT = $opt_reject;
      print "REJECT is $REJECT\n" if $VERBOSE;
    }

    # Gotta have warn, crit, and source options

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
	($warn_at, $warn_high, $warn_low, $crit_at, $crit_high,
          $crit_low) = parse_ranges($opt_warn, $opt_crit);
	if ((!valid_lag($warn_at)) || (!valid_lag($warn_high))
	    || (!valid_lag($warn_low))
	    || (!valid_lag($crit_at)) || (!valid_lag($crit_high))
	    || (!valid_lag($crit_low)))
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

    print "Usage: $0 [-m <match>] [-r <reject>] "
    . "[-w <warn> -c <crit>] [-o <orb>] [-v verbose]\n";
}

######
#
sub print_help()
{
    print_version($VERSION, $AUTHOR);
    print_usage("");
    print "\n";
    print " Nagios plugin to query the specified orbserver orb for it's clients\n";
    print " relative read position (lag) in the orb\n";
    print " Check is performed from the Nagios server, not remotely\n";
    print "\n";
    print "-o  (--orb)     = The ORB to look at (addr:port, default: $ORB)\n";
    print "\n";
    print " Ranges for max lag values are between 0.00 and 1.00\n";
    print "-w  (--warn)    = Maxium lag value to trigger a warning, expressed "
        . " as a Nagios range phrase ([@][min:]max)\n";
    print "-c  (--crit)    = Max lag value to trigger a critical, expressed "
        . " as a Nagios range phrase ([@][min:]max)\n";
    print "\n";
    print "-m  (--match)\n";
    print "-r  (--reject)\n";
    print " These regular expressions are matched against the client names (as\n";
    print " shown in orbstat -c)\n";
    print "\n";
    print "-h  (--help)    = This help message\n";
    print "-V  (--version) = The version of this script\n";
    print "-v  (--verbose) = The verbosity of the output\n";
    print "\n";
}

######
# Verfies that the supplied value of $lag is between 0 and 1
# Returns 0 (false) if outside of range, 1 (true) if in range
sub valid_lag($) {
  my $lag = shift;
  if (!defined $lag)
  {
    return 0; #false
  }
  if (($lag < 0) || ($lag > 1))
  {
    return 0; #false
  }
  return 1; #true

}

######
# Get the client with the highest lag.
# Returns: an array with ($max_lag,$max_who)
sub get_max_laggard()
{
  my ($orb);
  my ($old,$new,$max,$range,@laggards); # returned by orblag()
  my ($max_lag,$max_who);
  my ($matched_clients) = 0;

  $orb = orbopen("$ORB", "r&");

  # bail early if we couldn't connect to the orb
  if ($orb < 0)
  {
    if ($VERBOSE)
    {
      print ("Could not connect to orb \"$ORB\"\n");
    }
    return undef;
  }

  ($old,$new,$max,$range,@laggards) = orblag($orb,0,0);

  if ($VERBOSE)
  {
    printf ("Requested orblag, got %s clients back\n",
      scalar(@laggards) );
  }

  orbclose($orb);

  if ( scalar(@laggards) < 1 )
  {
    return undef;
  }

# Find the user and hostname of the client with the greatest lag
  $max_lag = -1;
  $max_who = 'none';

  foreach (@laggards)
  {
    my ($lag, $thread, $pktid, $who, $what ) = split (' ', $_, 5) ;

    if ( valid_client($who,$MATCH,$REJECT) && ($lag > $max_lag) )
    {
      $matched_clients++;
      $max_lag = $lag;
      $max_who = $who;
    }
  }

  if ($VERBOSE)
  {
    print "Number of matched clients: $matched_clients\n";

    if ($matched_clients)
    {
      printf ("Biggest laggard: %s (%.2f)\n", $max_who, $max_lag);
    }
  }

  return undef unless $matched_clients;

  # return result
  return ($max_lag, $max_who);
}

######
# Check to see if the lag value is within the bounds
#
sub check_lag($)
{
  my $lag = shift;

  if ( (!defined $lag) || ($lag < 0) || ($lag > 1) )
  {
    return ($ERRORS{'CRITICAL'}, "ERROR");
  }

  # return result
  return categorize_return_value($lag, $warn_at, $warn_high,
    $warn_low, $crit_at, $crit_high, $crit_low);
}

######
# Check to see if the client is accepted by the match or reject regexs
# Evaluates matches before rejections, so if a client matches both
# $match and $reject it is considered invalid
#
# NOTE: I'm not sure how Antelope does this internally so this may need
# to be changed for consistency with orblag(1)
#
sub valid_client($$$)
{
  my ($who,$match,$reject);
  my ($is_valid);
  $who = shift;
  $match = shift;
  $reject = shift;

  print "valid_client $who , $match, $reject\n";

  $is_valid = 1; # Assume client is valid

  if ( "$match" ne '' )
  {
    $is_valid = ( $who =~ m/$match/i );
  }
  if ( $is_valid && "$reject" ne ''  )
  {
    $is_valid = ( ! ( $who =~ m/$reject/i ) );
  }

  return $is_valid;

}
# vim:ft=perl
