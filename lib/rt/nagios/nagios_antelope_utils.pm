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
#   Written By: Steve Foley 6/15/2004

package nagios_antelope_utils;

require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(&categorize_return_value
		&parse_ranges
		&print_version
		&print_results
		%ERRORS
		$TIMEOUT
		$VERBOSE);


use strict;

### PROTOTYPES
sub categorize_return_value($$$$$$$);
sub check_range($$$$);
sub parse_ranges($$);
sub print_version($$);
sub print_results;

our (%ERRORS)=('OK'=>0,'WARNING'=>1,'CRITICAL'=>2,'UNKNOWN'=>3,'DEPENDENT'=>4);
our ($TIMEOUT) = 15;
our ($VERBOSE) = 0;

######
# Parse and check warn and critical range arguments from command line
# Nagios-style ranges (see below) into a set of global variables
#
# [@]start:end
#
#Notes:
#- start > end>
#- start and ":" is not required if start=0
#- if range is of format "start:" and end is not specified,
#    assume end is infinity
#- to specify negative infinity, use "~"
#- alert is raised if metric is outside start and end range
#   (inclusive of endpoints)
#- if range starts with "@", then alert if inside this range
#   (inclusive of endpoints)
# Return "0" if there was an error parsing
sub parse_ranges($$)
{
    my ($opt_warn, $opt_crit) = @_;
    my ($warn_at, $warn_hi, $warn_low, $crit_at, $crit_hi, $crit_low);

    # Handle warning now
    if ($opt_warn =~ /^(\@?)(-?\d*\.?\d*)$/)
    {
	$warn_low = 0;
	$warn_hi = $2;
	$warn_at = $1;
    }
    else
    {
	($warn_at, $warn_low,$warn_hi) = ($opt_warn =~ /(\@?)(~|-?\d*\.?\d*):?(-?\d*\.?\d*)/);

	if (($warn_low eq "") && ($warn_hi eq ""))
	{
	    return ();
	}
	if (($warn_low ne "") && ($warn_hi ne "") && ($warn_low ne "~")
	    && ($warn_low > $warn_hi))
	{
	    return ();
	}
	if (($warn_low eq "") && ($warn_hi ne ""))
	{
	    $warn_low = 0;
	}
	if ($warn_low eq "~") { $warn_low = ""; }
    }

    # Handle critical now
    if ($opt_crit =~ /^(\@?)(-?\d*\.?\d*)$/)
    {
	$crit_low = 0;
	$crit_hi = $2;
	$crit_at = $1;
    }
    else
    {
	($crit_at, $crit_low, $crit_hi) = ($opt_crit =~ /(\@?)(~|-?\d*\.?\d*):?(-?\d*\.?\d*)/);
	if (($crit_low eq "") && ($crit_hi eq ""))
	{
	    return ();
	}

	if (($crit_low ne "") && ($crit_hi ne "") && ($crit_low ne "~")
	    && ($crit_low > $crit_hi))
	{
	    return ();
	}

	if (($crit_low eq "") && ($crit_hi ne ""))
	{
	    $crit_low = 0;
	}
	if ($crit_low eq "~") { $crit_low = ""; }
    }

    return ($warn_at, $warn_hi, $warn_low, $crit_at, $crit_hi, $crit_low);
}

######
# Check a value against the inputted warning and critical ranges.
#
# Param: Value to check
# Return: (error code from %ERRORS, Description)
sub categorize_return_value($$$$$$$)
{
    my ($return_value, $warn_at, $warn_hi, $warn_low,
	$crit_at, $crit_hi, $crit_low) = @_;

    if ($VERBOSE)
    {
	if (defined $return_value)
	{
	    print "Value being compared: $return_value\n";
	}
	if ((defined $warn_at) && (defined $warn_low) && (defined $warn_hi))
	{
	    print "Warning @/low/high: $warn_at/$warn_low/$warn_hi\n";
	}
	else
	{
	    print "Not all warning values defined!\n";
	}
	if ((defined $crit_at) && (defined $crit_low) && (defined $crit_hi))
	{
	    print "Critical @/low/high: $crit_at/$crit_low/$crit_hi\n";
	}
	else
	{
	    print "Not all critical values defined!\n";
	}
    }

    # Check to see if it is critical first
    if (check_range($return_value, $crit_at, $crit_low, $crit_hi))
    {
	return ($ERRORS{'CRITICAL'}, $return_value);
    }

    # Check to see if it is worth a warning
    if (check_range($return_value, $warn_at, $warn_low, $warn_hi))
    {
	return ($ERRORS{'WARNING'}, $return_value);
    }

    # else, return an okay
    return ($ERRORS{'OK'}, $return_value);
}

######
# Check to see if a particular value falls within a given range.
# Assumes hi and low values are not BOTH empty, but returns 1 if they are
#
# Param: $test_val - The value in question
# Param: $at - "@" or "" if checking in or out of the range (see Nagios range)
# Param: $low_val - The low value of the range, "" for negative infinity
# Param: $high_val - The high value of the range, "" for infinity
# Return: 1 if the value is in the range, 0 if not
sub check_range($$$$)
{
    my ($test_val, $at, $low_val, $high_val) = @_;

    # should already be checked before we get here, but...
    if (((!defined $low_val) && (!defined $high_val))
	|| (($low_val eq "") && ($high_val eq ""))) { return 1; }

#   print "testing val: $test_val, at: $at, low: $low_val, hi: $high_val\n"; #debug

    if ($at) # looking for inside range
    {
	if (($low_val eq "") && ($test_val < $high_val)) { return 1; }
	if (($low_val eq "") && ($test_val > $high_val)) { return 0; }
	if (($high_val eq "") && ($test_val > $low_val)) { return 1; }
	if (($high_val eq "") && ($test_val < $low_val)) { return 0; }
	if (($test_val > $low_val) && ($test_val < $high_val)) { return 1; }
	# didnt catch any, so we must be outside the range
	return 0;
    }
    else # looking for outside range
    {
	if (($low_val eq "") && ($test_val > $high_val)) { return 1; }
	if (($low_val eq "") && ($test_val < $high_val)) { return 0; }
	if (($high_val eq "") && ($test_val < $low_val)) { return 1; }
	if (($high_val eq "") && ($test_val > $low_val)) { return 0; }
	if (($test_val < $low_val) || ($test_val > $high_val)) { return 1; }
	# didnt catch any, so we must be outside the range
	return 0;
    }
}

######
# Prints the version string for a program. Includes author, version, and
# program name.
sub print_version($ $)
{
    my ($version, $author) = @_;
    print "$0 version $version\nwritten by $author\n";
}

######
# Print the results in a nagios friendly format given a result code
# and possibly some performance data.
# Param: service_name - A short name for the service to display in Nagios
# Param: result_code - A scalar from %ERRORS
# Param: result_perf - A scalar performance value that was fetched
# Param: result_descr - A short description (1-2 words) describing the
#                       performance value (ie "descr = $result_perf")
# Param: result_ext - Extended information about the result that should not be
#                       included in the perfdata
sub print_results
{
    my ($service_name, $result_code, $result_perf, $result_descr,
        $result_ext) = @_;
    my ($prefix);

    SWITCH :
    {
        if ($result_code == $ERRORS{'OK'})
        {
            $prefix = "$service_name OK:";
            last SWITCH;
        }
        if ($result_code == $ERRORS{'WARNING'})
        {
            $prefix = "$service_name WARNING:";
            last SWITCH;
        }
        if ($result_code == $ERRORS{'CRITICAL'})
        {
            $prefix = "$service_name CRITICAL:";
            last SWITCH;
        }
        if ($result_code == $ERRORS{'UNKNOWN'})
        {
            $prefix = "$service_name UNKNOWN:";
            last SWITCH;
        }
    }
    print "$prefix $result_descr = $result_perf";
    print (" " . $result_ext) if ($result_ext);
    print "|\'$result_descr\'=$result_perf\n";
}
