
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
#   Written By: Steve Foley 6/12/2007
# 

# Look through a file glob of antelope license pf files for licenses that
# have already expired or will expire soon.

use strict;
use Time::Local;
use Getopt::Std;

# Prototypes
sub usage();

# Get our command options
our %Options;
getopt('d', \%Options);

# Constants
our $VERSION = '$Revision$';
our $AUTHOR = "Steve Foley, UCSD ROADNet Project, sfoley\@ucsd.edu";
our $NOW = time;
our $DEFAULT_WINDOW = 30;   # default alert time in days

our %MonthNames = qw(
  Jan 0  Feb 1  Mar 2  Apr 3  May 4  Jun 5
  Jul 6  Aug 7  Sep 8  Oct 9 Nov 10 Dec 11
);

MAIN:
{
   my $license_time;
   my $time_diff;
   my $alert_range;
   my $expires; 
   my $file;
   my $year;
   my $month;
   my $day;

   # Set alert range
   if (defined $Options{d}) 
   { 
      $alert_range = $Options{d} * 86400; 
   } 
   else
   {
      $alert_range = $DEFAULT_WINDOW * 86400;
   }
   if ($#ARGV < 0) { &usage(); exit; }

   # open files
   foreach $file (@ARGV)
   {
      open (LICENSE_FILE, "$file");
   
      $expires = 0;

      while (<LICENSE_FILE>)
      {
         $time_diff = 0;
         ($year, $month, $day) = ($_ =~ /[a-zA-Z]\w+\s(\d{4})\s(\w{3})\s(\d+)\s/);
         if (defined $year)
         {
            $license_time = timelocal(0, 0, 0, $day, $MonthNames{$month}, $year);
            $time_diff = $license_time - $NOW;

            if ($time_diff < $alert_range)
            {
               # print a pretty header for the first expiration in a file
               if ($expires == 0) { print "*** In file \"$file\":\n"; }

               print $_;
               $expires++;
            }
         } 
      }

      close(LICENSE_FILE);
   }
}

sub usage()
{
   print "$0\nVersion: $VERSION\nAuthor: $AUTHOR\n\n";
   print "Usage: $0 [-d <days>] <files...>\n";
   print "Setting \"-d\" alerts for licenses expiring less than <days> days in the future\n";
   print "The default alert time is 30 days\n";
}

