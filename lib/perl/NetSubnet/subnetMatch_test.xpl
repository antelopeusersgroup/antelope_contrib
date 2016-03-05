# Simple example and test code for the Perl module
# subnetMatch included in this repo.
#
# AUTHOR:   Juan Reyes <reyes@ucsd.edu>
#

use strict;
use warnings;

use lib "$ENV{ANTELOPE}/contrib/data/perl" ;
use subnetMatch;

my @subnets =  qw(
          10.10.10.10/255.255.255.255
          10.22.22.0/255.255.255.0
          10.96.0.0/255.255.0.0
          136.0.0.0/255.0.0.0
          99.99.99.99
          111.111.111.0/24
);

print "\nUse these networks for test:\n";
for ( @subnets ) { print "$_\n"; }

# Init match object
my $match_nets = subnet_match( @subnets );



# Some random numbers to test
my @test_ips =  qw(
          111.111.111.9
          99.99.99.99
          10.10.10.10
          10.10.10.9
          10.16.10.10
          10.22.0.0
          10.22.22.10
          136.96.10.10
          10.96.10.10
          136.10
          trash
          []
);

for my $ip  ( @test_ips) {

    print "\n";
    print "========================\n";
    print "Verify ip: $ip => " , $match_nets->($ip) ? 'yes' : 'no', "\n";

}
