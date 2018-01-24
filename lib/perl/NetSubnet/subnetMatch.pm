# Simple Perl module for determining whether
# an IP address is in a given set of subnets.
#
#   AUTHOR: Juan Reyes <reyes@ucsd.edu>
#
#
#   USAGE:
#
#       my @subnets =  qw(
#                 10.10.10.10/255.255.255.255
#                 10.22.22.0/255.255.255.0
#                 2.2.2.2
#                 15.15.15.15/24
#       );
#
#       my $match_nets = subnet_match( @subnets );
#
#       print $match_nets->($ip) ? 'yes' : 'no', "\n";
#

package subnetMatch;

use strict;
use Socket;

use base 'Exporter';
our @EXPORT = qw(subnet_match);


our $VERSION = '1';

sub cidr{
    return pack("N", 0xffffffff << 32 - shift );
}

sub subnet_match {
    @_ > 1 and goto &multi_matcher;

    my ($net, $mask) = split m[/], shift;
    return matcher($net, $mask);
}

sub matcher {
    my ($net, $mask) = @_;

    $mask ||= 32;
    $net = inet_aton($net);
    $mask = $mask =~ /\./ ? inet_aton($mask) : cidr($mask);
    my $masked_net = $net & $mask;

    return sub { ((inet_aton(shift) || return !1) & $mask) eq $masked_net };
}

sub multi_matcher {
    my @v4 = map subnet_match($_), @_;

    return sub {
        $_->($_[0]) and return 1 for @v4;
        return !!0;
    }
}

1;

__END__
