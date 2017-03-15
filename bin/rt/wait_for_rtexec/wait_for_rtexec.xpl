# This code was created as part of the USArray ANF Project.
# See http://anf.ucsd.edu/
#
# Author: Geoff Davis 15-March-2017
# License: BSD 2-clause
use warnings;
use strict;
use Getopt::Long;
use Pod::Usage;
use File::Basename;
use Datascope; # for elog and friends

our ($opt_version, $opt_debug, $rtsystem, $pid_file);

our $VERSION  = '1.0';
our $AUTHOR   = 'Geoff Davis, gadavis@ucsd.edu';
our $PROGNAME = 'wait_for_rexec';

our $PID_FILE_BASE = 'logs/rtexec.pid';

$opt_verbose = 0;

sub sig_handler {
    our $pid_file;
    our $opt_debug;
    my ( $fh, $pid, $pid_running );

    print "Caught interrupt\n";

    # ignore further interrupts
    $SIG{'INT'} = 'IGNORE';

    open( $fh, '<', $pid_file ) or elog_die "open ${pid_file}: $!";
    ( $pid ) = <$fh>; chomp $pid;
    close $fh;

    $pid_running = kill (0, $pid);

    if ( $pid_running ) {
        my $sleep_interval = 5;
        my $message_every = 12;
        my $message_interval = $sleep_interval * $message_every;
        my $waited_for = 0;

        elog_notify("Waiting for PID %s, check interval %ss",
        $pid, $sleep_interval);
        while (kill(0, $pid)) {
            sleep($sleep_interval);
            $waited_for += $sleep_interval;
            elog_notify(
                "PID ${pid} still running after ${waited_for} seconds")
            if ( $waited_for % $message_interval == 0 );
        }

        elog_notify("Waited for ${waited_for} seconds before process terminated.");

    } else {
        elog_notify("rtexec PID ${pid} was not running");
    }
    exit(0);
}

sub init {
    our ($opt_debug, $opt_verbose, $PROGNAME, $PID_FILE_BASE, $rtsystem,
        $pid_file);
    elog_init($PROGNAME);
    elog_debug("Starting ${PROGNAME}");

    # Parse options or die with usage message if they're bad
    GetOptions(
        'debug'   => \$opt_debug,
    ) or pod2usage(2);

    # Parse the rtsystem directory name, die if it's missing or bad
    pod2usage({
            -message => 'no real-time system directory specified',
            -exitval => 2,
        }) unless ( defined($ARGV[0]) && $ARGV[0] ne '' );
    $rtsystem = $ARGV[0];

    # Generate the path to the pid_file for rtexec
    $pid_file = join('/', $rtsystem, $PID_FILE_BASE);

    elog_die("Can't find PID file ${pid_file}!") unless (-f $pid_file);
    elog_die("PID file ${pid_file} can't be empty") unless (-s $pid_file);

    # If we get here, our arguments are good, and we can run.
    elog_debug('Initialization complete');
}

# Sleep indefinitely until signaled
sub run_forever {
    our $opt_debug;
    elog_notify("Will monitor rtexec $pid_file before stopping");
    # Install our signal handler
    if ($opt_debug) {
        elog_notify("Debug mode activated - will wait for rtexec on CTRL-C");
        $SIG{INT} = \&sig_handler;
    }

    $SIG{TERM} = \&sig_handler;
    elog_notify("Press CTRL-C to terminate");
    sleep();
}

init();
run_forever();

################ END ###############
__END__

=head1 NAME

wait_for_rtexec - Wait for an rtxec process to terminate

=head1 SYNOPSIS

wait_for_rtexec [-d|--debug] /path/to/real-time-system

=head1 OPTIONS

=over 8
=item B<-d  (--debug)>

Places program into debug mode. In this mode, if CTRL-C is pressed or it receives a SIGINT, the process will start it's shutdown routine. Normally, the process doesn't start it's shutdown routine unless it receives a SIGTERM

=back

=head1 DESCRIPTION

B<wait_for_rtexec> will monitor an rtexec instance and terminate when that
rtexec instance terminates. It's designed to act as a service dependency
between different rtexec instances.

The process will loop indefinitely until it receives a SIGTERM. At that point,
it will look for an rtexec(1) process associated with the given path, and wait
for that process to terminate. If no rtexec is currently running, it will exit
cleanly.

The use case for this program is for running processes in two separate Antelope
real-time systems, where the processes in the second real-time system depend on
a process in the first. For example, you might set up a real-time system named
I<admin> containing one or more dbids(1) processes. Then a second real-time
system named I<writers> containing the database writing processes (orb2db(1),
orb2dbt(1), orb2wf(1) is set up.

A setup like this allows a subset of database writes to be stopped
independently of dbids(1).

Under this split rtexec instances design, the rtexec I<Shutdown_order>
parameter is normally useless, since there is no way for for the I<admin>
rtexec(1) to know about the various processes in the I<writers> rtexec
instance.

However, you can use B<wait_for_rtexec> as a stand-in for those processes that
can't be monitored directly by the containing rtexec instance.

=head1 USAGE IN RTEXEC

B<wait_for_rtexec> is designed to be run under a containing rtexec instance,
and watch a supervised rtexec instance. In order for it to work as intended,
you'll need to modify the I<Shutdown_order> parameter in the containing
I<rtexec.pf>.

You'll also need to tweak the I<Time_to_die> parameter to a value larger than
the normal shutdown time for the supervised rtexec(1) instance. So if your
supervised rtexec(1) takes 3 minutes to shutdown, you'll want to set
I<Time_to_die> to something like 190 seconds (adding in an extra 10 seconds or
so of "slop" time).
