#!/opt/antelope/perl/bin/perl

use File::Basename;

my $filename = basename($0);
my $orblookupdir = dirname($0);  

# The location where the file containing orblook's pid for this orb will be
# for example, "/var/run/orblookup"
my $pidfileloc = "/tmp/.orblookup";
my $pidfile;

# The location for the log for this orb
# for example, "/var/log/orblookup" 
my $logfileloc = "/tmp/.orblookup";

# End customizations

my $orbname;

sub usage {
    print "usage: $filename [start] [orbname] [interval]\n";
    print "       $filename [restart] [orbname]\n";
    print "       $filename [stop] [orbname]\n";
    exit -1;
}

sub stoporb{
    ($orbname) = @_;
    if ( ! -e $pidfile ) {
        print "orblookup on $orbname not running.\n";
        exit -1;
    }
    open PF, "< $pidfile";
    my $pid = <PF>;
    close PF;
    unlink $pidfile;
    kill 2, $pid
        or die "Couldn't kill $pid: $!";
    print "orblookup on $orbname stopped.\n";
}

sub startorb{
    $orbname = $_[0];
    my $interval  = $_[1];

    open PF, "> $pidfile";
    print PF "$$\n";
    close PF;
    $interval = $interval * 60;

    &runorb( $orbname, $interval ); 
}

sub checkdir{
   my ($dirname) = @_;

   if ( ! ( -e $dirname ) )
   {
      mkdir $dirname;
      if ( ! ( -e $dirname ) )
      {
          die "Could not create directory $dirname";
      }
   }
   elsif ( ! -d $dirname )
   {
       die "orblookup cannot write to $dirname: not a directory";
   }
   elsif ( ! -w $dirname )
   {
       die "orblookup cannot write to $dirname: check permissions";
   }
}

sub checkargs{
    my @arglist = @_;
    my $numargs = $#arglist;
    my $command = $arglist[0];

    if ( ( $numargs eq "2" ) && ( ( $command eq "start") || 
                                ( $command eq "restart" ) ) )
    {
        return;
    }

    if ( ( $numargs eq "1" ) && ( $command eq "stop" ) )
    {
        return;
    }
    &usage;
}

sub choose_options {
    my ( $command, $orbname, $interval ) = @_;
    my $numargs = $#_;
    $pidfile = "$pidfileloc/$orbname.pid";

    if ( $command eq "start" ) {
        if ( $numargs ne 2 ) {
            &usage;
        }
        if ( -e $pidfile ) {
            print "$pidfile exists.  Is another instance already running this orb?\n";
            exit;
        }

        print "Starting orblookup on $orbname at an interval of $interval\n";
        &startorb( $orbname, $interval );
    }
    elsif ( $command eq "restart" ) {
        if ( $numargs ne 2 ) {
            &usage;
        }
        &stoporb ($orbname);
        print "Restarting orblookup on $orbname at an interval of $interval\n";
        &startorb( $orbname, $interval );
    }
    elsif ( $command eq "stop" ) {
        if ( $numargs ne 1 ) {
            &usage;
        }
        &stoporb ($orbname);
    }
    else {
        &usage;
    }
     
}

sub runorb {
    my ( $orbname, $sectime ) = @_;
    my $cmd = "$orblookupdir/orblookupd";
    my $logfile = "$logfileloc/$orbname.log";

    while ( ) {
        $starttime = time;
    
        open LOG, ">> $logfile"
            or die "Cannot create logfile: $!";

        # Run the orblookup program.
        $results = `$cmd $orbname 2>&1`;
        print LOG "$results";

        # Calculate the time to sleep in seconds until next interval.
        $endtime = time;
        $sleeptime = $sectime - $endtime + $starttime;

        print LOG "Sleeping for $sleeptime seconds.";
        print LOG "\n";
        close LOG;

        sleep $sleeptime;
    }   
}

sub interrupt_handler {
    stoporb($orbname);
    unlink "$oldir/$orbname.pid";
    exit -1;
}

$SIG{'TERM'} = 'interrupt_handler';
$SIG{'INT'} = 'interrupt_handler';
$SIG{'HUP'} = 'interrupt_handler';
#$SIG{'QUIT'} = 'interrupt_handler';

&checkargs( @ARGV );
&checkdir( $pidfileloc );
&checkdir( $logfileloc );
&choose_options( @ARGV );
