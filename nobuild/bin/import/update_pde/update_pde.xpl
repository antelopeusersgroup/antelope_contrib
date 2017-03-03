
#This program is used to grab the pde format pde monthly
#files from the site $remote_host and leave it in the directory
#$pde_dir. This program will check the update times of files
#on the remote site and compare that to the files in $pde_store.
#If there are files to update they will be grabbed via the 
#Net:Ftp module.  ]

#If an origin has been updated (normally not the case with the
# PDE bulletin), then the modified origin will be added to the
# origin table keeping the old event as well.


#here are the modules we are going to be using .. 
#Net::Ping to see if the machine is accessable (no longer works)
#LWP to access the web , and see if the server is up
#Datascope of course 

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope;
use File::Basename;
#use Net::Ping;
use Net::FTP;
use LWP::Simple;
use Cwd;

require "flush.pl";
require "css30.pl";
use Getopt::Std;
require "pf2.pl";

    if (! getopts('Vvy:p:') || @ARGV != 0 ) {
        &usage;
    }

    &usage unless ($opt_y) ;



    $Pf = "update_pde"  || $opt_p  ;

    if ( $Pf =~ /\.pf/) {
	print STDERR "\n Do not use the 'pf' extension when specifying the paramter file. \n"; 
        &usage;
    }

    $Pf = $Pf . ".pf" ;		# need .pf for eval_pf call`

    &eval_pf( $Pf ) ;

    if ($opt_y !~ /\d{4}/) {		
	print STDERR "-y must be followed by a four-digit year.\n";
	&usage;
    } else {
	$iwant	= "\*".$opt_y."\*";
        print STDERR "I want to get a dir listing of $iwant\n";
    }

    $mail_to = join(" ",@mail_to);
    print STDERR "mail_to is @mail_to \n" if $opt_v;

    &setup_global_vars;

# if directories from setup_global_vars dont exist you need to make them 
    &mk_pde_dir($pde_dir);
    &mk_store_dir($pde_store);
    &mk_temp_dir($pde_temp);

    open (MAILLOG, ">$mail_log") || die "Can't open $mail_log \n";
    &log_mesg("Starting getter...\n");

# This check is now irrelevant because you can no longer ping remote host.  

    if(&server_up){
     &log_mesg("Server [$remote_host] is up\n");

# check for updated files on remote server
# if new files exist on remote site, retreive them and continue,
# else exit now and don't update the local pde_YYYY   catalog,
# convert retreived file to css format
# check the new origin file for start/end times

     $ftp = Net::FTP->new("$remote_host")  or die "Can't connect: $@ \n" ;
     $ftp->login("anonymous",$account)	or die "Couldn't login \n";
     $ftp->cwd("$remote_dir")		or die "Couldn't change directory\n";
     $ftp->binary;
     @listing = $ftp->dir("$iwant") 	or die "Couldn't get file listing on remote machine\n";
     &check_remote_list;	# check to see if remote site has newer file than local pde file 
				# and retreive it
     $ftp->quit;	
     &log_mesg("Done with ftp. \n");
     
     $num2convert	= @convert_list;
   
# quit if there are no files to work on
     if ($num2convert == 0) {
 	&log_prob_and_die("No files to update.  Exiting... \n");
     }

     $c_cnt	= 0;
     while ($c_cnt <  $num2convert) {
        $file2convert = $convert_list[$c_cnt];
        &log_mesg("Going to run convert file for $file2convert. \n");

        # Need to strip .dat from $file2convert so that new_css can be of the form ehdf199907
        ($basename,$ext) = split (/\./,$file2convert); # remove extension with split
        $base		= basename($basename);
        $new_css 	= $pde_temp.$base;
       
        print STDERR	"$pde2tables $pde_store$base.$ext $new_css -auth $author \n" if $opt_v;

        if (! -e $new_css) {
            &log_mesg("No descriptor file for $new_css, creating. \n");
	    dbcreate("$new_css","css3.0","$pde_temp\{$base}");
        }

        &log_mesg("Running $pde2tables for $pde_store$base.$ext. \n");
        $output	= `$pde2tables $pde_store$base.$ext $new_css -auth $author \n`;

        if ($output ne "") {
            &log_mesg("pde2origin ERROR for $base: \n $output \n");
            return;
        } 

        &log_mesg("Zipping $pde_store$base.$ext. \n");

# hard-wired path to gzip
	&safe_exec("/usr/bin/gzip $pde_store$base.$ext");
        # check db for start/end times
        @db2add		= dbopen($new_css,"r");
        @db2add_origin	= dblookup(@db2add,"","origin","","");

        #get min and max times of oriigns to be added
        $min_otime		=dbex_eval(@db2add_origin, "min(time)");  
        $max_otime		=dbex_eval(@db2add_origin, "max(time)");  

	print STDERR "\nLooking to add origins from:", &strydtime($min_otime), "to:", &strydtime($max_otime), " \n\n" if ($opt_v || $opt_V);

	@pde		= dbopen($pde_catalog,"r+");
	@pde_or		= dblookup(@pde,"","origin","","");
	$pde_recs	= dbquery (@pde_or,"dbRECORD_COUNT");

	print STDERR "Number of records in current pde bulletin: $pde_recs\n" if ($opt_v || $opt_V);

	@dbcheck	= dblookup(@pde_or,"","origin","","dbSCRATCH");

	$nrows	= dbquery (@db2add_origin,"dbRECORD_COUNT");
	print STDERR "Number of records that might be added: $nrows \n" if ($opt_v || $opt_V);

	for ($row = 0; $row < $nrows; $row++) {
	    $db2add_origin[3] = $row ;
	    $record = dbget(@db2add_origin);
	    print "Record is: $record\n" if $opt_V;
	    @or_record = split(//,$record);
	    dbput(@dbcheck,$record);

# find any matching rows in existing pde bulletin
	    @matchall = dbmatches(@dbcheck,@pde_or, "chkor","lat","lon","time","depth","ml","mb","ms","ndef","evid");

	    if ($#matchall != -1) { 	# Found a matching record, no need to update

		print STDERR "Found matching record with no updates needed\n" if ($opt_v || $opt_V) ;
		next;
	    } else {			#Either record is completely new, or you need to update a
					# pre-existing pde record if there has been a change to 
					# lat/lon/time/depth/mag
		print STDERR "All fields of this record do not match.\n" if ($opt_v || $opt_V) ;

#		@matchsome = dbmatches(@dbcheck,@pde_or,"chkor2","evid");
		@matchsome = dbmatches(@dbcheck,@pde_or,"chkor2");
		if ($#matchsome != -1) {	# Need to update values to pre-existing event
		    print STDERR "Updating fields for an event\n" if ($opt_v ||$opt_V);
#		    $pde_or[3] = $matchsome[0];

		    dbput(@pde_or,$record);
		} else {	# we have a new record
		    print STDERR "Adding new record to origin table for an event\n" if ($opt_v || $opt_V);
		    eval { dbadd(@pde_or,$record) } ;
		    if ($@) {
			warn $@;
			print STDERR "Duplicate origin matches pre-existing origin.  Will ignore\n" if ($opt_v || $opt_V);
		    }
		}
	    }
	}
        dbclose @db2add;

	print STDERR "Number of records in earlier pde bulletin: $pde_recs\n" if ($opt_v || $opt_V);
	$nrows2	= dbquery(@pde_or,"dbRECORD_COUNT");
	print STDERR "Number of records in final pde bulletin: $nrows2 \n" if ($opt_v || $opt_V);
	
	
        dbclose @pde;

        ++$c_cnt;
        }

    } else {
        &log_mesg("Server [$remote_host] is down.\n");
    }

    &tidy_up;

# Here are all the subroutines

sub usage {
    print STDERR <<END;
	\nUSAGE: \t$0 [-v] [-V] -p pffile -y year 

END
	exit(1);
}

sub setup_global_vars{

    ($thismonth)			= (localtime)[4];
    ($thisyear)				= (localtime)[5];
 # change thismonth from 0-11 to 1-12 convention
    $thismonth		= $thismonth + 1;

 # EDIT THESE VARIABLES
    $pde2tables          = "$ENV{ANTELOPE}/bin/pde2origin";  #path to pde2origin 

    print STDERR "Account is $account from pf $Pf. \n" if $opt_v;

# The remote_host variable should be set in the default pf file.  
# Use this value if remote_host is not defined in pf
    if (!$remote_host) { 
        $remote_host         = "hazards.cr.usgs.gov";
    }

# I have hard wired this...  Hopefully USGS doesn't reorganize without warning.
    $remote_dir	        = "pde/";

 # LOCAL STORAGE DIRECTORIES AND LOGS (modify if needed)
    $pde_store           = $pde_dir.$opt_y."_HDF_monthly/";
    $pde_temp            = $pde_dir."temp/";
    $temp_pde		= $pde_temp."temp_pde";
    $pde_catalog	= $pde_dir."pde_$opt_y";		
    $log_file            = $pde_dir."pde_monthly.log";
    $mail_log		= $pde_dir."pde_mail.log";
    $opened_log          = 0;
    $recs_b4		= 0;
    $recs_after		= 0;
    $recs_removed	= 0;
    $table		= ".origin";
    @convert_list	= ();
    
}

sub safe_exec {		# run system cmds safely
    my ( $cmd ) = @_ ;
#    &log_mesg ("$cmd \n") ;
    print STDERR "safe_exec cmd is $cmd \n" if $opt_v;
    system ( $cmd ) ;
    if ($?) {
	print STDERR "$cmd error $? \n"  if $opt_v ;
	&log_prob_and_die("$cmd error $? \n");
#	print STDERR "$cmd error $? \n";
        &bad_exit();
    }
}        

sub bad_exit {
    my $cmd;
    print MAILLOG "Check errors in $mail_log and $log_file \n";
    close (MAILLOG) ;
    $cmd = "mailx -s \"PROBLEMS - $0 \" $mail_to < $mail_log";
    print STDERR "$cmd \n" if $opt_v;
    if ($mail_to) {
        &safe_exec($cmd) ;
        if ($?) {
	    print STDERR "$cmd error $? \n";
	    &log_mesg ("$cmd error $? \n");
        }
    }
    exit(1);
}
 
sub mk_temp_dir {	# mk pde temp dir
    my ( $pde_temp ) = @_ ;
    my ( $mkdir );


    if (-W $pde_temp) {
        return;
    } else {
        &log_mesg("mk_temp_dir $pde_temp\n") ;
	$mkdir = "mkdir -p $pde_temp";
	&safe_exec( $mkdir );
        return;
    }
}

sub mk_store_dir {	# mk pde store dir
    my ( $pde_store ) = @_ ;
    my ( $mkdir );


    if (-W $pde_store) {
        return;
    } else {
        &log_mesg("mk_store_dir $pde_store \n");
	$mkdir = "mkdir -p $pde_store";
	&safe_exec( $mkdir );
        return;
    }
}

sub mk_pde_dir {	# mk pde dir
    my ( $pde_dir ) = @_ ;
    my ( $mkdir );

# check to see if pde_dir exists and that it is writable (cant use log_mesg because log does not exist)
    if (-W $pde_dir) {
        return;
    } else {
        print STDERR "Default pde dir does not exist, creating.\n" if $opt_v;
	$mkdir = "mkdir -p $pde_dir";
        print STDERR "Going to run safe_exec for $mkdir \n" if $opt_v;
	&safe_exec( $mkdir );
        return;
    }
}

sub server_up{

# This check has been phased out as over the years 
# pingecho, Net:Ping, and unix ping have all been blocked.
# Now, I just assume that the system is up and attempt to 
# get files I want...  

#    $timeout = 10;
# Going to use safe_exec to run unix ping.
#
#    $pingme = "/usr/sbin/ping $remote_host";
#    &log_mesg("$pingme\n");
#    $ping_out = system( $pingme );	# ping returns 0 if machine is up, non-zero if problem
#
    $ping_out = 0;

    unless ($ping_out) {
	&log_mesg("Assuming sys is alive, so set server_up to 1.\n");
	return 1;
    } else {
	# Until they allow ping-ing again, you'll never get here without
	# some code re-write.
	&log_mesg("sys is dead, so set server_up to 0.\n");
	return 0;
    }
}

sub log_mesg{

    my($log_prob) = $_[0];

    if(!$opened_log){
        open(LOGFILE, ">>$log_file") || 
            (print STDERR "CAN'T OPEN LOGFILE\n"); 
        $opened_log = 1;
    }

    #set up the time in a nice format for output to the log file
    $prob_time = substr(strtime(time),0,-4);

    print LOGFILE "$prob_time\t$log_prob";
    flush(LOGFILE);
}

sub tidy_up{
    my $cmd;

    &log_mesg("Cleaning up $pde_temp\n");
    
    $rm_origin = "/usr/bin/rm -r $pde_temp*.origin";
#   system("\rm -r $pde_temp/*.origin");     
    &safe_exec ($rm_origin);
    &log_mesg("Exiting $0\n");
    print MAILLOG "Finished with $0.  Verify that catalog looks ok.\n";
    print MAILLOG "\n Files retreived from $remote_host include: @convert_list.\n";
    close(MAILLOG);
    $cmd = "mailx -s \"PDE monthly catalog updated \" $mail_to < $mail_log";
    if ($mail_to) {
        system ($cmd) ;
        if ($?) {
            print STDERR "$cmd error $? \n";
        }
    }
    close(LOGFILE);
    exit(0);
}

sub check_remote_list {

    foreach (@listing) {
	(@foo)	= split(/\s+/, $_);
        if ($foo[0] =~ /total/) {
            next;
        } elsif ($foo[8] =~ /ehdf/) {
	    $file	= $foo[8]  or die "Couldn't find file name for @foo \n"; 
	    $upd_mo	= $foo[5];
	    $upd_dy	= $foo[6];
	    $upd_t_yr	= $foo[7];

# looks like it will be year if time is gt 6 months previous to now, time if less than 6 mos.

	    if (length $upd_t_yr != 4) {

# get the abbreviated month names mapped to 1-12 numeric months

		    %mo_names = (
			    Jan => 1,
			    Feb => 2,
			    Mar => 3,
			    Apr => 4,
			    May => 5,
			    Jun => 6,
			    Jul => 7,
			    Aug => 8,
			    Sep => 9, 
			    Oct => 10,
			    Nov => 11,
			    Dec => 12,
		    );

                $num_mo = $mo_names{$upd_mo};

                if ($num_mo <= $thismonth) {
		    $upd_yr	= $thisyear;	
                } else {       
		    --$upd_yr;
                }
# need to get abreviated month converted to 1-12 and current month converted to 1-12.
# if ab_month >1 and < cur_month than set upd_yr = this_yr
# else set upd_yr = this_yr - 1

		$upd_time	= $upd_t_yr;	
	    } else {
		$upd_yr		= $upd_t_yr;	
		$upd_time	= "00:00:00";	
	    }
# get last update time of remote file (this could be replaced by an ftp->mdtm if permissions allow) 

	   $last_remote_update	= str2epoch("$upd_mo $upd_dy $upd_yr $upd_time") || die "Couldn't find last update time of $file \n";
           print STDERR "file is $file \n" if $opt_v;
#	   $last_remote_update	= $ftp->mdtm( $test_run )  || die "Couldn't get modify time on remote $file \n";
#	   print STDERR "$last_remote_update \n" || die "Can't get last_remote_update time.\n" ;

	   $gzipfile	= $pde_store.$file.".gz";
	   if (-e $pde_store.$file) {		# ADD GZIP FILE CHECK HERE
		$local_update	= (stat("$pde_store$file"))[9]  || die "Couldn't find last update time for $pde_store$file \n";
		&log_mesg("Local update for $file: $local_update.  Remote update for $file: $last_remote_update\n");    
		if ($local_update < $last_remote_update) {
		    &get_remote_file;
		    next;
		} else {
		    &log_mesg("No change to file $file. \n");
		}
	   } elsif (-e $gzipfile) {
		$local_update	= (stat("$gzipfile"))[9]  || die "Couldn't find last update time for $gzipfile \n";
		if ($local_update < $last_remote_update) {
		    &get_remote_file;
		    next;
		} else {
		    &log_mesg("No change to file $file. \n");
		}
	   } else {
		&log_mesg("File $file does not exist locally, grabbing it. \n");
		&get_remote_file;
		next;
	   }
	} else {
          &log_mesg("no ehdf match for $foo[8] \n") ;
          next;
        }
    }
}

sub get_remote_file {

    &log_mesg("Retreiving file $file from $remote_host .\n");
    $ftp->get($file,$pde_store.$file)  || die "Can't retreive file $file from $remote_host or store it as $pde_store.$file.\n";
    @convert_list = (@convert_list, $pde_store.$file);
}


sub log_prob_and_die{

    my($problem) = $_[0];

    &log_mesg($problem);
    print MAILLOG "Check errors in $mail_log and $log_file \n";
    print MAILLOG "\n\t $problem \n";
    close MAILLOG ;
    print STDERR "$problem \n";
    $cmd = "mailx -s \"PROBLEMS - $0 \" $mail_to < $mail_log" ;
    if ($mail_to) {
        system($cmd);
        if ($?) {
            print STDERR "$cmd error $? \n";
        }
    }
    exit(1);
        
#    die $problem;
}


