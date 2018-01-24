use lib "$ENV{ANTELOPE}/data/perl" ;

#This program is used to grab the pde format qed weekly
#files from the site $remote_host and leave it in the directory
#$qed_dir. This program will check the update times of files
#on the remote site and compare that to the files in $qed_store.
#If there are files to update they will be grabbed via the 
#Net:Ftp module.  The local qed_weekly catalog will be
#subset and inserts the new origins.


#here are the modules we are going to be using .. 
#Net::Ping to see if the machine is accessable	(no longer works 1/2004)
#LWP to access the web , and see if the server is up
#Datascope of course 

# Have to include these push statements to get all the modules I need.  If
# program is run without them I get error messages about not finding 
# the Net/FTP.pm, LWP/Simple.pm, and MIME/Base64.pm.


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

    if (! getopts('v') || @ARGV != 1 ) {
        &usage;
    }

# get pf file from command line (or look down pf path)
    $Pf = $ARGV[0] ;
    if ( $Pf =~ /\.pf/) {
	print STDERR "\n Do not use the 'pf' extension when specifying the paramter file. \n"; 
        &usage;
    }

    $Pf = $ARGV[0] . ".pf";
    
    unless ( -e $Pf) { 

       print STDERR "PFPATH - $ENV{PFPATH} \n" if $opt_v;
       @pf_dirs = split /:/, $ENV{PFPATH} ;
       print STDERR "@pf_dirs \n"  if $opt_v;

       foreach $pf_dir (@pf_dirs) {
         $tmp_dir = $pf_dir . "/" . $Pf;
         print STDERR "$tmp_dir \n" if $opt_v;
         &eval_pf( $tmp_dir ) ;
       }
    } else {
       &eval_pf( $Pf ) ;
    }

    $mail_to = join(" ",@mail_to);
    print STDERR "mail_to is @mail_to \n" if $opt_v;

    &setup_global_vars;

# if directories from setup_global_vars dont exist you need to make them 
    &mk_qed_dir($qed_dir);
    &mk_store_dir($qed_store);
    &mk_temp_dir($qed_temp);

    open (MAILLOG, ">$mail_log") || die "Can't open $mail_log \n";
    &log_mesg("Starting getter...\n");

# removed on 1/17/03 as the USGS seems to have disallowed ping 
    if(&server_up){
     &log_mesg("Server [$remote_host] is up\n");

# check for update files on remote server
# if new files exist on remote site, retreive them and continue,
# else exit now and don't update the local qed_weekly catalog,
# convert retreived file to css format
# check the new origin file for start/end times

     $ftp = Net::FTP->new("$remote_host")  or die "Can't connect: $@ \n" ;
     $ftp->login("anonymous",$account)	or die "Couldn't login \n";
#     $remote_pwd = $ftp->pwd		or die "Couldn't get remote pwd. \n";
#     print STDERR "CWD on $remote_host is: $remote_pwd\n";
     $ftp->cwd("$remote_dir")		or die "Couldn't change to directory $remote_dir. \n";
     $ftp->binary;
     @listing = $ftp->dir		or die "Couldn't get file listing on remote machine\n";
     &check_remote_list;	# check to see if remote site has newer file than local qed file 
				# and retreive it
     $ftp->quit;	
     &log_mesg("Done with ftp. \n");
     
     $num2convert	= @convert_list;
   
# quit if there are no files to work on
     if ($num2convert == 0) {
 	&log_failure_and_die("No files to update.  Exiting... \n");
     }

     $c_cnt	= 0;
     while ($c_cnt <  $num2convert) {
        $file2convert = $convert_list[$c_cnt];
        &log_mesg("Going to run convert file for $file2convert. \n");

        # Don't process ehdf.doc file
        if ($file2convert =~ /doc\b/) {
           &log_mesg("$file2convert is not a valid qed weekly file.  This file will not be converted. \n");
           ++$c_cnt;
           next;
        }

        #need to strip .dat from $file2convert so that new_css can be of the form 1999807w
        ($basename,$ext) = split (/\./,$file2convert); # remove extension with split
        $base		= basename($basename);
        $new_css 	= $qed_temp.$base;
       

        if (! -e $new_css) {
            &log_mesg("No descriptor file for $new_css, creating. \n");
	    dbcreate("$new_css","css3.0","$qed_temp\{$base}");
        }


	if ($file2convert =~ /qedevents/) { # the qed2origin program isn't working since y2k, file2convert will never be qedevents.txt
	    &log_mesg("Run $qed2tables2 for $file2convert. \n");
            print STDERR	"$qed2tables2 $fullyear $qed_store$base.$ext $new_css \n" if $opt_v;
            &log_mesg("$qed2tables2 $fullyear $qed_store$base.$ext $new_css \n") if $opt_v;
            &safe_exec("$qed2tables2 $fullyear $qed_store$base.$ext $new_css") ;
        }  else {
 
            print STDERR	"$qed2tables $qed_store$base.$ext $new_css -auth $author \n" if $opt_v;

            &log_mesg("Running $qed2tables for $qed_store$base.$ext. \n");
            $output	= `$qed2tables $qed_store$base.$ext $new_css -auth $author \n`;
	}

        if ($output ne "") {
            &log_mesg("pde2origin ERROR for $base: \n $output \n");
            return;
        } 

        # check db for start/end times
        @db2add		= dbopen($new_css,"r");
        @db2add_origin	= dblookup(@db2add,"","origin","","");

        #get min and max times of origins to be added
        $min_otime		=dbex_eval(@db2add_origin, "min(time)");  
        $max_otime		=dbex_eval(@db2add_origin, "max(time)");  

        print STDERR "\nLooking to add origins from:", &strydtime($min_otime), "to:", &strydtime($max_otime), " \n\n" if ($opt_v || $opt_V);

        @qed		= dbopen($qed_catalog,"r+");
        @qed_or         = dblookup(@qed,"","origin","","");
        $qed_recs       = dbquery (@qed_or,"dbRECORD_COUNT");

        print STDERR "Number of records in current qed_weekly bulletin: $qed_recs\n" if ($opt_v || $opt_V);

        @dbcheck        = dblookup(@qed_or,"","origin","","dbSCRATCH");

        $nrows  = dbquery (@db2add_origin,"dbRECORD_COUNT");
        print STDERR "Number of records that might be added: $nrows \n" if ($opt_v || $opt_V);

        for ($row = 0; $row < $nrows; $row++) {
            $db2add_origin[3] = $row ;
            $record = dbget(@db2add_origin);
            print "Record is: $record\n" if $opt_V;
            @or_record = split(//,$record);
            dbput(@dbcheck,$record);

# find any matching rows in existing qed bulletin
            @matchall = dbmatches(@dbcheck,@qed_or, "chkor","lat","lon","time","depth","ml","mb","ms","ndef","evid");

            if ($#matchall != -1) {     # Found a matching record, no need to update

                print STDERR "Found matching record with no updates needed\n" if ($opt_v || $opt_V) ;
                next;
            } else {                    #Either record is completely new, or you need to update a
                                        # pre-existing qed record if there has been a change to
                                        # lat/lon/time/depth/mag
                print STDERR "All fields of this record do not match.\n" if ($opt_v || $opt_V) ;

#               @matchsome = dbmatches(@dbcheck,@qed_or,"chkor2","evid");
                @matchsome = dbmatches(@dbcheck,@qed_or,"chkor2");
                if ($#matchsome != -1) {        # Need to update values to pre-existing event
                    print STDERR "Updating fields for an event\n" if ($opt_v ||$opt_V);
#                   $qed_or[3] = $matchsome[0];

                    dbput(@qed_or,$record);
                } else {        # we have a new record
                    print STDERR "Adding new record to origin table for an event\n" if ($opt_v || $opt_V);
                    eval { dbadd(@qed_or,$record) } ;
                    if ($@) {
                        warn $@;
                        print STDERR "Duplicate origin matches pre-existing origin.  Will ignore\n" if ($opt_v || $opt_V);
                    }
                }
            }
        }
        dbclose @db2add;

        print STDERR "Number of records in earlier qed bulletin: $qed_recs\n" if ($opt_v || $opt_V);
        $nrows2 = dbquery(@qed_or,"dbRECORD_COUNT");

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
	\nUSAGE: \t$0 [-v] parameter_file

END
	exit(1);
}

sub setup_global_vars{

    ($thismonth)			= (localtime)[4];
    ($thisyear)				= (localtime)[5];
 # change thismonth from 0-11 to 1-12 convention
    $thismonth		= $thismonth + 1;
    $fullyear		= $thisyear + 1900 ;
 # EDIT THESE VARIABLES
    $qed2tables2          = "$ENV{ANTELOPE}/bin/qed2oribin";  #path to qed2origin
    print STDERR "Account is $account from pf $Pf. \n" if $opt_v;

 # QED PICKUP (should not need modification)
#    $remote_host         = "gldfs.cr.usgs.gov";
    $remote_host         = "ghtftp.cr.usgs.gov";
#    $remote_dir	        = "weekly/";
    $remote_dir	        = "pub/weekly/";

 # LOCAL STORAGE DIRECTORIES AND LOGS (modify if needed)
    $qed_store           = $qed_dir."qed_store/";
    $qed_temp            = $qed_dir."temp/";
    $temp_qed		= $qed_temp."temp_qed";
    $qed_catalog	= $qed_dir."qed_weekly";
    $log_file            = $qed_dir."weekly.log";
    $mail_log		= $qed_dir."mail.log";
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
 
sub mk_temp_dir {	# mk qed temp dir
    my ( $qed_temp ) = @_ ;
    my ( $mkdir );


    if (-W $qed_temp) {
        return;
    } else {
        &log_mesg("mk_temp_dir $qed_temp\n") ;
	$mkdir = "mkdir -p $qed_temp";
	&safe_exec( $mkdir );
        return;
    }
}

sub mk_store_dir {	# mk qed store dir
    my ( $qed_store ) = @_ ;
    my ( $mkdir );


    if (-W $qed_store) {
        return;
    } else {
        &log_mesg("mk_store_dir $qed_store \n");
	$mkdir = "mkdir -p $qed_store";
	&safe_exec( $mkdir );
        return;
    }
}

sub mk_qed_dir {	# mk qed dir
    my ( $qed_dir ) = @_ ;
    my ( $mkdir );

# check to see if qed_dir exists and that it is writable (cant use log_mesg because log does not exist)
    if (-W $qed_dir) {
        return;
    } else {
        print STDERR "Default qed dir does not exist, creating.\n" if $opt_v;
	$mkdir = "mkdir -p $qed_dir";
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

    $timeout = 10;
# The pingecho command stopped working ~8/24/1999.
# I have no idea why.  

#    if(pingecho($remote_host, $timeout)){
#        return 1;
#    } else {
#        return 0;
#    }

# Tried to get Net::Ping to work but unsuccessful.
#    $p = Net::Ping->new("tcp", 2);
#    $epi = "epicenter.ucsd.edu";
#    if ($p->ping($epi)) {
#	print STDERR "$epi is alive and well.\n";
#    } else {
#	print STDERR "Your ping of $epi did not work.\n";
#    }
#    print "$epi is alive according to pingecho.\n" if pingecho($epi);
#
#    if ($p->ping($remote_host)) {
#	return 1;
#    } else {
#	return 0;
#    }
#    $p->close();

# Going to use safe_exec to run unix ping.

#    $pingme = "/usr/sbin/ping $remote_host";
#    &log_mesg("$pingme\n");
#    $ping_out = system( $pingme );	# ping returns 0 if machine is up, non-zero if problem

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

    &log_mesg("Cleaning up $qed_temp\n");

# change to figure out why qedevents.txt is not added properly
    
    $rm_origin = "/usr/bin/rm -r $qed_temp/e*";
#   system("\rm -r $qed_temp/e*");     
   &safe_exec ($rm_origin);
    &log_mesg("Exiting $0\n");
    print MAILLOG "Finished with $0.  Verify that catalog looks ok.\n";
    print MAILLOG "\n Files retreived from $remote_host include: @convert_list.\n";
    close(MAILLOG);
    $cmd = "mailx -s \"QED weekly catalog updated \" $mail_to < $mail_log";
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
	(@lsout)	= split(/\s+/, $_);
        if ($lsout[0] =~ /total/) {
            next;
#        } elsif ($lsout[8] =~ /ehdf|qedevents/) {	# qedevents.txt contains updated qed, but not yet weekly files
        } elsif ($lsout[8] =~ /ehdf/) {	#  removed qedevents.txt becauase of y2k probs.
	    $file	= $lsout[8]  or die "Couldn't find file name for @lsout \n"; 
	    $upd_mo	= $lsout[5];
	    $upd_dy	= $lsout[6];
	    $upd_t_yr	= $lsout[7];

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
		    $upd_yr	= $fullyear;	
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
                $num_mo 	= $mo_names{$upd_mo};
	    }
# get last update time of remote file (this could be replaced by an ftp->mdtm if permissions allow) 

	   $last_remote_update	= str2epoch("$upd_mo $upd_dy $upd_yr $upd_time") || die "Couldn't find last update time of $file \n";
           print STDERR "checking file: $file \n" if $opt_v;
#	   $last_remote_update	= $ftp->mdtm( $test_run )  || die "Couldn't get modify time on remote $file \n";
#	   print STDERR "$last_remote_update \n" || die "Can't get last_remote_update time.\n" ;


	   $gzipfile	= $qed_store.$file.".gz";
	   if (-e $qed_store.$file) {
		$local_update	= (stat("$qed_store$file"))[9]  || die "Couldn't find last update time for $qed_store$file \n";
                print STDERR "file: $file \n\tlast remote update: $last_remote_update last local update: $local_update\n" if $opt_v;
		if ($local_update < $last_remote_update) {
		    &get_remote_file;
		    next;
		} else {
		    &log_mesg("No change to file $file. \n");
		}
           } elsif (-e $gzipfile) {	 # add check to see if gzipped file exists 
                $local_update   = (stat("$gzipfile"))[9]  || die "Couldn't find last update time for $gzipfile \n";
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
           #else close
	   }
	} else {
          &log_mesg("no ehdf match for $lsout[8] \n") ;
          next;
        }
    #foreach close
    }
}

sub get_remote_file {

    &log_mesg("Retreiving file $file from $remote_host .\n");
    $ftp->get($file,$qed_store.$file)  || die "Can't retreive file $file from $remote_host or store it as $qed_store.$file.\n";
    @convert_list = (@convert_list, $qed_store.$file);
}


sub log_failure_and_die {

    my($problem) = $_[0];

    &log_mesg($problem);
    print MAILLOG "\n\t $problem \n";
    close MAILLOG ;
    print STDERR "$problem \n";
    $cmd = "mailx -s \"NO UPDATE- $0 \" $mail_to < $mail_log" ;
    if ($mail_to) {
        system($cmd);
        if ($?) {
            print STDERR "$cmd error $? \n";
        }
    }
    exit(1);
        
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
        
}


