use sysinfo;
use Net::FTP;
use Datascope;
use Time::Local;
use strict;

######################
#                    #
#  Program setup     #
#                    #
######################

if ( @ARGV < 4 || @ARGV > 6) { 
  die "\n\nUsage: $0 IP PORT LOCAL_DIR REMOTE_DIR [LOG_FILE] [TRUE_OR_DEMO]\n\n";
}

#
# Vars
#
  our($IP,$PORT,$LOCAL,$REMOTE,$LOG,$TRUE);
  our $event : unique = 0;
  our($ftp,$start_sta,$start_file,$prelim_end);
  our($cmd,$temp_local);
  our($start_file,$new_start,$span);
  our($rem_file,$log_file,$ftp,$stat);
  our(%seen,@diff,@download);

#
#  Get params form cmd
# 
  $IP       = $ARGV[0];
  $PORT     = $ARGV[1];
  $LOCAL    = $ARGV[2];
  $REMOTE   = $ARGV[3];
  $LOG      = $ARGV[4]; #optional, defaults to STDOUT
  $TRUE     = $ARGV[5]; #optional, defaults to false

#
# Output PID to parent
#
print "$$";

#
# Open log file 
#
  if(defined($LOG)) {
    open STDOUT, '>', "$LOG" or die "Can't redirect STDOUT to $LOG: $!";
  }
  open STDERR, ">&STDOUT"    or die "Can't redirect STDERR to $LOG: $!";

  select STDERR; $| = 1;      # make unbuffered
  select STDOUT; $| = 1;      # make unbuffered

  $cmd = "$0 @ARGV" ;
  log_and_print ( "NOTIFY-PF: ".$cmd ) ; 
  log_and_print ( "NOTIFY-PF: Starting execution on ".my_hostname() );
  log_and_print ( "NOTIFY-PF: ".strydtime(now()) );
  log_and_print ( "NOTIFY-PID: $$" ) ; 

    log_and_print( "NOTIFY-PF: Parameters:" );
    log_and_print( "NOTIFY-PF:\tIP         ->$IP" );
    log_and_print( "NOTIFY-PF:\tPORT       ->$PORT" );
    log_and_print( "NOTIFY-PF:\tLOCAL_DIR  ->$LOCAL" );
    log_and_print( "NOTIFY-PF:\tREMOTE_DIR ->$REMOTE" );
    log_and_print( "NOTIFY-PF:\tLOG        ->$LOG" );
    log_and_print( "NOTIFY-PF:\tTRUE       ->$TRUE" );

 
# 
# Fix variables
#
  if(!defined($TRUE)){ $TRUE = 0; }
  if($TRUE eq "demo" || $TRUE == "DEMO" ){ $TRUE = 0; }

#
# Sanity check
#
  if(! $IP=~ /([\d]{1,3}\.[\d]{1,3}\.[\d]{1,3}\.[\d]{1,3})/ ) {
    die "ERROR: IP $IP is not a valid format."; }
  if(! $PORT=~ /([\d]{1,6})/) { die "ERROR: Port $PORT not valid"; }    
  if(! -e $LOCAL )  { die "ERROR: Can't access local direcotry $LOCAL"; }
 

######################
#                    #
#  MAIN              #
#                    #
######################

   #Recor time of start
   $start_sta = now();
   log_and_print ("NOTIFY: Start time ".strydtime($start_sta) );

   $ftp = loggin_in($IP,$PORT);
   if ( $ftp == 0 ) { die "ERROR: Unable to stablish connection to $IP:$PORT"; }

   #
   # Get files from dires
   #
   log_and_print("NOTIFY-LOOP: Reading remote directory $REMOTE");
   my( $rem_file ) = read_dir( $REMOTE, $ftp );
   log_and_print("NOTIFY-LOOP: Reading local directory $LOCAL");
   my( $loc_file ) = read_dir( $LOCAL );

   #print_dirs($loc_file,$rem_file);
   @download = compare_dirs($loc_file,$rem_file,'files flagged');

   FILE: foreach (@download) {
	 $start_file = now();
	 log_and_print("NOTIFY-LOOP:\t$_".strydtime($start_file) );
     if ( $TRUE ) {
	   while (){
		   $stat = $ftp->status;
		   if ($stat != 2 ) {
			 $ftp->quit;
			 $ftp = loggin_in($IP,$PORT);
		   } 
		   elsif ( $ftp == 0 ) { last; }
		   else{
			   $new_start = now();
			   $ftp->get("$REMOTE/$_", "$LOCAL/$_") or 
				   log_and_print("ERROR:\t\tCan't download $REMOTE/$_ : $!");
			   log_and_print( "NOTIFY-LOOP:\t\tSpan:".strtdelta(now()-$new_start) );

			   #Lets check if file download completed
			   $temp_local = read_dir( $LOCAL );
			   if($temp_local->{$_}{size} == $rem_file->{$_}{size}) { last; }
		   }
	   }
	 }
     else { log_and_print("NOTIFY-LOOP:\t\tDEMO run. Not downloading."); }

     log_and_print("NOTIFY-LOOP:\t\tEND ".strtdelta(now()-$start_file));
   } #end of foreach @downloads
 
   my( $loc_file ) = read_dir( $LOCAL );
   my( $rem_file ) = read_dir( $REMOTE, $ftp );
   @diff = compare_dirs($loc_file,$rem_file,'files missing');

   log_and_print("NOTIFY: Connection attempts $event");
   $ftp->quit;
   log_and_print ("NOTIFY: End time ".strydtime(now()) );
   log_and_print("NOTIFY: Total of ".strtdelta(now()-$start_sta));

######################
#                    #
#  Compare Dirs      #
#                    #
######################
sub compare_dirs {
  my $local_files= shift;
  my $remote_files= shift;
  my $comment = shift;
  my @flagged; 
  my $rf;
  my $lf;

  FILE: foreach $rf ( keys %$remote_files ) {
     foreach $lf ( keys %$local_files ) {
         if($lf eq $rf) {
              if($local_files->{$lf}{size} < $remote_files->{$rf}{size}) {
                 push @flagged, "$rf";
                 next FILE;
              }
              elsif($local_files->{$lf}{date} < $remote_files->{$rf}{date} ) {
                 push @flagged, "$rf";
                 next FILE;
              }
              else { 
                 next FILE;
			  }
         }
     }
     push @flagged, "$rf";
  } #end of foreach $rt

	log_and_print("NOTIFY: ".@flagged." $comment");
	return @flagged;
}


######################
#                    #
#  Login in to sta   #
#                    #
######################
sub loggin_in {
	my $my_ip   = shift;
	my $my_port = shift;
	my $my_ftp;

	 $event++;

	 my $my_ftp=Net::FTP->new(Host=>$my_ip, Passive=>1, Timeout=>30, Port=>$my_port) or 
	   log_and_print("ERROR: Can't ftp to $my_ip:$my_port : $@ ");

     if(!defined($my_ftp)) {
       log_and_print("ERROR: Problem in connection $@."); 
       return 0;
     }
     else {
       log_and_print("NOTIFY-LOOP: Connected to $my_ip");
       $my_ftp->login() or 
	   			log_and_print("ERROR: Can't login to $my_ip: $! $@");

       return $my_ftp;
     }

} #end of loggin_in

######################
#                    #
#  Read rem/loc dir  #
#                    #
######################
sub read_dir {
   my $path     = shift;
   my $ftp_pnt  = shift;
   my %file     = (); 
   my @directory= ();
   my $open;
   my $name;
   my $epoch;
   my $line;
   my $f;

   if(defined($ftp_pnt)) {
	#log_and_print("\tRemote directory");
	@directory = $ftp_pnt->dir( $path) ;
	if(@directory){
	   foreach $line (@directory) {
	      if( $line =~ /(\d+)\s(\w{3}) +(\d{1,2}) (\d{2}):(\d{2})\s(.+-.+-)(\d{4})(.+)$/ ){
	        #log_and_print($line);
	         $epoch = timelocal(0,$5,$4,$3,$2,$7);
	         $name  = "$6$7$8";
	         $file{$name}{size}=$1;
	         $file{$name}{date}=$epoch;
	      }
	      else { log_and_print("ERROR: Can't interpret remote structure: $line "); }
	   }
	}
	else {
		log_and_print("ERROR: Remote directory empty or error in ftp->dir");
		return 0;
	}
   }
   else {
     $open = opendir DIR, $path or
        log_and_print("ERROR: Failed to open $path: $!");

     if ($open ) {
       while($f = readdir DIR) {
          my $file = "$path/$f";
          if(-d "$file"){ next; } 
          else { 
            #log_and_print($file);
            $file{$f}{size} = (stat($file))[7]; 
            $file{$f}{date} = (stat($file))[9]; 
          }   
       }   
     }
     else{ return 0; }
     close(DIR);
   }

   return (\%file);
} #end of read_remote_dir


######################
#                    #
#  Log and Print     #
#                    #
######################


sub log_and_print {
  my $log = shift;

  print now()."-$log \n";

  return 0;
}
