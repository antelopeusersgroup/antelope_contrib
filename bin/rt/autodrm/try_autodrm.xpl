#   Copyright (c) 2003 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. Any use of this software module without        
#   express written permission from Boulder Real Time Technologies,   
#   Inc. is prohibited.                                               


use Getopt::Std ;
 
if ( ! getopts('nt:vV:') || @ARGV > 1)
    { die ( "Usage: $0 [-nv] [-t start] [-V version] [email]\n" ) ; }

use Datascope ;
use Mail::Internet ;

$VERSION = $opt_V ? $opt_V : "GSE2.0" ;

if ( defined $ARGV[0] ) { 
    if ( $ARGV[0] =~ /@/ ) { 
	$autodrm = $ARGV[0] ; 
    } else { 
	$autodrm = "autodrm\@$ARGV[0]" ; 
    }
} else { 
    $autodrm = "autodrm" ; 
}

$t0 = $opt_t ? $opt_t : time() - 86400 ;
$start = &epoch2str( $t0, "%Y/%m/%d %H:%M" ) ;
$stop = &epoch2str( $t0+60, "%Y/%m/%d %H:%M" ) ;
$date = &epoch2str ( time, "%a_%b_%d_%Y_%H:%M", "" ) ;

$start_bulletin = &epoch2str( $t0-7*86400, "%Y/%m/%d %H:%M" ) ;
$start_outage = &epoch2str( $t0-2*3600, "%Y/%m/%d %H:%M" ) ;
$stop_bulletin = &epoch2str( $t0, "%Y/%m/%d %H:%M" ) ;

chop($idinfo = `idinfo`) ;
($host, $ip, $hostid, $email, $name) = split (' ', $idinfo, 5 ) ;
$who = $name ; 
$who =~ tr/ /_/ ;

$mail = "
BEGIN $VERSION
MSG_TYPE REQUEST
MSG_ID $date $who
E-MAIL $email

STA_LIST *
CHAN_LIST *Z,*z
TIME $start TO $stop

STATION $VERSION
CHANNEL $VERSION
RESPONSE

WAVEFORM $VERSION

TIME $start_outage TO $stop_bulletin
OUTAGE

TIME $start_bulletin TO $stop_bulletin
BULLETIN

STOP
" ; 

@mail = split ( '\n', $mail ) ; 

if ( $opt_v || $opt_n ) { 
    print STDERR "rtmail $autodrm\n" ;
    print STDERR "$mail\n<EOT>\n" ;
}

if ( ! $opt_n ) { 
    open ( MAIL, "| rtmail $autodrm" ) ; 
    print MAIL $mail ;
    close MAIL ;
}
