package timeutil ;

require Exporter;   
@ISA = ('Exporter');

@EXPORT=qw( month_times next_jdate prev_jdate prev_month next_month yearmonth yearmonth2epoch month ) ;

# ( $starttime, $endtime )       =  &month_times( $year, $month );
# $next_jdate                    =  &next_jdate ( $jdate ) ;
# $prev_jdate                    =  &prev_jdate ( $jdate ) ;
# $yearmonth                     =  &prev_month ( $yearmonth ) ;
# $yearmonth                     =  &next_month ( $yearmonth ) ;
# $yearmonth                     =  &yearmonth  ( $epoch ) ;
# $time                          =  &yearmonth2epoch ( $yearmonth ) ;
# $month                         =  &month ( $epoch ) ;
use strict ;
use Datascope ;
use archive ;


sub month_times {  # ( $starttime, $endtime ) = &month_times( $year, $month ) ;
    my ( $year, $month ) = @_ ; 
    my ( $stime ) ;
    my ( $starttime, $endtime ) ;
    
    $stime = "$month/1/$year" ;
    elog_debug( "sub times	stime	$stime" ) if $::opt_V ;

    $starttime = str2epoch($stime) ;
    $month++ ;
    if ($month == 13 ) {
        $month = 1 ;
        $year++ ;
    }

    $stime = "$month/1/$year" ;
    elog_debug( $stime ) if $::opt_V ;

    $endtime = str2epoch( $stime ) ;
    
    return ( $starttime, $endtime ) ;
}

sub next_jdate {  # $next_jdate = &next_jdate ( $jdate ) ;
    my ( $jdate ) = @_ ;
    my ( $next_jdate ) ;
    
    $next_jdate = yearday( epoch( $jdate ) + 86400 ) ;
    return $next_jdate ;
}

sub prev_jdate {  # $prev_jdate = &prev_jdate ( $jdate ) ;
    my ( $jdate ) = @_ ;
    my ( $prev_jdate ) ;
    
    $prev_jdate = yearday( epoch( $jdate)  - 86400 ) ;
    return $prev_jdate ;
}

sub prev_month {  # $yearmonth = &prev_month ( $yearmonth ) ;
    my ( $yearmonth ) = @_ ;
    my ( $month, $time, $year ) ;
    
    $month = $yearmonth % 100 ;
    $year  = int( $yearmonth / 100 ) ;
    if ( $month == 1 ) {
        $month = 12 ;
        $year-- ;
    } else {
        $month-- ;
    }
    
    $yearmonth = ( $year * 100 ) + $month ;
    return $yearmonth ;
}

sub next_month {  # $yearmonth = &next_month ( $yearmonth ) ;
    my ( $yearmonth ) = @_ ;
    my ( $month, $time, $year ) ;
    
    $month = $yearmonth % 100 ;
    $year  = int( $yearmonth / 100 ) ;
    if ( $month == 12 ) {
        $month = 1 ;
        $year++ ;
    } else {
        $month++ ;
    }
    
    $yearmonth = ( $year * 100 ) + $month ;
    return $yearmonth ;
}

sub yearmonth {  # $yearmonth = &yearmonth ( $epoch ) ;
    my ( $epoch ) = @_ ;
    my ( $yearmonth ) ;
    
    $yearmonth = epoch2str ( $epoch, "%Y%m" ) ;

    return ( $yearmonth ) ;
}

sub yearmonth2epoch {  # $time = &yearmonth2epoch ( $yearmonth ) ;
    my ( $yearmonth ) = @_ ;
    my ( $month, $time, $year ) ;
    
    $month = $yearmonth % 100 ;
    $year  = int( $yearmonth / 100 ) ;
    $time  = "$year-$month-01" ;
    $time  = str2epoch ( $time ) ;

    return ( $time ) ;
}

sub month {  # $month = &month ( $epoch ) ;
    my ( $epoch ) = @_ ;
    my ( $month ) ;
    
    $month = epoch2str ( $epoch, "%m" ) ;

    return ( $month ) ;
}


