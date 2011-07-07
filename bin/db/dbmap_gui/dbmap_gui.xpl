# *
# * Copyright (c) 2010 Nikolaus Horn, ZAMG / Vienna. All rights reserved
# *
# * This software may be used freely in any way as long as the copyright
# * statement above is not removed.
# *
# * dbmap_gui, now in perl-tk
# *
# * Created on 2010-04-22, fixed later
# * based again on the bplot manpage
# *

package main;

use strict;
use warnings;

use Datascope;

use Tk;
use Tk::After;
use Tk::Balloon;
use Tk::Canvas;
use Tk::Bplot;

use ptkform;
use ptkalert;
use POSIX qw(_exit);

use Vector;

use Encode 'from_to';

our( $opt_v, $opt_o, $opt_O, $opt_p, $opt_r, $opt_f, $opt_l, $opt_L );
our $items;
use Getopt::Std;

if ( !getopts('f:l:L:o:O:p:r:v') || @ARGV < 1 ) {
    die (
"Usage: $0 [-v] [-p pf] [-l field] [-r range] [-f psfile] [-o dboverlay.table|-O table] database[.table]\n"
    );
}

our $psfile = ($opt_f) // "/tmp/dbmap_gui.ps";
my $Pf = ($opt_p) // "dbmap_gui";
my $pagewidth_mm  = pfget( $Pf, "pagewidth_mm" );
my $pageheight_mm = pfget( $Pf, "pageheight_mm" );
my $pagemargin_mm = pfget( $Pf, "pagemargin_mm" );
our $printcmd = pfget( $Pf, "printcmd" );
my $symbol            = pfget( $Pf, "symbol" );
my $symbol_color            = pfget( $Pf, "symbol_color" );
my $symbol_fill_color            = pfget( $Pf, "symbol_fill_color" );
my $symbol_size            = pfget( $Pf, "symbol_size" );
my $label_color          = pfget( $Pf, "label_color" );
my $label_font          = pfget( $Pf, "label_font" );
my $label_font_color          = pfget( $Pf, "label_font_color" );
our $circle_color         = pfget( $Pf, "circle_color" );
my $overlay_symbol    = pfget( $Pf, "overlay_symbol" );
my $overlay_symbol_size    = pfget( $Pf, "overlay_symbol_size" );
my $overlay_symbol_color    = pfget( $Pf, "overlay_symbol_color" );
my $overlay_symbol_fill_color    = pfget( $Pf, "overlay_symbol_fill_color" );
my $overlay_label_color  = pfget( $Pf, "overlay_label_color" );
my $overlay_label_font  = pfget( $Pf, "overlay_label_font" );
my $overlay_label_font_color  = pfget( $Pf, "overlay_label_font_color" );
our $overlay_circle_color = pfget( $Pf, "overlay_circle_color" );
my $range_degrees        =  ($opt_r) // pfget( $Pf, "range_degrees" );
my %tables        = %{ pfget( $Pf, "tables" ) };
my @window_layout = @{ pfget( $Pf, "window_layout" ) };

our $mw = MainWindow->new;
$mw->setPalette(priority=>100, background=>"lightsteelblue") ;


our( %var, %widget, $latlon );        # = MainWindow->new ;

ptkform($mw, \%var, \%widget, @window_layout) ;

#th widget was obviously created be ptkform with the someting->Scrolled method, so we are only interested in the subwidget 
our $canvas = $widget{mycanvas}->Subwidget("canvas") ;


my $water_color = '#c0c0ff';

#get dimensions from existing canvas, stuff previously specified in parameter file
my $cwidth=$canvas->reqwidth();
my $cheight=$canvas->reqheight();

$canvas->configure( 
				-scrollregion => [0, 0, $cwidth, $cheight ],
				-borderwidth => 0
			);
$widget{mycanvas}->configure(
	-scrollbars=>""
);	


$canvas->create(
  'bpviewport', 'vp', 0, 0, 
  -wtran => 'edp',
  -latr       => -0.0,
  -lonr       => 0.0,
  -width      => $cwidth,
  -height     => $cheight,
  -xleft      => -10.0,
  -xright     => 10.0,
  -ybottom    => -10.0,
  -ytop       => 10.0,
  -mleft	=> 10,
  #-fill       => 'lightsteelblu',
  -fill_frame => 'lightsteelblue',
  -tags       => [ 'vp', 'map' ],
);

#    create map for EDP map

$canvas->create(
  'bpmap', 'vp', -resolution => 'auto',
  -political  => '1:#ff0000:1,2:#00a000:0,3:#ff00ff:0',
  -fill_water => $water_color,
  -tags       => 'map',
);

#    put in axes labeling for EDP map
#    note that currently the axis number labeling
#    is disabled for EDP maps

$canvas->create(
  'bpaxes', 'vp', -mindx => 100,
  -xincrement       => 30.0,
  -yincrement       => 30.0,
  -xincrement_small => 5.0,
  -yincrement_small => 5.0,
  -tags             => 'map',
);

#    put in lat-lon grid lines for EDP map

$canvas->create(
  'bpgrid', 'vp', -mindx => 0,
  -mindy            => 0,
  -xincrement       => 10.0,
  -yincrement       => 10.0,
  -xincrement_small => 5.0,
  -yincrement_small => 5.0,
  -linewidth        => -1,
  -linewidth_small  => 1,
  -fill             => 'darkgray',
  -fill_small       => 'darkgray',
  -tags             => 'map',
);

$canvas->CanvasBind( '<KeyPress>'            => \\&bindzoommap );
$canvas->CanvasBind( '<ButtonPress-3>'       => \\&bindstartdrag );
$canvas->CanvasBind( '<Button3-Motion>'      => \\&binddrag );
$canvas->CanvasBind( '<ButtonRelease-3>'     => \\&bindstopdrag );
#$canvas->CanvasBind ( '<ButtonPress-1>' => \\&bindfindclosest ) ;
$canvas->CanvasBind( '<ButtonPress-1>'   => \\&bindstartdrag );
$canvas->CanvasBind( '<Button1-Motion>'  => \\&binddrag );
$canvas->CanvasBind( '<ButtonRelease-1>' => \\&bindstopdrag );
$canvas->CanvasBind( '<Motion>'          => \\&showlatlon );
$canvas->CanvasBind( '<Any-Enter>'       => \\&bindenter );

$widget{quit}->configure( -command=>sub { POSIX::_exit(0) ; }, -bg =>"red", -fg =>"white" ) if defined $widget{quit} ;
$widget{print}->configure( -command=>sub{ postscript(1) } , -bg =>"lightblue", -fg =>"blue" ) if defined $widget{print} ;

# open database and do stuff
$items = vector_create;

my $dbname = shift;

my $mapcenter_lat = 0;
my $mapcenter_lon = 0;
my $recno         = shift;
my $subset_recno  = 0;
if ( defined($recno) ) {
    $subset_recno = 1;
}

my @db = dbopen_database( $dbname, 'r' );
@db = dblookup( @db, '', 'site', '', '' ) if ( $db[1] < 0 );
@db = dbsubset( @db, "lat != NULL && lon != NULL" );
my $nrecs = dbquery @db, 'dbRECORD_COUNT';

if ( $opt_o || $opt_O ) {
	my $tablename;
    my $fieldname  = 0;
    my $circlename = 0;
    my @dbo;
    if ($opt_O) {
        my $ntables = dbquery( @db, "dbVIEW_TABLE_COUNT" );
        print "not enough tables for $opt_O" if ( $ntables < 2 );
        @dbo = dbseparate( @db, $opt_O );
		$tablename = $opt_O;
    }
    else {
        @dbo = dbopen_database( $opt_o, "r" );
		$tablename = dbquery( @dbo, "dbTABLE_NAME" );
    }

    if ( $opt_L) {
        $fieldname = $opt_L;
	} elsif ( defined( $tables{$tablename} )
      && defined( $tables{$tablename}{label} ) ) {
        $fieldname = $tables{$tablename}{label};
    } else {
        print "no label for table $tablename defined!\n";
    }

    if ( defined( $tables{$tablename} )
      && defined( $tables{$tablename}{circle} ) )
    {
        $circlename = $tables{$tablename}{circle};
    }
    my $nov = dbquery @dbo, 'dbRECORD_COUNT';

    if ( $nov > 0 && $fieldname ) {
        my $overlay = vector_create;
        for ( $dbo[3] = 0 ; $dbo[3] < $nov ; $dbo[3]++ ) {
            my ( $lat, $lon, $label ) =
              dbgetv( @dbo, 'lat', 'lon', $fieldname );
            from_to( $label, "iso-8859-1", "utf-8" );
            vector_append( $overlay, -1, $lon, $lat, sprintf( '%s', $label ) );

            if ($circlename) {
                my $circle = vector_create;
                my ($radius) = dbgetv( @dbo, $circlename );
                $radius /= 111.15;
                my $steps = 20;

                #circle_vector($lat,$lon,$radius,20,$circles);		
				my ($x1,$y1);
                for ( my $i = 0 ; $i < $steps ; $i++ ) {
                    $x1 = $lon + $radius * cos( rad( $i * 360.0 / $steps ) );
                    $y1 = $lat + $radius * sin( rad( $i * 360.0 / $steps ) );
                    vector_append $circle, -1, $x1, $y1;
                }
				$x1 = $lon + $radius * cos( 0 );
				$y1 = $lat + $radius * sin( 0 );
				vector_append $circle, -1, $x1, $y1;

                $canvas->create(
                  'bppolyline', 'vp', -vector => $circle,
                  -fill      => $overlay_circle_color,
                  -visible   => 1,
                  -outline   => $overlay_circle_color,
                  -linewidth => 2,
                  -tags      => 'overlay_circle'
                );
                vector_free($circle);
            }
        }
        $canvas->create(
          'bppolypoint', 'vp', -vector => $overlay,
          -symbol   => $overlay_symbol,
          -outline     => $overlay_symbol_color,
          -fill     => $overlay_symbol_fill_color,
          -size     => $overlay_symbol_size,
		  -font => $overlay_label_font,
		  -textforeground => $overlay_label_font_color,
          -showtext => 1,
          -tags     => 'overlay',
        );
    }

}

my @dblat = dblookup( @db, "", "", "lat", 0 );
my $tablename = dbquery( @dblat, "dbFIELD_BASE_TABLE" );
if ($subset_recno) {
    $db[3] = $recno;
    ( $mapcenter_lat, $mapcenter_lon ) = dbgetv( @db, "lat", "lon" );
}
else {
    $mapcenter_lat = dbex_eval( @db, "sum(lat)/count()" );
    $mapcenter_lon = dbex_eval( @db, "sum(lon)/count()" );
}

if ( ($opt_l) ||(defined( $tables{$tablename} ) && defined( $tables{$tablename}{label} ) )) {
    my $fieldname = $opt_l // $tables{$tablename}{label};
    for ( $db[3] = 0 ; $db[3] < $nrecs ; $db[3]++ ) {
        my ( $lat, $lon, $label ) = dbgetv( @db, 'lat', 'lon', $fieldname );
        from_to( $label, "iso-8859-1", "utf-8" );
        vector_append( $items, -1, $lon, $lat, sprintf( '%s', $label ) );
    }
} else {

    for ( $db[3] = 0 ; $db[3] < $nrecs ; $db[3]++ ) {
        my ( $lat, $lon ) = dbgetv( @db, 'lat', 'lon' );
        vector_append( $items, -1, $lon, $lat );
    }
}

$canvas->create(
  'bppolypoint', 'vp', -vector => $items,
  -symbol   => $symbol,
  -fill     => $symbol_color,
  -outline  => $symbol_color,
  -size     => $symbol_size,
  -font => $label_font,
  -textforeground => $label_font_color,
  -showtext => 1,
  -tags     => 'items',
);


$canvas->itemconfigure(
  'vp', 
  -xleft => -$range_degrees,
  -xright  => $range_degrees,
  -ybottom => -$range_degrees,
  -ytop    => $range_degrees,
-latr => $mapcenter_lat,
-lonr => $mapcenter_lon
	);

$mw->resizable( 0, 0 );
$mw->title($dbname);

MainLoop;

sub bindstartdrag {
    my ($w) = @_;

    my ( $vp, $inside ) = bplot_locate($w);

    if ( !defined $vp ) { return; }

    our $dragwindow = $vp;

    my $e = $w->XEvent;

    our( $xstart, $ystart ) = ( $e->x, $e->y );
}

sub binddrag {
    my ($w) = @_;

    our $dragwindow;

    if ( !defined $dragwindow ) { return; }

    my $e = $w->XEvent;

    my ( $x, $y ) = ( $e->x, $e->y );

    our( $xstart, $ystart );

    my $delx = $x - $xstart;
    my $dely = $y - $ystart;

    $w->itemconfigure(
      $dragwindow, -xtranslate => -$delx,
      -ytranslate => -$dely,
    );
}

sub bindstopdrag {
    my ($w) = @_;

    our $dragwindow;

    if ( !defined $dragwindow ) { return; }

    $w->itemconfigure(
      $dragwindow, -xtranslate => 'apply',
      -ytranslate => 'apply',
    );

    undef $dragwindow;
}

sub postscript {
    my $printflag = @_;
    our $canvas;

    my $vp = 'vp';
    our $printcmd;

    our $psfile;
	my $result;
    $canvas->postscript(
      -colormode  => 'color',
      -file       => $psfile,
      -pageheight => '15c',
      -pagewidth  => '15c',
      -rotate     => 0
    );

    if ($printflag) {
        my $cmd         = "$printcmd " . $psfile . ' >/dev/null';
		eval {
			$result = system $cmd;
		};
		if ($@) {
			ptkalert $mw, "alert $@\n";
		}
    }

}

sub showlatlon {
    my ($w) = @_;

    my ( $vp, $inside ) = bplot_locate($w);

    our $latlon;

    if ( !defined $inside || $inside == 0 ) {
        $latlon = "";
        return;
    }

    #    set the display

    my $e = $w->XEvent;
    my ( $lon, $lat ) = viewport_pixels2wcoords( $vp, $e->x, $e->y );
    if ( $lat < 0.0 ) {
        $lat = sprintf "S%06.3f", -$lat;
    }
    else {
        $lat = sprintf "N%06.3f", $lat;
    }

    if ( $lon < 0.0 ) {
        $lon = sprintf "W%07.3f", -$lon;
    }
    else {
        $lon = sprintf "E%07.3f", $lon;
    }

    $var{latlon} = $lat . " " . $lon;
}

sub zoommap {
    my $vp     = shift;
    my $x      = shift;
    my $y      = shift;
    my $factor = shift;

    our $canvas;

    my ($xl) = $canvas->itemcget( $vp, -xleft );
    my ($xr) = $canvas->itemcget( $vp, -xright );
    my ($yb) = $canvas->itemcget( $vp, -ybottom );
    my ($yt) = $canvas->itemcget( $vp, -ytop );

    my $xc = 0.5 * ( $xl + $xr );
    my $yc = 0.5 * ( $yb + $yt );

    $xl -= $xc;
    $xr -= $xc;
    $yb -= $yc;
    $yt -= $yc;

    $xl = $xc + $factor * $xl;
    $xr = $xc + $factor * $xr;
    $yb = $yc + $factor * $yb;
    $yt = $yc + $factor * $yt;

    $canvas->itemconfigure(
      $vp, -xleft => $xl,
      -xright  => $xr,
      -ybottom => $yb,
      -ytop    => $yt,
    );
}

sub bindzoommap {
    my ($w) = @_;
    my $e = $w->XEvent;
    my ( $vp, $inside ) = bplot_locate($w);
    if ( !defined $inside || $inside == 0 ) { return; }
    my $key = $e->K;
    if ( $key eq "O" ) { zoommap( $vp, $e->x, $e->y, 2.0 ); }
    if ( $key eq "o" ) { zoommap( $vp, $e->x, $e->y, 1.25 ); }
    if ( $key eq "I" ) { zoommap( $vp, $e->x, $e->y, 0.5 ); }
    if ( $key eq "i" ) { zoommap( $vp, $e->x, $e->y, 0.8 ); }

    if ($opt_o || $opt_O) {
        if ( $key eq "L" ) { togglelabels( $vp, 'overlay' ); }
        if ( $key eq "l" ) { togglelabels( $vp, 'items' ); }
        if ( $key eq "c" ) { togglelabels( $vp, 'fillcircles' ); }
        if ( $key eq "C" ) { togglelabels( $vp, 'hidecircles' ); }
    }
    else {
        if ( $key eq "L" ) { togglelabels( $vp, 'all' ); }
        if ( $key eq "l" ) { togglelabels( $vp, 'all' ); }
    }
    if ( $key eq "p" ) { postscript(0); }
    if ( $key eq "P" ) { postscript(1); }
    if ( $key eq "Q" ) { POSIX::_exit(0); }
}

sub togglelabels {
    my $vp   = shift;
    my $flag = shift;
    our $canvas;
    our $circlecolor;
    my $switch;

    if ( $flag eq "all" || $flag eq "items" ) {
        my $myitems = $canvas->find( 'withtag', 'items' );
        $switch = $canvas->itemcget( $myitems, -showtext );
        if ( $switch == 0 ) {
            $switch = 1;
        }
        else {
            $switch = 0;
        }
        $canvas->itemconfigure( $myitems, -showtext, $switch );
    }

    if ( $flag eq "all" || $flag eq "overlay" ) {
        my $myoverlay = $canvas->find( 'withtag', 'overlay' );
        if ($myoverlay) {
            $switch = $canvas->itemcget( $myoverlay, -showtext );
            if ( $switch == 0 ) {
                $switch = 1;
            }
            else {
                $switch = 0;
            }
            $canvas->itemconfigure( $myoverlay, -showtext, $switch );
        }
    }

    #if ($flag eq "fillcircles" || $flag eq "hidecircles") {
    my (@mycircles) = $canvas->find( 'withtag', 'xxxxxx' );

    if (@mycircles) {

        if ( $flag eq "fillcircles" || $flag eq "hidecircles" ) {
            $switch = $canvas->itemcget( $mycircles[0], -visible );
            if ( $switch == 0 ) {
                $switch = 1;
            }
            else {
                $switch = 0;
            }
            my $color = $canvas->itemcget( $mycircles[0], -fill );
            if ( $color ) {
                $color = '';
            }
            else {
                $color = $overlay_circle_color;
            }


			if ($flag eq "hidecircles") {
				foreach my $i (@mycircles) {
					$canvas->itemconfigure( $i, -visible, $switch );
				}
			}
			if ($flag eq "fillcircles") {
				foreach my $i (@mycircles) {
					$canvas->itemconfigure( $i, -fill, $color );
				}
			}
        }
    }

    #	if ($flag eq "fillcircles" || $flag eq "hidecircles") {

}

sub bindenter {
    my ($w) = @_;
    my $e = $w->XEvent;
    our $canvas;
    $canvas->CanvasFocus();
}
