use strict ;
use warnings ;
use feature ":5.10" ; 

use Tk ;
use Tk::After ;
use Tk::Balloon ;
use Tk::Canvas ;
use Tk::Bplot ;

use ptkform ; 
use ptkalert ; 
use POSIX qw(_exit) ;
use Data::Dumper ;

use Datascope ;
use Vector ;
use dbmap ; 
use record ;

use Getopt::Std ;
 
our($opt_d, $opt_e, $opt_o, $opt_p, $opt_v) ;
my $pgm = $0 ; 
$pgm =~ s".*/"" ;
if ( ! getopts('d:e:o:p:v') || @ARGV < 1 ) { 
    die ( "Usage: $pgm [-d overlay,...] [-e evid] [-o orid] [-p pf] [-v] database [orid]\n" ) ; 
}

our (%Var, %Widget) ;
our ($Canvas, $Viewport, $Latlon) ;

our $Pf = $opt_p // "$pgm.pf" ;
our $Pfdbloc = "dbloc2.pf" ;
our $Database = shift ;
our @Db = dbopen($Database, 'r') ;
our %Db ;

my (@dborigin, @dborigin_subset) ;
@dborigin = dblookup(@Db, 0, "origin", 0, 0 ) ; 
if ( $opt_e ) { 
    @dborigin_subset = subset_event($opt_e, @Db) ; 
} elsif ( $opt_o || @ARGV > 0 ) { 
    $opt_o = shift if @ARGV > 0 ; 
    @dborigin_subset = subset_origin($opt_o, @Db) ; 
}  else { 
    @dborigin_subset = @dborigin ;
}
my $nrecords = dbquery(@dborigin, dbRECORD_COUNT) ;
elog_die ( 0, "No matching origins found!\n" ) if $nrecords == 0 ;

my @dbsite = dblookup(@Db, 0, "site", 0, 0 ) ; 
#my @operational = ("dbjoin origin ondate::offdate#time", "dbseparate site") ; 
my @operational = ("dbjoin origin ondate::offdate#time", "dbseparate site", "dbjoin snetsta","dbsubset snet !~ /XX|Z3|ZS/ || (snet =~/Z3/ && sta =~ /A0.*A|D109/)", "dbseparate site" ) ; 
unshift(@operational, "verbose") if $opt_v ; 
@dbsite = dbprocess(@dbsite, @operational) ; 

my @process = ("dbjoin assoc", "dbjoin arrival", "dbseparate arrival", "dbjoin site", "dbseparate site") ; 

#my @process = ("dbjoin assoc", "dbjoin arrival", "dbseparate arrival", "dbjoin site", "dbjoin snetsta", "dbsubset snet !~ /XX|Z3|ZS/ || (snet =~/Z3/ && sta =~ /A0.*A|D109/)" , "dbseparate site") ; 
unshift(@process, "verbose") if $opt_v ; 
my @dbsite_subset = dbprocess(@dborigin_subset, @process) ; 

our $MW = MainWindow->new ;
$MW->setPalette(priority=>100, background=>"lightsteelblue") ;

$MW->fontCreate("site", -family=>"lucidabright", -size=>9) ;
$MW->fontCreate("origin", -family=>"lucidabright", -size=>12) ;
# $MW->setPalette("#e0e0e0") ;

# $MW->optionAdd ( "*Menu.tearOff", "0" );
# $MW->optionAdd ( "*Font", [ 'helvetica', '10', 'bold' ] );
# $MW->optionAdd ( "*Background", "#e0e0e0") ;
# $MW->optionAdd ( "*activeBackground", "darkgray") ;
# $MW->optionAdd ( "*activeForeground", "red") ;

my @specs = <DATA> ; 
ptkform($MW, \%Var, \%Widget, @specs) ; 
$Widget{Quit}->configure( -command=>sub { POSIX::_exit(0) ; }, -bg =>"red", -fg =>"white" ) if defined $Widget{Quit} ;

my $scrolled_canvas = $Widget{canvas} ;
$Canvas = $scrolled_canvas->Subwidget("canvas") ; 
my $width = $Canvas->reqwidth ;
my $height = $Canvas->reqheight ;
my @region=(0,0,$width, $height) ;
$Canvas->configure(-confine=>"yes", -scrollregion=>\@region) ; 
$Latlon = $Var{latlon} ;
$Viewport = "vp" ;

my ($latc, $lonc, $range) = get_range(@dborigin_subset, @dbsite_subset) ;
create_map($Canvas, $latc, $lonc, $range) ;


if ( defined $opt_d ) { 
    foreach my $name (split(',', $opt_d)) { 
	display_overlay($name, $Canvas) ;
    }
}

our ($Site, $Site_subset, $Origin, $Origin_subset, $Show_this_vector) ;

my ($symbol, $color, $size, $vector) ;
$symbol = pfget($Pf, "site_symbol") ;
$color = pfget($Pf, "site_color") ;
$size = pfget($Pf, "site_size" ) ;
$Site = show_site_records($Canvas, $color, 0, $symbol, $size, @dbsite) ;
$Db{$Site} = \@dbsite ;
$Site_subset = show_site_records($Canvas, $color, 1, $symbol, $size, @dbsite_subset) ;
$Db{$Site_subset} = \@dbsite_subset ;

$symbol = pfget($Pf, "other_origin_symbol") ;
$color = pfget($Pf, "other_origin_color") ;
$size = pfget($Pf, "other_origin_size" ) ;
$Show_this_vector = $Origin = show_origin_records($Canvas, 0, $color, $symbol, $size, @dborigin) ;
$Db{$Origin} = \@dborigin ;
no warnings 'experimental::smartmatch'; # Niko, get rid of warning on "smartmatch"
if (! (@dborigin ~~ @dborigin_subset) ) {
    $symbol = pfget($Pf, "origin_symbol") ;
    $color = pfget($Pf, "origin_color") ;
    $size = pfget($Pf, "origin_size" ) ;
    $Origin_subset = show_origin_records($Canvas, 1, $color, $symbol, $size, @dborigin_subset) ;
    $Db{$Origin_subset} = \@dborigin_subset ;
}
use warnings;

$MW->resizable (0, 0) ;

our $Balloon = $MW->Balloon (
                    -state => 'balloon',
		    -balloonposition => 'widget',
		    -initwait => 10,
		 ) ;
our $Linger = pfget($Pf, "balloon_linger_seconds") * 1000 ;

MainLoop ;

sub create_map { 
    my ($canvas, $latc, $lonc, $radius) = @_ ;
    printf STDERR "create map latc=%8.3f lonc=%8.3f radius=%8.2f \n", $latc, $lonc, $radius if $opt_v ;
    my $width = $canvas->reqwidth() ;
    my $height = $canvas->reqheight() ;
    my $water_color = pfget($Pf, "water_color") ;
    $canvas->create ( 'bpviewport', $Viewport, 0, 0,
		    -wtran => 'edp',
		    -latr => $latc,
		    -lonr => $lonc,
		    -width => $width,
		    -height => $height,
		    -xleft => -$radius,
		    -xright => $radius,
		    -ybottom => -$radius,
		    -ytop => $radius,
		    -fill => 'gray',
		    -fill_frame => '#ecffec',
		    -tags => [$Viewport, 'map'],
	     ) ;

    $canvas->create ( 'bpmap', $Viewport,
		    -resolution => 'auto',
		    -political => '1:#ff0000:1,2:#00a000:0,3:#ff00ff:0',
		    -fill_water => $water_color,
		    -tags => 'map',
	    ) ;

    $canvas->create ( 'bpaxes', $Viewport,
		    -mindx => 100,
		    -xincrement => 30.0,
		    -yincrement => 30.0,
		    -xincrement_small => 5.0,
		    -yincrement_small => 5.0,
		    -tags => 'map',
	    ) ;

    $canvas->create ( 'bpgrid', $Viewport,
		    -mindx => 0,
		    -mindy => 0,
		    -xincrement => 30.0,
		    -yincrement => 30.0,
		    -xincrement_small => 5.0,
		    -yincrement_small => 5.0,
		    -linewidth => -1,
		    -linewidth_small => 1,
		    -fill => 'darkgray',
		    -fill_small => 'darkgray',
		    -tags => 'map',
	    ) ;

    #    assign canvas bindings for mouse and
    #    keyboard events
    #    O,o  = zoom out
    #    I,i  = zoom in
    #    but3-drag = pan map
    #    Shift-but3 = pan map by resetting center lat-lon
    #    motion = display coords

    $canvas->CanvasBind ( '<KeyPress>' => \&bindzoommap ) ;
    $canvas->CanvasBind ( '<ButtonPress-3>' => \&bindstartdrag ) ;
    $canvas->CanvasBind ( '<Button3-Motion>' => \&binddrag ) ;
    $canvas->CanvasBind ( '<ButtonRelease-3>' => \&bindstopdrag ) ;
    $canvas->CanvasBind ( '<Shift-ButtonPress-3>' => \&bindpanmap ) ;
    $canvas->CanvasBind ( '<ButtonPress-1>' => \&bindfindclosest ) ;
    $canvas->CanvasBind ( '<Motion>' => [ \&showlatlon, \$Var{latlon}] ) ;
    $canvas->CanvasBind ( '<Any-Enter>' => \&bindenter ) ;
}

#    this procedure sets a color specification
#    for transforming a depth value into a color
sub setcolor {
    my $depth = shift ;
    my $symlit = 0.75 ;
    my $hue  = 240.0*(1.0-($depth/600.0)) ;
    return sprintf '%.1f:%s:1.0', $hue, $symlit ;
}

sub show_overlay_records { 
    my ($canvas, $table, $color, $symbol, $size, $label) = @_ ;
    my @db = dbopen_table($table, "r") ;
    my $nrecs = dbquery(@db, dbRECORD_COUNT) ;
    our $vector = vector_create ;
    print STDERR "show_overlay_records $nrecs records : color=$color symbol=$symbol size=$size label=$label\n" if $opt_v ;
    my ($lat, $lon, $lval) ;
    for ($db[3] = 0; $db[3] < $nrecs; $db[3]++) {
	if ( defined $label ) { 
	    ($lat, $lon, $lval) = dbgetv(@db, 'lat', 'lon', $label) ;
	} else { 
	    ($lat, $lon) = dbgetv(@db, 'lat', 'lon') ;
	}
	if ( defined $lval && $lval !~ /^\s*$/ ) { 
	    vector_append ( $vector, -1, $lon, $lat, $lval ) ;
	} else { 
	    vector_append ( $vector, -1, $lon, $lat ) ;
	}
    }
    $canvas->create ( 'bppolypoint', $Viewport,
		    -vector => $vector,
		    -symbol => $symbol,
		    -fill => $color,
		    -outline => $color,
		    -size => $size,
		    -textforeground=>$color,
		    -font => "overlay",
		    -showtext => 1,
	    ) ;
    return $vector ;
}

sub display_overlay { 
    my ($name, $canvas) = @_ ; 
    my $specs = pfget($Pfdbloc, "overlays{$name}") ;
    if ( ! defined $specs ) { 
	$specs = pfget($Pf, "overlays{$name}") ; 
    }
    my $vector ;
    if ( ! defined $specs ) { 
	print STDERR "can't find specs for overlay database tables $name\n" ;
    } else { 
	print STDERR "showing overlay $name : $specs\n" ; 
	my ($table, $color, $symbol, $size, $label) = split(" ", $specs) ;
	$vector = show_overlay_records($canvas, $table, $color, $symbol, $size, $label) ;
    }
    return $vector ;
}

sub show_site_records { 
    my ($canvas, $color, $fill, $symbol, $size, @db) = @_ ;
    my $nrecs = dbquery(@db, dbRECORD_COUNT) ;
    print STDERR "show_site_records $nrecs records : color=$color symbol=$symbol size=$size\n" if $opt_v ;
    our $vector = vector_create ;
    for ($db[3] = 0; $db[3] < $nrecs; $db[3]++) {
	my ($lat, $lon, $sta) = dbgetv(@db, 'lat', 'lon', 'sta') ;
	vector_append ( $vector, -1, $lon, $lat, $sta ) ;
    }
    if ( $fill ) { 
	$canvas->create ( 'bppolypoint', $Viewport,
			-vector => $vector,
			-symbol => $symbol,
			-fill => $color,
			-outline => $color,
			-size => $size,
			-textforeground=>$color,
			-font => "site",
			-showtext => 1,
		) ;
    } else { 
	$canvas->create ( 'bppolypoint', $Viewport,
			-vector => $vector,
			-symbol => $symbol,
			-outline => $color,
			-size => $size,
			-textforeground=>$color,
			-font => "site",
			-showtext => 1,
		) ;
    }
    return $vector ;
}

sub show_origin_records { 
    my ($canvas, $showtext, $color, $symbol, $size, @db) = @_ ;
    my $nrecs = dbquery(@db, dbRECORD_COUNT) ;
    our $vector = vector_create ;
    print STDERR "show_origin_records db=(@db) $nrecs records : color=$color symbol=$symbol size=$size\n" if $opt_v ;
    for ($db[3] = 0; $db[3] < $nrecs; $db[3]++) {
	my ($lat, $lon, $orid, $depth) = dbgetv(@db, 'lat', 'lon', "orid", 'depth') ;
	# my $color = setcolor($depth) ;
	# vector_append ( $vector, -1, $lon, $lat, sprintf ( '{f=%s}', $color ) ) ;
	vector_append ( $vector, -1, $lon, $lat, "$orid" ) ;
	# print STDERR "adding $lat, $lon  $color $size\n" ; 
    }
    $canvas->create ( 'bppolypoint', $Viewport,
		    -vector => $vector,
		    -symbol => $symbol,
		    -fill => $color,
		    -outline => $color,
		    -size => $size,
		    -textforeground=>$color,
		    -font => "origin",
		    -showtext => $showtext,
	    ) ;
    return $vector ;
}

sub subset_event { 
    my ($evid, @db) = @_ ; 
    @db = dblookup(@db, 0, "event", 0, 0) ; 
    @db = dbsubset(@db, "evid == $evid" ) ; 
    my @dbo = dblookup(@db, 0, "origin", 0, 0) ; 
    @db = dbjoin (@db, @dbo) ; 
    return @db ; 
}

sub subset_origin { 
    my ($orid, @db) = @_ ; 
    @db = dblookup(@db, 0, "origin", 0, 0) ; 
    @db = dbsubset(@db, "orid == $orid" ) ; 
    return @db ; 
}


sub get_range { 
    my (@dborigin) = @_[0..3] ; 
    my (@dbsite) = @_[4..7] ;
    my (@coords) = () ;
    my $nrecs = dbquery(@dborigin, dbRECORD_COUNT) ;
    for ($dborigin[3] = 0; $dborigin[3] < $nrecs; $dborigin[3]++) {
	    my ($lat, $lon ) = dbgetv(@dborigin, 'lat', 'lon') ;
	    push (@coords, $lat, $lon) ; 
    }
    $nrecs = dbquery(@dbsite, dbRECORD_COUNT) ;
    for ($dbsite[3] = 0; $dbsite[3] < $nrecs; $dbsite[3]++) {
	    my ($lat, $lon ) = dbgetv(@dbsite, 'lat', 'lon') ;
	    push (@coords, $lat, $lon) ; 
    }
    my ($latc, $lonc, $radius)  = mapcenter(@coords) ;
    return ($latc, $lonc, $radius) ;
}

__DATA__
canvas canvas   800x800 0,0
frame  controls -     +,0
label  help     -     0,0 I/i=zoom in, O/o=zoom out, right-click-drag
label  latlon	-     =,+ 
#button  Quit	-     +,2 Quit
endframe
#frame buttons   -     +,0    
#button  print	-     0,0  Print
#button  postscript -  =,+  Postscript
#button	Quit	-     =,3  Quit 
#endframe
