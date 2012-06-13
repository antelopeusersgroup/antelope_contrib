#   Copyright (c) 2004 Boulder Real Time Technologies, Inc.           
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Getopt::Std ;
use Datascope;

sub exec_cmd {
	my( $cmd ) = @_;

	if( $opt_V ) {

		print STDERR "Executing $cmd\n"
	} 

	system( "$cmd" );

	return;
}
 
sub check_for_executable {
        my( $program ) = @_;

        my( $ok ) = 0;

        foreach $path ( split( ':', $ENV{'PATH'} ) ) {
                if( -x "$path/$program" ) {
                        $ok = 1;
                        last;
                }
        }

        return $ok;
}

my $pgm = $0 ; 
$pgm =~ s".*/"" ;
$Pfname = $pgm;

if ( ! getopts('R:V') || @ARGV != 0 ) { 

	die ( "Usage: $pgm [-V] -Rw/e/s/n\n" ) ; 

} elsif( ! $opt_R ) {

	die ( "Usage: $pgm [-V] -Rw/e/s/n\n" ) ; 

} else {

	$R = "-R" . $opt_R;

	$V = $opt_V ? "-V" : "";
}

@helpers = (
	   "dbgmtgrid",
	   "grd2cggrid",
	   "grdimage",
	   "grdinfo",
	   "grdlandmask",
	   "grdmath",
	   "grdgradient", 
	   "grd2xyz",
	   "makecpt",
	   "psbasemap",
	   "pscoast",
	   );

foreach $helper ( @helpers ) {
	next if check_for_executable( $helper );
	die( "Can't find the program '$helper' on the path.\n" );
}

$grid_database = pfget( $Pfname, "grid_database" );
$grid_spacing = pfget( $Pfname, "grid_spacing" );
$J = pfget( $Pfname, "projection" );
$gradient_options = pfget( $Pfname, "gradient_options" );
$vs30_min_mps = pfget( $Pfname, "vs30_min_mps" );
$vs30_range_mps = pfget( $Pfname, "vs30_range_mps" );
$vs30_colormap_interval_mps = pfget( $Pfname, "vs30_colormap_interval_mps" );
$psbasemap_boundary = pfget( $Pfname, "psbasemap_boundary" );
$colormap_name = pfget( $Pfname, "colormap" );
$ocean_color = pfget( $Pfname, "ocean_color" );

$vs30_max_mps = $vs30_min_mps + $vs30_range_mps;

$topo_grid = "fake.grd";
$landmask = "fake_landmask.grd";
$masked = "fake_masked.grd";
$masked_gradient = "fake_masked.grad";
$colormap_file = "vscolors.cpt";

$output_grd = "fakevs30.grd";
$output_cggrid = "fakevs30.xyz";
$output_ps = "fake.ps";

@commands = (
	"dbgmtgrid $V $R $grid_database $topo_grid",
	"grdlandmask -G$landmask $R -I$grid_spacing -F",
	"grdmath $topo_grid $landmask MUL = $masked",
	"grdgradient $masked -G$masked_gradient $gradient_options",
	"grdmath $masked_gradient ABS $vs30_range_mps MUL $vs30_min_mps ADD = $output_grd",
	"psbasemap $V -P $R $J -B$psbasemap_boundary -K > fake.ps",
	"makecpt -C$colormap_name -I -T$vs30_min_mps/$vs30_max_mps/$vs30_colormap_interval_mps > $colormap_file",
	"grdimage $output_grd $V $R $J -O -K -C$colormap_file >> fake.ps",
	"pscoast $V $R $J -O -S$ocean_color >> $output_ps",
	"grd2cggrid $V $output_grd $output_cggrid"
	);

foreach $cmd ( @commands ) {
	exec_cmd $cmd;
}

exit( 0 );
