use Getopt::Std;

use Datascope;
use atws_mwp ':all';

use Tk;
use Tk::LineGraph;
use Tk::LineGraphDataset;

my $curStation = 0;
my $tmpdatabase = "/tmp/mwp_DB";
my @stations;
my @chans;
my @starttimes = 0;
my @endtimes = 0;
my @sta_dat;
my @sta_mag;
my @sta_mom;
my @sta_xarr;

my @dat;
my @mag;
my @mom;

if($#ARGV == 1)
	{
	$database = $ARGV[0];
	$orid = $ARGV[1];
	}
elsif($#ARGV == 5)
	{
	$database = $ARGV[0];
	$orid = $ARGV[1];
	$stations[0] = $ARGV[2];
	$chans[0] = $ARGV[3];
	$starttimes[0] = $ARGV[4];
	$endtimes[0] = $ARGV[5];
	}
else
	{
	print "Wrong number of arguments supplied.\n";
	print "You must supply DBPath, origin id, sta, chan, starttime, endtime.\n";
	exit 0;
	}
	


my $mw = MainWindow->new;

@db = dbopen($database,"r+");
dbcreate($tmpdatabase, "css3.0","/tmp/", 0, 0);
@tmpdb = dbopen($tmpdatabase,"r+");

if(!length($stations[0]))
	{
	@db_assoc = dblookup(@db, 0, "assoc", 0, 0);
	@db_assoc = dbsubset(@db_assoc,"orid == $orid");
	@db_arrival = dblookup(@db, 0, "arrival", 0, 0);

	@dbs = dbjoin(@db_assoc, @db_arrival);
	@dbs = dbsubset(@dbs, "chan != 'SHZ'");
	@dbs = dbsort(@dbs, "delta");
	
	$no_records_join =  dbquery (@dbs,"dbRECORD_COUNT") ;
	print "Records Found = $no_records_join\n";
	
	for($i = 0; $i < $no_records_join; $i++)
		{
		$dbs[3]=$i;
		($sta,$chn,$time, $dlt) =  dbgetv(@dbs,"sta","chan","arrival.time", "delta");
		$stations[$i] = $sta;
		$chans[$i] = $chn;
		$starttimes[$i] = int($time - 10);
		$endtimes[$i] = int($time + 120);
		$delta[$i] = sprintf(" Delta:%.1f", $dlt);
		}
	}

$x = 0;
foreach $sta (@stations)
	{
	@dbf = dblookup(@db, 0, "wfdisc", 0, 0);
	@dbwf = dbsubset(@dbf,"sta == \"$sta\" && chan == \"${chans[$x]}\"");
	$no_records_join =  dbquery (@dbwf,"dbRECORD_COUNT") ;
	print "Records Found = $no_records_join\n";
	if($no_records_join < 1)
		{
		$x++;
		print "No Records found for STA: $sta\n";
		next;
		}
		
	(@values) = mwp_prepare($dbwf[0] ,$dbwf[1],$dbwf[2],$dbwf[3], $orid, $sta, $starttimes[$x],$endtimes[$x]); 

	$i = 0;

	$nrecs = (scalar @values);

#	print DBG "MWP run at ". now() . " for " . $chans[$x] . " times=${starttimes[$x]},${endtimes[$x]}\n";

	for($i = 0; $i < $nrecs; undef)
		{
	#	print DBG ($i / 3) . ", " . $values[$i + 0] . ", " . $values[$i + 1] . ", " . $values[$i + 2] . "\n";
		push @{$sta_dat[$x]}, $values[$i];$i++;
		$values[$i] = 0 if($values[$i] < 0);
		push @{$sta_mag[$x]}, $values[$i];$i++;
		push @{$sta_mom[$x]}, $values[$i];$i++;
		push @{$sta_xarr[$x]}, ($i/3);
		}
	$stamwp[$x] = -999.0;
	$stapnt[$x] = 0;
	$x++;
	}


for($x = 0; $x < scalar @sta_dat; $x++)
	{
	$num_mom = scalar @{$sta_mom[$x]};

	$min_mom = 9e99;
	$max_mom = -9e99;

	if($num_mom == 1)
		{
		next;
		}

	for($i = 0; $i < $num_mom; $i++)
		{
		$min_mom = $sta_mom[$x][$i] if($sta_mom[$x][$i] < $min_mom);
		$max_mom = $sta_mom[$x][$i] if($sta_mom[$x][$i] > $max_mom);
		}
	
	print "Moment = $min_mom -> $max_mom\n";
	if(($min_mom > -9e99) && ($max_mom < 9e99))
		{
		$range = $max_mom - $min_mom;
		$scale = ($range) ? 10.0 / ($max_mom - $min_mom) : 1;
		}
	else
		{
		$scale = 1;
		}

#	open(DBG, ">/tmp/mwp/${stations[$x]}_${orid}_dbg.txt");
	for($i = 0; $i < $num_mom; $i++)
		{
		$sta_mom[$x][$i] -= $min_mom;
		$sta_mom[$x][$i] *= $scale;
#		print DBG "$i,${sta_mag[$x][$i]},${sta_mom[$x][$i]}\n";
		}
#	close(DBG);
	}

my $cp = $mw->LineGraph
  (
   -height => 600,
   -width  => 800,
   background => 'snow1',

     )->grid;

$cp->Tk::bind("<Button-1>" , ""   );
$cp->Tk::bind("<ButtonRelease-1>" , ""   );
$cp->Tk::bind("<B1-Motion>" , ""   );

$cp->Tk::bind("<Button-2>" , ""   );
$cp->Tk::bind("<ButtonRelease-2>" , ""   );



$cp->Tk::bind("<ButtonRelease-1>" , [  \&markMag, 1 ]   );
$mw->Tk::bind("<Escape>" , [  sub {$mw->destroy}, 1 ]   );

$cp->Tk::bind("<Button-2>" ,        [  \&Tk::LineGraph::zoom, 0 ]   );
$cp->Tk::bind("<ButtonRelease-2>" , [  \&Tk::LineGraph::zoom, 1 ]   );
$cp->Tk::bind("<B2-Motion>" ,       [  \&Tk::LineGraph::zoom, 2 ]   );


my $ds1 = LineGraphDataset->new(-yData=>\@{$sta_dat[0]}, -xData=>\@{$sta_xarr[0]},  -yAxis=>"Y1", -name=>"velocity(nm/s)");
$cp->addDatasets(-dataset=>$ds1);

my $ds2 = LineGraphDataset->new(-yData=>\@{$sta_mag[0]}, -xData=>\@{$sta_xarr[0]}, -yAxis=>"Y", -name=>"magnitude");
$cp->addDatasets(-dataset=>$ds2);

my $ds3 = LineGraphDataset->new(-yData=>\@{$sta_mom[0]}, -xData=>\@{$sta_xarr[0]}, -yAxis=>"Y", -name=>"moment");
$cp->addDatasets(-dataset=>$ds3);

my $menubar = $mw->cget("-menu");
#my $fileMenu = $menubar->cascade(-label => 'File',-tearoff=>0);

if(scalar @stations > 1)
	{
	$menubar->command(-label=>'Back',-command=>     [ \&prevSta, 0]);
	$menubar->command(-label=>'Next',-command=>     [ \&nextSta, 0]);
	$menubar->command(-label=>'Save Netmag',-command=>     [ \&saveNetMag, 0]);
	}
else
	{
	$menubar->command(-label=>'Save Stamag',-command=>     [ \&saveStaMag, 0]);
	}
$menubar->command(-label=>'Quit',-command=>       [ \&exit, 0]);

$starttimes[0] = sprintf("%.1f",$starttimes[0]); 
$endtimes[0] = sprintf("%.1f",$endtimes[0]); 

my $p = $cp->cget('-plotTitle');
$p->[0] = "Station " . ($curStation + 1) . "/" . (scalar @stations) . " --> ${stations[$curStation]}:${chans[$curStation]}${delta[$curStation]} from ${starttimes[$curStation]} to ${endtimes[$curStation]}";
$p->[1] = 4;
$cp->configure('-plotTitle' => $p);
$cp->configure('-xlabel' => "Sample Number");
$cp->configure('-ylabel' => "Magnitude");
$cp->configure('-y1label' => "Velocity nm/s");

$cp->plot();


MainLoop;

sub markMag
	{
	my $self = shift;
  	my $e = $cp->XEvent;
  	
  	($xC,$yC) = ($e->x, $e->y);
    my($xW, $yW, $y1W) = $cp->toWorldPoints($xC, $yC);
    $xW = int($xW);
    ($lne) = $cp->find( "withtag", "pointer" );
  	
  	$dp = $cp->{'-datasets'}[1]->get('-yData');
  	@ds = @{$dp};
  	
  	$stamwp[$curStation] = sprintf("%01.3f", $ds[$xW]);
  	$stapnt[$curStation] = $e->x;
  	
  	if(!defined($lne))
  		{
  		$cp->createLine($e->x, 25, $e->x, 500, -fill => 'black', -tags => 'pointer');
		}
  	else
  		{
  		($oldx, $oldy) = $cp->coords("pointer");
  		$cp->move("pointer", ($e->x - $oldx), 0);
  		}
  	}
  	
sub saveStaMag
	{
	if($stamwp[$curStation] > 0)
		{
		#print "Saving Sta Mag: $stamag\n";
		@db_stamag = dblookup(@tmpdb, 0, "stamag", 0, 0);
		@db_stamag2 = dbsubset(@db_stamag, "(orid == $orid && sta == '${stations[0]}' && magtype == 'Mwp')");
			
		
		$no_records_join =  dbquery (@db_stamag2,"dbRECORD_COUNT") ;
		#print "Records Found = $no_records_join\n";

		$name = getlogin;
		
		if($no_records_join < 1)
			{
			dbaddv(@db_stamag, "magid", 0, "sta", "${stations[0]}", "orid", $orid, "magtype", "Mwp", "magnitude", $stamwp[$curStation], "auth", $name );
			#print "Added New Record\n";
			}
		else
			{
			$recnum = dbfind(@db_stamag, "(orid == $orid && sta == '${stations[0]}' && magtype == 'Mwp')", -1);
			if($recnum >= 0)
				{
				$db_stamag[3] = $recnum;
				}
			
			dbputv(@db_stamag, "magtype", "Mwp", "magnitude", $stamwp[$curStation], "auth", $name );
			#print "Updated Existing Record\n";
			}
		exit;
		}
	}

sub saveNetMag
	{
	$mwpcnt = 0;
	$mwpavg = 0;
	$mwptot = 0;
	$mwpdev = 0;
	
	my %results;
	
	@db_netmag = dblookup(@tmpdb, 0, "netmag", 0, 0);
	@db_netmag2 = dbsubset(@db_netmag, "(orid == $orid && magtype == 'Mwp')");
	
	$nrec =  dbquery (@db_netmag2,"dbRECORD_COUNT");
	if($nrec)
		{
		$db_netmag2[3] = 0;
		($magid) =  dbgetv(@db_netmag2,"magid");
		$oldnetmag = 1;
		}
	else
		{
		$magid = dbnextid(@tmpdb, "magid");
		$oldnetmag = 0;
		}
		
	@db_stamag = dblookup(@tmpdb, 0, "stamag", 0, 0);
	@db_stamag2 = dbsubset(@db_stamag, "(orid == $orid && magid == $magid)");

	$nrec = dbquery(@db_stamag2, "dbRECORD_COUNT");
	for($x = 0; $x < $nrec; $x++)
		{
		$db_stamag2[3] = $x;
		($sta, $mag, $auth) = dbgetv(@db_stamag2, "sta", "magnitude", "auth");
		$results{$sta}{"mag"} = $mag;
		$results{$sta}{"auth"} = $auth;
		}

	for ($x = 0; $x < scalar @stations; $x++)
		{
		if($stamwp[$x] > 0.0)
			{
			$results{$stations[$x]}{"mag"} = $stamwp[$x];
			$results{$stations[$x]}{"auth"} = getlogin;
			}
		}

	foreach $sta (keys %results)
		{
		print "Station $sta: Magnitude = ${results{$sta}{'mag'}}\n";
		$mwpcnt++;
		$mwptot += $results{$sta}{'mag'};
		}
		
	if($mwpcnt)
		{
		$mwpavg = sprintf("%01.3f", $mwptot / $mwpcnt);
		foreach $sta (keys %results)
			{
			$mwpdev += (($mwpavg - $results{$sta}{'mag'}) * ($mwpavg - $results{$sta}{'mag'}));
			}
		$mwpdev = sprintf("%01.3f", sqrt($mwpdev / $mwpcnt));
		}
	
	print "Network Magnitude is $mwpavg from $mwpcnt Stations, Uncertainty = $mwpdev\n";
	
	# find the event ID
	@db_origin = dblookup(@db, 0, "origin", 0, 0);
	@db_origin = dbsubset(@db_origin, "orid == $orid");

	$db_origin[3] = 0;
	($evid) = dbgetv(@db_origin, "evid");
	
	#save netmag row
	if ($oldnetmag)
		{
		$nm_rec = dbfind(@db_netmag, "magid == $magid", -1);
		$db_netmag[3] = $nm_rec;
		#print "Putting Netmag For $magid\@$nm_rec\n";
		dbputv(@db_netmag, "magid", $magid, "orid", $orid, "evid", $evid, "magtype", "Mwp", "nsta", $mwpcnt, "magnitude", $mwpavg, "uncertainty", $mwpdev, "auth", getlogin);
		} 
	else 
		{
		dbaddv(@db_netmag,"magid", $magid, "orid", $orid, "evid", $evid, "magtype", "Mwp", "nsta", $mwpcnt, "magnitude", $mwpavg, "uncertainty", $mwpdev, "auth", getlogin);
		}

	foreach $sta (keys %results)
		{
		$nm_rec = dbfind(@db_stamag, "(orid == $orid && sta == '$sta' && magtype == 'Mwp')", -1);
		if($nm_rec >= 0)
			{
			$db_stamag[3] = $nm_rec;
			#print "Putting Stamag For $magid:$sta\@$nm_rec\n";
			dbputv(@db_stamag, "magid", $magid, "orid", $orid, "evid", $evid, "magtype", "Mwp", "sta", $sta, "magnitude", $results{$sta}{'mag'}, "auth", $results{$sta}{'auth'});
			}
		else 
			{
			dbaddv(@db_stamag, "magid", $magid, "orid", $orid, "evid", $evid, "magtype", "Mwp", "sta", $sta, "magnitude", $results{$sta}{'mag'}, "auth", $results{$sta}{'auth'});
			}
		}
	}

sub prevSta
	{
	#print "curstation = $curStation (${stations[$curStation]})\n";
	$curStation--;
	$curStation = ((scalar @stations) - 1) if($curStation < 0);
	
	$cp->{'-datasets'}[0]->set(-yData => \@{$sta_dat[$curStation]}, -xData=>\@{$sta_xarr[$curStation]});
	$cp->{'-datasets'}[1]->set(-yData => \@{$sta_mag[$curStation]}, -xData=>\@{$sta_xarr[$curStation]});
	$cp->{'-datasets'}[2]->set(-yData => \@{$sta_mom[$curStation]}, -xData=>\@{$sta_xarr[$curStation]});
	
	my $p = $cp->cget('-plotTitle');
	$p->[0] = "Station " . ($curStation + 1) . "/" . (scalar @stations) . " --> ${stations[$curStation]}:${chans[$curStation]}${delta[$curStation]} from ${starttimes[$curStation]} to ${endtimes[$curStation]}";
	$p->[1] = 4;
	$cp->configure('-plotTitle' => $p);

	
	$cp->plot();

	if($stapnt[$curStation])
		{
		$cp->createLine($stapnt[$curStation], 25, $stapnt[$curStation], 500, -fill => 'black', -tags => 'pointer');
		}

	#print "curstation = $curStation (${stations[$curStation]})\n";
	}

sub nextSta
	{
	#print "curstation = $curStation (${stations[$curStation]})\n";
	$curStation++;
	$curStation = 0 if ($curStation == (scalar @stations));
	
	$cp->{'-datasets'}[0]->set(-yData => \@{$sta_dat[$curStation]}, -xData=>\@{$sta_xarr[$curStation]});
	$cp->{'-datasets'}[1]->set(-yData => \@{$sta_mag[$curStation]}, -xData=>\@{$sta_xarr[$curStation]});
	$cp->{'-datasets'}[2]->set(-yData => \@{$sta_mom[$curStation]}, -xData=>\@{$sta_xarr[$curStation]});
	
	my $p = $cp->cget('-plotTitle');
	$p->[0] = "Station " . ($curStation + 1) . "/" . (scalar @stations) . " --> ${stations[$curStation]}:${chans[$curStation]}${delta[$curStation]} from ${starttimes[$curStation]} to ${endtimes[$curStation]}";
	$p->[1] = 4;
	$cp->configure('-plotTitle' => $p);

	$cp->plot();

	if($stapnt[$curStation])
		{
		$cp->createLine($stapnt[$curStation], 25, $stapnt[$curStation], 500, -fill => 'black', -tags => 'pointer');
		}

	#print "curstation = $curStation (${stations[$curStation]})\n";

	}
