<?php
# Include relevant libraries from JpGraph
$jppath = "/opt/src/jpgraph-1.17/src/" ;
include( $jppath . "jpgraph.php" ) ; # Load JpGraph
include( $jppath . "jpgraph_line.php" ) ; # Line plot

function wfplot( $mydb, $sta, $cha, $t0, $t1, $myimage, $width, $height, $format ) {
	if( empty( $mydb ) || empty( $sta ) || empty( $cha ) || empty( $t0 ) || empty( $t1 ) || empty( $myimage ) || empty( $width ) || empty( $height ) || empty( $format ) ) {
		echo "<b>wfplot error: One of more of the required arguments were not defined!</b>" ;
		return ;
	} else {
		$mytr = trloadchan( $mydb, $t0, $t1, $sta, $cha ) ;
		for( $mytr[3]=0; $mytr[3]<dbnrecs( $mytr ); $mytr[3]++ ) {
			list( $time, $endtime, $nsamp, $samprate ) = dbgetv( $mytr, 'time','endtime','nsamp','samprate' ) ;
			$data_y = trextract_data( $mytr ) ;
			for( $i=0; $i<count( $data_y ); $i++ ) {
				$data_x[$i] = $time + ( $i * ( 1 / $samprate ) ) ;
			}
		}
		$mytr = dblookup( $mytr, '', '', '', 'dbALL' ); # free up the trace-object structure
		trfree( $mytr );
		function TimeCallback( $aVal ) { // convert timestamp to minutes and secs
		        return date( 'H:i:s', $aVal ) ;
		}
		$graph = new Graph( $width,$height ) ;
		$graph->img->SetAntiAliasing() ;
		$graph->img->SetImgFormat( $format ) ;
		$graph->SetScale("intlin",0,0,$time,$data_x[count($data_y)-1] ) ;
		$graph->img->SetMargin(60,40,40,80) ;
		$graph->SetMarginColor('#81818B') ;
		$graph->SetColor('#000099') ;
		$graph->xaxis->SetLabelFormatCallback( 'TimeCallback' ) ;
		$graph->title->SetMargin( 10 );
		$graph->title->SetColor('black');
		$graph->title->Align('center');
		$graph->title->SetBox('white','black',0,0,0);
		$graph->title->Set("Waveform Plot $sta:$cha\nStart time: $time End time: $endtime");
		$graph->xaxis->title->Set("Time");
		$graph->xaxis->title->SetColor( '#FFFFFF' );
		$graph->yaxis->title->Set("nm/sec");
		$graph->yaxis->title->SetColor( '#FFFFFF' );
		$graph->xaxis->SetPos("min");
		$graph->xaxis->SetLabelAngle(90);
		$graph->xaxis->SetLabelMargin( 5 );
		$graph->xaxis->SetColor("white");
		$graph->yaxis->SetTitleMargin( 40 );
		$graph->yaxis->SetColor("white");
		$graph->yaxis->HideFirstTicklabel();
		$graph->ygrid->Show(false,false) ;
		$graph->xgrid->Show(true,false) ;
		$graph->xgrid->SetLineStyle("dotted") ;
		$lp1 = new LinePlot( $data_y, $data_x );
		$lp1->SetWeight(1);
		$lp1->SetColor('#FFFF00');
		$graph->Add($lp1);
		$graph->Stroke( $myimage . "." . $format );
		$wfplot_image_name = $myimage . "." . $format ;
		return $wfplot_image_name ;
	}
}
?>
