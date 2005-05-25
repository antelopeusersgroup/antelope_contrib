<?php

function wfplot( $db, $sta, $cha, $t0, $t1, $width, $height, $format, $output_image ) {

	if( ! class_exists( 'Graph' ) ) {

		echo "<b>wfplot error: class 'Graph' is not defined. Please " .
		     "make sure you have included jpgraph.php</b>" ;
		return;
	}

	if( ! class_exists( 'LinePlot' ) ) {

		echo "<b>wfplot error: class 'LinePlot' is not defined. Please " .
		     "make sure you have included jpgraph_line.php</b>" ;
		return;
	}

	if( empty( $db ) ) {

		echo "<b>wfplot error: The required argument 'db' is not defined!</b>" ;
		return ;

	} elseif( empty( $sta ) ) {

		echo "<b>wfplot error: The required argument 'sta' is not defined!</b>" ;
		return ;

	} elseif( empty( $cha ) ) {

		echo "<b>wfplot error: The required argument 'chan' is not defined!</b>" ;
		return ;

	} elseif( empty( $t0 ) ) {

		echo "<b>wfplot error: The required argument 't0' is not defined!</b>" ;
		return ;

	} elseif( empty( $t1 ) ) {

		echo "<b>wfplot error: The required argument 't1' is not defined!</b>" ;
		return ;

	} elseif( empty( $width ) ) {

		echo "<b>wfplot error: The required argument 'width' is not defined!</b>" ;
		return ;

	} elseif( empty( $height ) ) {

		echo "<b>wfplot error: The required argument 'height' is not defined!</b>" ;
		return ;

	} elseif( empty( $format ) ) {

		echo "<b>wfplot error: The required argument 'format' is not defined!</b>" ;
		return ;

	} else {
	
		$mytr = trloadchan( $db, $t0, $t1, $sta, $cha ) ;

		for( $mytr[3]=0; $mytr[3]<dbnrecs( $mytr ); $mytr[3]++ ) {

			list( $time, $endtime, $nsamp, $samprate ) =
				dbgetv( $mytr, 'time','endtime','nsamp','samprate' ) ;

			$data_y = trextract_data( $mytr ) ;

			for( $i=0; $i<count( $data_y ); $i++ ) {

				$data_x[$i] = $time + ( $i * ( 1 / $samprate ) ) ;
			}
		}

		$mytr = dblookup( $mytr, '', '', '', 'dbALL' ); 
		trfree( $mytr );

		// convert timestamp to minutes and secs

		function TimeCallback( $aVal ) { 
		        return gmdate( 'H:i:s', $aVal ) ;
		}

		$graph = new Graph( $width, $height ) ;
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

		$graph->Stroke( $output_image );
		
		return;

		// The preferred behaviour would be to return a blob:
		// ob_start();
		// imagepng($graph->Stroke(_IMG_HANDLER));
		// $blob = ob_get_contents();
		// ob_end_clean();

		// return $blob ;
	}
}
?>
