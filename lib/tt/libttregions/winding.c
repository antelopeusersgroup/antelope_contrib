#include <stdlib.h>
#include <stdio.h>

/* Reimplementation of winding-number algorithm

 Godkin and Pulli, BSSA v.74 1845-1948, 1984.

 K. Lindquist
 Geophysical Institute
 University of Alaska
 1999
*/

double
signed_crossing_number( double *segment )
{
	double	x1, y1, x2, y2;
	double	slope;
	double 	xintercept;
	double	direction;

	x1 = segment[0];
	y1 = segment[1];
	x2 = segment[2];
	y2 = segment[3];

	if( y1 * y2 > 0 )
	{
		/* No crossing--both points on same side of y axis */
		return 0;
	}

	/* Horizontal lines: */
	if( y1 == 0 && y2 == 0 )
	{
		if( x1 * x2 > 0 )
		{
			/* No crossing */
			return 0;
		}
		else
		{
			/* Segment crosses or touches origin */
			return 2;
		}
	}

	if( x2 == x1 )
	{
		/* Vertical lines */
		xintercept = x1;
	}
	else 
	{
		slope = ( y2 - y1 ) / (x2 - x1 );
		xintercept = x1 - y1 / slope;
	}

	if( y2 > y1 )
	{
		direction = 1;
	}
	else 
	{
		direction = -1;
	}	

	if( xintercept > 0 )
	{
		return 0;

	}
	else if( xintercept == 0 ) 
	{
		
		return 2;
	}
	else if( y1 == 0 || y2 == 0 )
	{

		return 0.5 * direction;
	} else
	{
		return direction;
	}
}

double *
segment_number( int seg_index, double *shifted_polygon, int ncoords )
{
	int     nsegs;
	double	*segment;

	nsegs = ncoords / 2;

	if( seg_index > nsegs )
	{
		fprintf( stderr, "Bad segment number\n" );
		return NULL;
	}

	segment = (double *) malloc( 4 * sizeof( double ) );

	if( seg_index == nsegs )
	{
		segment[0] = shifted_polygon[ncoords-2];
		segment[1] = shifted_polygon[ncoords-1];
		segment[2] = shifted_polygon[0];
		segment[3] = shifted_polygon[1];
	} else {
		segment[0] = shifted_polygon[(seg_index-1)*2];
		segment[1] = shifted_polygon[(seg_index-1)*2+1];
		segment[2] = shifted_polygon[(seg_index-1)*2+2];
		segment[3] = shifted_polygon[(seg_index-1)*2+3];
	}

	return segment;
}

double
winding_number( double *shifted_polygon, int ncoords )
{
	double	winding_num = 0;
	double	*segment;
	int	nsegs;
	int	seg_index;

	nsegs = ncoords / 2;
	
	for( seg_index = 1; seg_index <= nsegs; seg_index++ ) 
	{
		segment = segment_number( seg_index, shifted_polygon, ncoords );
		winding_num += signed_crossing_number( segment );
		free( segment );
	}

	return winding_num;
}

double *
shift_polygon( double x, double y, double *polygon, int ncoords )
{
	double *shifted_polygon;
	int	nsegs;
	int	seg_index;
	int	x_index, y_index;

	nsegs = ncoords / 2;
	shifted_polygon = (double *) malloc( ncoords * sizeof( double ) );

	for( seg_index = 0; seg_index < nsegs; seg_index++ )
	{
		x_index = 2 * seg_index;
		y_index = 2 * seg_index + 1;

		shifted_polygon[x_index] = polygon[x_index] - x;
		shifted_polygon[y_index] = polygon[y_index] - y;
	}

	return shifted_polygon;
}

int
is_inside( double x, double y, double *polygon, int ncoords )
{
	double *shifted_polygon;
	int	winding_num;

	shifted_polygon = shift_polygon( x, y, polygon, ncoords );

	winding_num = winding_number( shifted_polygon, ncoords );

	free( shifted_polygon );

	if( winding_num )
	{
		return 1;

	} else {

		return 0;
	}
}
