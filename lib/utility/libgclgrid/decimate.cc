#include "gclgrid.h"
/*! \brief Decimate a GCLgrid3d.

We sometimes want to decimate a grid. This procedure does this for
a 3D grid with variable decimation for each generalized coordinate 
axis.  An added compliation is the fact that because GCLgrids hae
the x3 direction directed upward in earth coordinates and the free
surface is so special, we work the x3 coordinate backward compared
to the others.  i.e. we for the n3-1 point to be the n3-1 point in
the result, not the 0 points.  

\param g parent grid that is to be decimated
\param dec1 decimation factor for x1
\param dec2 decimation factor for x2
\param dec3 decimamtion factor for x3

\return pointer to decimated grid object 
*/
GCLgrid3d *decimate(GCLgrid3d& g,int dec1, int dec2, int dec3)
{
	int n1,n2,n3;
	n1=(g.n1)/dec1;
	n2=(g.n2)/dec2;
	n3=(g.n3)/dec3;
	if( (n1<dec1) || (n2<dec2) || (n3<dec3) )
	  throw GCLgrid_error(string("GLCgrid3d decimator:  decimation factor larger than grid dimension"));
	/* Call an appropriate constructor here.  May need to reset
	some attributes of the grid object */
	GCLgrid3d *result=new GCLgrid3d(n1,n2,n3);;

	int i,j,k,ii,jj,kk;
	for(i=0,ii=0;i<g.n1 && ii<n1;i+=dec1,++ii)
	{
		for(j=0,jj=0;j<g.n2 && jj<n2;j+=dec2,++jj)
		{
			// note reverse order 
			//for(k=0,kk=0;k<g.n3 && kk<n3; k+=dec3,++kk)
			for(k=g.n3-1,kk=n3-1;kk>=0; k-=dec3,--kk)
			{
				result->x1[ii][jj][kk]=g.x1[i][j][k];
				result->x2[ii][jj][kk]=g.x2[i][j][k];
				result->x3[ii][jj][kk]=g.x3[i][j][k];
			}
		}
	}
	/* We clone other attribures of the parent grid.*/
	result->lat0=g.lat0;
	result->lon0=g.lon0;
	result->r0=g.r0;
	result->azimuth_y=g.azimuth_y;
	result->x1low=g.x1low;
	result->x1high=g.x1high;
	result->x2low=g.x2low;
	result->x2high=g.x2high;
	result->x3low=g.x3low;
	result->x3high=g.x3high;
	result->dx1_nom=g.dx1_nom*(static_cast<double>(dec1));
	result->dx2_nom=g.dx2_nom*(static_cast<double>(dec2));
	result->dx3_nom=g.dx3_nom*(static_cast<double>(dec3));
	// Decimated grids have no simple way of guaranteeing the index values
	// will mesh with lat0,lon0,r0.  Coordinates are defined by lat0,lon0,r0
	// so beware this assumption about i0,j0,k0.  After this function is called
	// the two are highly likely to be inconsistent
	result->i0=g.i0/dec1;
	result->j0=g.j0/dec2;
	result->k0=g.k0/dec3;
	for(i=0;i<3;++i)
		for(j=0;j<3;++j) 
		    result->gtoc_rmatrix[i][j]=g.gtoc_rmatrix[i][j];
	for(i=0;i<3;++i)result->translation_vector[i]
			 = g.translation_vector[i];

	return(result);
}
