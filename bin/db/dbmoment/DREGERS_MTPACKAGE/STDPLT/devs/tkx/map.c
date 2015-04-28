#include	<stdio.h>

float	xinch	=10.0;
float	yinch	= 7.5;
float	xbord	=0.25;
float	ybord	=0.50;
float inch	=9.0;
float xorig	=0.5;
float yorig	=0.5;

main()
   {
	float lat1, lon1, lat2, lon2;
	int c;

	map(-180.0,-90.0,180.0,90.0);

	while( (c=getubox(&lon1,&lat1,&lon2,&lat2)) != '\0')
	   {
		switch(c)
		   {
			case 'o':	/* original */
				map(-180.0,-90.0,180.0,90.0);
				break;
			case 's':
				map(lon1,lat1,lon2,lat2);
				break;
			case 'q':
				goto last;
		   }
	   }
last:
	plot(1.0,1.0,0);
	fprintf(stdout,"\n");
   }

map(lon1,lat1,lon2,lat2)
float lon1,lat1,lon2,lat2;
   {
	float scl1, scl2;
	int ilat1, ilon1, ilat2, ilon2, ilat, ilon;
	int range1, range2, range, inc;
	double fabs();

	mapscale(lon1,lat1,lon2,lat2,xinch,yinch,xbord,ybord);
	range1= abs((int)(lon2-lon1));
	range2= abs((int)(lat2-lat1));
	range= (range2 > range1 ? range1 : range2);
	if(range >= 180) inc= 20;
	if(range >=135 && range < 180) inc= 15;
	if(range >=90 && range < 135) inc= 10;
	if(range >=40 && range <  90) inc= 5;
	if(range >=10 && range <  40) inc= 2;
	if(range >= 1 && range <  10) inc= 1;
	if(range < 1) inc=1;
	/*
	setmin(lon1,lat1);
	scl1= inch/(lon2-lon1);
	scl2= inch/(lat2-lat1);
	if(fabs(scl1) < fabs(scl2))	setscl(scl1,scl1);
	 else				setscl(scl2,scl2);
	setorig(xorig,yorig);
	*/
	erase();
	ubox(lon1,lat1,lon2,lat2);
	uwindow(lon1,lat1,lon2,lat2);
	ilat1= above(lat1,(float)(inc));
	ilon1= above(lon1,(float)(inc));
	ilat2= below(lat2,(float)(inc));
	ilon2= below(lon2,(float)(inc));
	for(ilat= ilat1; ilat <= ilat2; ilat += inc)
	   {
		uplot(lon1,(float)(ilat),0);
		uplot(lon2,(float)(ilat),1);
	   }
	for(ilon= ilon1; ilon <= ilon2; ilon += inc)
	   {
		uplot((float)(ilon),lat1,0);
		uplot((float)(ilon),lat2,1);
	   }
	
	uplot(-145.0,-30.0,0); uplot(145.0,30.0,1);
	uplot( 145.0,-30.0,0); uplot(-145.0,30.0,1);
   }
mapscale(lon1,lat1,lon2,lat2,xinch,yinch,xbord,ybord)
float lon1,lat1,lon2,lat2,xinch,yinch,xbord,ybord;
   {
	float xrange, yrange, xscl, yscl, scale, xorig, yorig;
	double fabs();
	xrange= fabs(lon2-lon1);
	yrange= fabs(lat2-lat1);
	xscl= (xinch - 2.0*xbord)/xrange;
	yscl= (yinch - 2.0*ybord)/yrange;
	scale= (xscl < yscl ? xscl : yscl);
	xorig= (xinch - xrange*scale)/2.0;
	yorig= (yinch - yrange*scale)/2.0;
	setorig(xorig,yorig);
	setmin(lon1,lat1);
	setscl(scale,scale);
   }

above(x,xinc)
float x, xinc;
   {
	int ix;
	ix= (int)(x/xinc);
	ix= (int)(ix*xinc);
	if( x > (float)(ix)) ix += (int)(xinc);
	return(ix);
   }

below(x,xinc)
float x, xinc;
   {
	int ix;
	ix= (int)(x/xinc);
	ix= (int)(ix*xinc);
	if( x < (float)(ix)) ix -= (int)(xinc);
	return(ix);
   }
