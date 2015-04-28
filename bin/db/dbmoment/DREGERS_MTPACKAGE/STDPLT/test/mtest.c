#
/*
 * test plot for pen
 * make a spiraling box pattern
 * flags:
 *	-e  output erase first.
 *	-b  make a big box.
 */
#include	<stdio.h>
#define NBOXSMALL	30
#define NBOXBIG		40
float xsmall[5] ={1.0,6.0,6.0,1.0,1.0};
float ysmall[5] ={1.0,1.0,6.00,6.00,1.0};
float xbig[5] ={1.0,10.00,10.00,1.0,1.0};
float ybig[5] ={1.00,1.0,10.0,10.0,1.0};
main(argc,argv)
short argc; char **argv;
   {
	int i,nrep,n;
	int nbox,big,doerase;
	float xc[5], yc[5];
	float *xt, *yt, wt1, wt2;

	argc--; argv++;
	big= doerase= 0;
	while(argc>0)
	   {
		if(argv[0][0]=='-')
			switch(argv[0][1])
			   {
				case 'e':
					doerase++;
					break;
				case 'b':
					big++;
					break;
			   }
		argc--;
		argv++;
	   }
	if(big)
	   {
		xt= xbig; yt=ybig;
		nbox= NBOXBIG;
	   }
	 else
	   {
		xt= xsmall; yt= ysmall;
		nbox= NBOXSMALL;
	   }
	if(doerase) erase();
	wt1= 0.9;
	wt2= 0.1;
	for(nrep=0; nrep<2; nrep++)
	   {
		for(i=0;i<5;i++)
		   {
			xc[i]=xt[i];
			yc[i]=yt[i];
		   }
		for(n=0; n<nbox; n++)
		   {
			plot(xc[0],yc[0],0);
			for(i=1;i<5;i++) plot(xc[i],yc[i],1);
			for(i=0;i<4;i++)
			   {
				xc[i]= xc[i]*wt1+xc[i+1]*wt2;
				yc[i]= yc[i]*wt1+yc[i+1]*wt2;
			   }
			xc[4]=xc[0];
			yc[4]=yc[0];
		   }
		wt2=wt1;
		wt1= 0.1;
	   }
	erase();
/*
 * test plot for all of the symbols
 */
#define MAX_SYMBOLS	20
   {
	int symbol();
	int i;
	double x,y;

	x = 1.0; y = 0.0;
	for (i = 0; i< MAX_SYMBOLS; i++) {
		x += 1.5;
		if (i%4 == 0) {
			y += 1.0;
			x = 1.0;
		}
		symbol (x,y,i,.5,0.);
	}
   }

	erase();


   {
	int nang	=90;
	float rad	=5.0;
	float x0	=1.0;
	float y0	=1.0;

	int i;
	double dang, angle, cos(), sin();
	float xp, yp;
/*
	setpar(ac,av);
	getpar("nang","d",&nang);
	getpar("rad","f",&rad);
	getpar("x0","f",&x0);
	getpar("y0","f",&y0);
	endpar();
*/

	dang= 90.0/ (double)(nang-1);
	setorig(x0,y0);
	for(i=0; i<nang; i++)
	   {
		angle= (i*dang)*3.14159/180.0;
		xp= rad * sin(angle);
		yp= rad * cos(angle);
		uplot(0.0,0.0,0);
		uplot( xp, yp,1);
/*
fprintf(stderr,"%3d %7.2f %7.2f\n",i,xp,yp);
*/
	   }
   }


   }

