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
   }
