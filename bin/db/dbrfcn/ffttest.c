/* to test the fft */
#include <stdio.h>
#include <math.h>

main()
{
	float xdat[4098], data[4098], x,y;
	char *calloc();
	char aline[256];
	int i, nn=0, nin;
	float ascl;
	
	while (fgets(aline, 255, stdin)) {
	    sscanf(aline, "%f %f", &x, &y);
	    xdat[nn] = x;
	    data[nn] = y;
	    nn += 1;
	    if (nn > 4095) break;
	}

	for (i=nn; i<4098; i++) {
	    xdat[i]=0.;
	    data[i]=0.;
	}
	nin = nn;
	for(i=1;((int)pow((double)2,(double)i)) < nn;++i);
	nn=(int)pow((double)2,(double)i);
	if (nn>4096) nn=4096;

	/* forward transform */
	cfftr(data, nn);



	/* inverse transform */
	cfftri(data, nn);

	/* rescale */
	ascl = 2./((float)nn);
	for (i=0; i<nin; i++) data[i] *= ascl;

	/* output */
	for (i=0; i<nin; i++) printf ("%f %f\n", xdat[i], data[i]);
}
