#include <stdio.h>
#include <math.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "tr.h"

int
init_plot (itran, size, fname, progname, title, subtitle)
/* stolen from JSPC's dbap */

int itran;
double size;
char *fname;
char *progname;
char *title;
char *subtitle;

{
	float ssize=0.95;
	float xwin=0.0;
	float ywin=0.0;
	char plotfile[256];
	char display[16];
	char program[1024];
	static int nplot=1;
        float xplt, yplt;
        float angle=0.0;
        int iclip=0;
        int iref=5;
        float height=0.06;
        float ratio=1.0;
        float slant=0.0;
        int jfont=114; 
	float xdim, ydim, xlow, ylow;
	float xmin, xmax, ymin, ymax;
	float thick=0.0;
	int ithick=0;
	float x1, y1, x2, y2;
	float hue, light, sat;
	float fac;
	long itime;
	int i, len;
	char black[8], white[8];
	strcpy(black, "black");
	strcpy(white, "white");

 	if (fname) {
 		sprintf (plotfile, "%s.ps", fname);
	} else {
		sprintf (plotfile, "none");
	}
	strcpy (display, " ");
	strcpy (program, progname);
	if (size > 0.0) ssize = size;
	initt_ (&itran, plotfile, display, program, &ssize,
		&xwin, &ywin, strlen(plotfile), strlen(display),
		strlen(program));
	if (itran == 0) {
		ydim = 10.0;
		xdim = 7.5;
		xlow = 0.0;
		ylow = 0.0;
		xmin = 0.0;
		xmax = 1.0;
		ymin = 0.0;
		ymax = 1.0;
	} else {
		xdim = 10.0;
		ydim = 7.5;
		xlow = 0.0;
		ylow = 0.0;
		xmin = 0.0;
		xmax = 1.0;
		ymin = 0.0;
		ymax = 1.0;
	}
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	iclip = 1;
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	if (itran == 0) {
		ydim = 9.8;
		xdim = 7.3;
		xlow = 0.1;
		ylow = 0.1;
		ymin = 0.0;
		ymax = 9.8;
		xmin = 0.0;
		xmax = 7.3;
	} else {
		xdim = 9.8;
		ydim = 7.3;
		xlow = 0.1;
		ylow = 0.1;
		xmin = 0.0;
		xmax = 9.8;
		ymin = 0.0;
		ymax = 7.3;
	}
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	setfg_ (black, strlen(black));
        xplt = 0.0;
        yplt = 0.0;
	height = 0.11;
	iref = 0;
        chrsiz_ (&height, &ratio, &slant);
	jfont = 115;
        cfont_ (&jfont);
	setfg_ (black, strlen(black));
        itime = time(0);
	sprintf (program, "%s: %s %s", progname, cuserid(0), ctime(&itime));
	/* fix the string */
	len = strlen(program);
	for (i=0; i<len; i++) {
	    if (((int)program[i])<32) {
		program[i] = '\0';
		len = strlen(program);
		break;
	    }
	}
	x1=0.5*(xmax+xmin);
	y1=ymax - 0.25;
	iref = 5;
	text_(&x1, &y1, &angle, &iref, title, &iclip, strlen(title));
	y1 = y1 - 2.*height;
	height = 0.11;
        chrsiz_ (&height, &ratio, &slant);
	text_(&x1, &y1, &angle, &iref, subtitle, &iclip, strlen(subtitle));
	x1 = xmin + 0.25;
	y1 = ymin + 0.2;
	iref = 1;
	height = 0.08;
        chrsiz_ (&height, &ratio, &slant);
	text_(&x1, &y1, &angle, &iref, program, &iclip, strlen(program));
	height = 0.10;
        chrsiz_ (&height, &ratio, &slant);

}

/*  Plot all of the traces in the "tr" trace-set.  
	ymin, ymax are bottom, top of group of traces, in units of fraction of page size
	Only works if tr has bundletype=1 for now.
 */
int
plot_tr( tr, ymin, ymax, plist, nplt)
Dbptr tr;			/* trace-set to plot (bundletype=1 works) */
float ymin, ymax;		/* vertical position, 0.0-1.0 fraction of page */
int *plist;			/* index list for each trace: >0 to plot ith tr */
int nplt;			/* # of records in tr or plist */
{
    float          *data;
    float	   *timar;
    int             rs,bs,
                    re,be;
    Dbptr           bundle;
    int             bundletype;
    int             retcode = 0;
    int             nsamp, nrec;
    double	    samprate, t1, t1min, t2, t2max ; 
    double	    cosphi,sinphi,temp;
    int		    i, ii, jj, nptmax=0; 
    char	    chan[8];
    float	    xwin1=0.5, xwinwd=6.5, paght=10.0;
    float	    ywin1, ywin2, ywinht;
    float	    y1, y2, xx, yy, xx1, yy1, ampmax=0., zero=0., yscl;
    float	    *ampar, *y1ar;
    float	    thick=0.0, xmax;
    float	    tic1, dtic, mtic1, dmtic;
    int		    ntic, nmtic;
    int 	    ithick=0, iclip=1, igraf = 0, iref=0;
    static char     asymb[] = " ";
    char 	    fgcolor[8], white[8], labl[32];
    int		    ticint();
    float 	    height=0.06;
    float 	    ratio=1.0;
    float	    slant=0.0;



    dbget_range(tr, &rs, &re);

    nrec = 0;
    for (i=0; i<nplt; i++) if (plist[i]!=0) nrec +=1;
    if (nrec<1) return(-1);
    ywin1 = ymin*paght;
    ywinht = paght*(ymax - ymin)/((float)nrec);

    /* get global data bounds */
    t1min = an_infinity();
    t2max = 0.;
    y1ar = (float *)malloc((re-rs)*sizeof(float));
    ampar = (float *)malloc((re-rs)*sizeof(float));
    ii = -1;
    jj = -1;
    for (tr.record = rs; tr.record < re; tr.record++)
      {
	dbgetv ( tr, 0, "bundle", &bundle, 0 ) ;
    	dbget_range(bundle, &bs, &be);
    	for (bundle.record = bs; bundle.record < be; bundle.record++)
	{
	 	jj += 1;
		if (plist[jj] == 0) continue;
	 	ii += 1;
		dbgetv(bundle, 0,
              		"bundletype", &bundletype,
              		0);

		if (bundletype != 0) elog_die(0,"plot_tr:bundletype != 0");
	
		if (dbgetv(bundle, 0,
		 "data", &data,
		  "nsamp", &nsamp,
		  "samprate", &samprate,
		  "time", &t1,
		   0)!=0) elog_die(0,"plot_tr:dbgetv problem\n");
		if (nsamp>nptmax) nptmax=nsamp;
	  	if (t1 < t1min || ii==0) t1min = t1;
		t2 = t1 + (float)(nsamp-1)/samprate;
		if (t2 > t2max) t2max = t2;
		y1 = data[0];
		y2 = y1;
		for (i=1; i<nsamp; i++) {
		    if (data[i]<y1) y1 = data[i];
		    if (data[i]>y2) y2 = data[i];
	 	}
		if (y2 - y1 > ampmax) ampmax = y2 - y1;
	    	ampar[ii] = y2 - y1;
		y1ar[ii] = y1;
	}
    }

	/* Start plotting cycle */
    timar = (float *)malloc(nptmax*sizeof(float));
    xmax = (float)(t2max - t1min);
    ii = -1;
    jj = -1;
    for (tr.record = rs; tr.record < re; tr.record++)
      {
	dbgetv ( tr, 0, "bundle", &bundle, 0 ) ;
    	dbget_range(bundle, &bs, &be);
    	for (bundle.record = bs; bundle.record < be; bundle.record++)
	{
		jj += 1;
		if (plist[jj] == 0) continue;
		ii += 1;
		dbgetv(bundle, 0,
              		"bundletype", &bundletype,
              		0);

		if (bundletype != 0) elog_die(0,"plot_tr:bundletype != 0");
	
		if (dbgetv(bundle, 0,
		 "data", &data,
		  "nsamp", &nsamp,
		  "samprate", &samprate,
		  "chan", chan,
		  "time", &t1,
		   0)!=0) elog_die(0,"plot_tr:dbgetv problem\n");

		timar[0] = (float)(t1 - t1min);
		for (i=1; i<nsamp; i++) timar[i] = timar[i-1]+ 1./samprate;

		 ywin2 = ywin1 + ywinht;
		y1 = y1ar[ii] - 0.5*(ampmax-ampar[ii]);
		y2 = y1+ampmax;
		setdim_ (&xwinwd, &ywinht, &xwin1, &ywin1);
		setscl_ (&zero, &xmax, &y1, &y2);
		iclip = 1;   /* set to 0 if works */
	        nplot_ (&nsamp, timar, data, &igraf, &iclip, &thick, &ithick, asymb, strlen(asymb));

		

		ywin1 += ywinht;
	}
    }
    ii = -1;
    jj = -1;
    ywin1 = ymin*paght;
    strcpy(fgcolor, "black");
    setfg_ (fgcolor, strlen(fgcolor));
    for (tr.record = rs; tr.record < re; tr.record++)
      {
	dbgetv ( tr, 0, "bundle", &bundle, 0 ) ;
    	dbget_range(bundle, &bs, &be);
    	for (bundle.record = bs; bundle.record < be; bundle.record++)
	{
		jj += 1;
		if (plist[jj] == 0) continue;
		ii += 1;
		dbgetv(bundle, 0,
              		"bundletype", &bundletype,
              		0);

		if (bundletype != 0) elog_die(0,"plot_tr:bundletype != 0");
	
		if (dbgetv(bundle, 0,
		  "chan", chan,
		   0)!=0) elog_die(0,"plot_tr:dbgetv problem\n");

		 ywin2 = ywin1 + ywinht;
		y1 = y1ar[ii] - 0.5*(ampmax-ampar[ii]);
		y2 = y1+ampmax;
		setdim_ (&xwinwd, &ywinht, &xwin1, &ywin1);
		setscl_ (&zero, &xmax, &y1, &y2);

		/* Label axes */

		ticint(6, 1, zero, xmax, &tic1, &dtic, &ntic, &mtic1, &dmtic, &nmtic);
	        height = 0.10;
	        chrsiz_ (&height, &ratio, &slant);
		xx = 0.015*xmax;
		yy = y2 - 0.05*ampmax;
		iref = 2;
		text_(&xx, &yy, &zero, &iref, chan, &iclip, strlen(chan));

	        height = 0.08;
	        chrsiz_ (&height, &ratio, &slant);
		yy = y1 + 0.05*ampmax;
		sprintf(labl,"dt:%.1f",dtic);
		iref = 0;
		text_(&xx, &yy, &zero, &iref, labl, &iclip, strlen(labl));

		xx = 0.98*xmax;
		yy = y2 - 0.05*ampmax;
		sprintf(labl,"%.3e",y2);
		iref=8;
		text_(&xx, &yy, &zero, &iref, labl, &iclip, strlen(labl));

		yy = y1 + 0.05*ampmax;
		sprintf(labl,"%.3e",y1);
		iref=6;
		text_(&xx, &yy, &zero, &iref, labl, &iclip, strlen(labl));

		/* Draw Tics: y=0 and x on bottom */
		iclip = 1;
		box_ (&zero, &xmax, &y1, &y2, &thick, &ithick, &iclip);
		iclip = 0;
		yy = 0.;
		xx = zero;
		xx1 = zero + 0.02*xmax;
		line_ (&xx, &yy, &xx1, &yy, &thick, &ithick, &iclip);
		xx = xmax;
		xx1 = xx - 0.02*xmax;
		line_ (&xx, &yy, &xx1, &yy, &thick, &ithick, &iclip);

		yy = y1;
		yy1 = y1 + 0.03*ampmax;
		for (i=0, xx=tic1; i<ntic; i++, xx+=dtic) 
			line_(&xx, &yy, &xx, &yy1,  &thick, &ithick, &iclip);
		

		ywin1 += ywinht;
	}
    }
    free(y1ar);
    free(ampar);
    free (timar);
}

/* calculate tick intervals */
int
ticint(nticopt, nminor, x1, x2, tic1, dtic, ntic, mtic1, dmtic, nmtic) 

int 	nticopt;		/* optimal # of major tics */
float 	x1, x2;			/* plot bounds */
float 	*tic1, *dtic;
int	*ntic;				/* start, increment, and number of major ticks */
float 	*mtic1, *dmtic;
int	*nmtic;				/* start, increment, and number of major ticks */
{
	double dt1, ldt1, iexp, dt, t1, t2;
	double scl=5.;


	dt1 = (x2 - x1)/(float)nticopt;
	ldt1 = log10(dt1);
	iexp = floor(ldt1);
	ldt1 = pow(10.,ldt1-iexp);	/* should be between 1 and 9.9999.. */
	if (ldt1 < 2.) scl=1.;
	if (ldt1 >= 2. && ldt1 <5.) scl=2.;
	dt = scl*pow(10.,iexp);
	t1 = floor(x1/dt)*dt;
	if (t1 < x1) t1 += dt;
	t2 = ceil(x2/dt)*dt;
	if (t2 > x2) t2 -= dt;
	*ntic = nint((t2-t1)/dt) + 1;
	*dtic = dt;
	*tic1 = t1;

	/* minor tics:  do this later */
	*mtic1 = *tic1;
	*dmtic = *dtic;
	*nmtic = *ntic;
	if (nminor <= 1) return(0);

}
