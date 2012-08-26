
#include <stdio.h>
#include <time.h>

int
antelope_init_plot (int itran, char *fname, char *progname, char *dbname, char *filter)

{
	float ssize=1.0;
	float xwin=0.0;
	float ywin=0.0;
	char plotfile[256];
	char username[256];
	char display[16];
	char program[1024];
	static int nplot=1;
        float xplt, yplt;
        float angle=0.0;
        int iclip=0;
        int iref=5;
        float height=0.08;
        float ratio=1.0;
        float slant=0.0;
        int jfont=114; 
	float xdim, ydim, xlow, ylow;
	float xmin, xmax, ymin, ymax;
	float thick=0.0;
	int ithick=0;
	time_t itime;

 	if (fname) {
 		if (fname[0]) {
 			strcpy (plotfile, fname);
		} else {
			sprintf (plotfile, "%s.%s.%d.ps", progname, dbname, nplot++);
		}
	} else {
		sprintf (plotfile, "%s.%s.%d.ps", progname, dbname, nplot++);
	}
	strcpy (display, "none");
	strcpy (program, progname);
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
		xlow = 0.2;
		ylow = 0.2;
		ymin = 0.0;
		ymax = 9.8;
		xmin = 0.0;
		xmax = 7.3;
	} else {
		xdim = 9.8;
		ydim = 7.3;
		xlow = 0.2;
		ylow = 0.2;
		xmin = 0.0;
		xmax = 9.8;
		ymin = 0.0;
		ymax = 7.3;
	}
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
        xplt = 0.0;
        yplt = 0.0;
	jfont = 113;
	height = 0.10;
	iref = 0;
        cfont_ (&jfont);
        chrsiz_ (&height, &ratio, &slant);
        text_ (&xplt, &yplt, &angle, &iref, "BRTT", &iclip, strlen("BRTT"));
	jfont = 115;
        cfont_ (&jfont);
        itime = time(NULL);
	strcpy (username, "");
	my_username (username);
	sprintf (program, "%s: %s %s %s %s", progname, dbname, plotfile, username, ctime(&itime));
	program[strlen(program)-1] = '\0';
	xplt = 0.5;
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	if (filter == NULL) {
        	sprintf (program, "Filter: None");
	} else {
        	sprintf (program, "Filter: %s", filter);
	}
        xplt = 0.0;
        yplt = 0.18;
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
}

/* $Id$ */
