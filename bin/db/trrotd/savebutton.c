int
savebutton(itran, asave)
/* To add a SAVE vs. QUIT button that causes nxplot window to die */
int itran;	/* 0 for portrait */
char *asave;	/* y or n on return */
{
	float xdim, ydim, xmax, ymax;
	float xlow=0., ylow=0., x1, y1, xsz, ysz, xx,yy, xs1,xs2;
	float thick=0.02;
	int ithick=0, iclip = 1;
        int jfont=114; 
	char labl[8];
	char black[8], white[8], red[8];
        int iref=4;
        float height=0.06, angle=0.;
        float ratio=1.0;
        float slant=0.0;
	char chr;

	strcpy(black, "black");
	strcpy(white, "white");
	strcpy(red, "red");

	if (itran == 0) {
		ydim = 10.0;
		xdim = 7.5;
	} else {
		xdim = 10.0;
		ydim = 7.5;
	}
	xmax = xlow + xdim;
	ymax = ylow + ydim;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xlow, &xmax, &ylow, &ymax);

	/* QUIT button */
	x1 = xmax - 0.5;
	y1 = ylow;
	xsz=x1 + 0.5;
	ysz = y1 + 0.4;
	setbg_(red, strlen(red));
	box_ (&x1, &xsz, &y1, &ysz, &thick, &ithick, &iclip);
	sprintf (labl,"QUIT");
	height = 0.12;
        chrsiz_ (&height, &ratio, &slant);
	xx = .25+x1;
	yy = .2+y1;
	text_(&xx, &yy, &angle, &iref, labl, &iclip, strlen(labl));

	/* SAVE button */
	xs1 = xmax - 1.5;
	xs2 = xs1 + 0.6;
	box_ (&xs1, &xs2, &y1, &ysz, &thick, &ithick, &iclip);
	sprintf (labl,"SAVE");
	xx = .3+xs1;
	yy = .2+y1;
	text_(&xx, &yy, &angle, &iref, labl, &iclip, strlen(labl));

	do {
	  cursor_(&xx, &yy,&chr, 1);
	  if (xx<xsz && yy<ysz && xx>=x1 && yy>= y1) {
	    *asave = 'n';
	    break;
	  } else if (xx<xs2 && yy<ysz && xx>=xs1 && yy>= y1) {
	    *asave = 'y';
	    break;
	  }
	} while (0 == 0);
}
