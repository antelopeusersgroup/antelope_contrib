int
killbutton(itran)
/* To add a QUIT button that causes nxplot window to die */
int itran;
{
	float xdim, ydim, xmax, ymax;
	float xlow=0., ylow=0., x1, y1, xsz, ysz, xx,yy;
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
	x1 = xmax - 0.5;
	y1 = ylow;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xlow, &xmax, &ylow, &ymax);
	xsz=x1 + 0.5;
	ysz = y1 + 0.4;
	setbg_(red, strlen(red));
	box_ (&x1, &xsz, &y1, &ysz, &thick, &ithick, &iclip);
	sprintf (labl,"QUIT");
	/* setfg_(black, strlen(black)); */
	height = 0.12;
        chrsiz_ (&height, &ratio, &slant);
	xx = .25+x1;
	yy = .2+y1;
	text_(&xx, &yy, &angle, &iref, labl, &iclip, strlen(labl));

	do {
	  cursor_(&xx, &yy,&chr, 1);
	  if (xx<xsz && yy<ysz) {
	    finitt_();
	    hdkild_();
	    break;
	  }
	} while (0 == 0);
}
