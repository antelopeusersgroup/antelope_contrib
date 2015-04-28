#include	<stdio.h>
#include	"../../h/igl.h"

int xbaseorig, ybaseorig;

get_std_par()
   {
	
	extern int xorigin, yorigin;
	extern float xscale, yscale;
	extern int fatbase;
	extern int page, xpagelen, ypagelen;
	extern int grid;
	extern int xmax, ymax;
	extern float pixinch;
	extern int do_pause,pausetime;
	extern char labelstr[];
	extern int do_erase,do_window,do_label;
	extern float textangle,textcosang,textsinang;
	extern float symangle,symcosang,symsinang;
	extern float labelx,labely;
	extern int labelix,labeliy;
	extern float symscale, textscale;
	extern int symfill;
	extern int ixwmin,iywmin, ixwmax, iywmax;
	int itemp, xpageorigin, ypageorigin, xorig, yorig, tempi;
	float scale, xtemp, ytemp, convert,tempscale;

	/* page orientation */
	if(getpar("xpagelen","f",&xtemp)) xpagelen= (int)(xtemp*pixinch);
	if(getpar("ypagelen","f",&ytemp)) ypagelen= (int)(ytemp*pixinch);
	if(getpar("pagerot","d",&itemp))
	   {
		while(itemp <    0) itemp += 360;
		while(itemp >= 360) itemp -= 360;
		page= (itemp +45)/90;
	   }

	if(getpar("textscale","f",&tempscale)) textscale=tempscale;


	labelix = labelx*pixinch;
	labeliy = labely*pixinch;

	symangle= 0.0;	/* page rotations are added when angle is changed */

	switch(page)
	   {
		case 0:	/*   0-degree rotation */
			xpageorigin= 0;
			ypageorigin= 0;
			symsinang = textsinang = 0.0;
			symcosang = textcosang = 1.0;
			break;
		case 1:	/*  90-degree rotation */
			xpageorigin= 0;
			ypageorigin= xpagelen;
			textangle = (90.0 * 2.0 * 3.141592)/360.0;
			symsinang = textsinang = 1.0;
			symcosang = textcosang = 0.0;
			break;
		case 2:	/* 180-degree rotation */
			xpageorigin= xpagelen;
			ypageorigin= ypagelen;
			textangle = (180.0 * 2.0 * 3.141592)/360.0;
			symsinang = textsinang = 0.0;
			symcosang = textcosang = -1.0;
			break;
		case 3:	/* 270-degree rotation */
			xpageorigin= ypagelen;
			ypageorigin= 0;
			textangle = (270.0 * 2.0 * 3.141592)/360.0;
			symsinang = textsinang = -1.0;
			symcosang = textcosang =  0.0;
			break;
	   }


	/* now set the scaling parameters */
	convert= pixinch / GEN_PIXINCH;
	scale=1.0;
	if(getpar("xscale","f",&xtemp)) xscale *= xtemp;
	if(getpar("yscale","f",&ytemp)) yscale *= ytemp;
	if(getpar("scale","f",&scale))
	   {
		xscale *= scale;
		yscale *= scale;
	   }
	xscale *= convert;
	yscale *= convert;

	/* now get the origin parameters */
	xorig= yorig= 0;
	if(getpar("xorig","f",&xtemp)) xorig= pixinch * xtemp;
	if(getpar("yorig","f",&ytemp)) yorig= pixinch * ytemp;
	switch(page)
	   {
		case 0:
			xorigin= xpageorigin + xorig;
			yorigin= ypageorigin + yorig;
			labelix += xorigin;
			labeliy += yorigin;
			break;
		case 1:
			xorigin= xpageorigin + xorig;
			yorigin= ypageorigin - yorig;
			tempi = labelix;
			labelix = yorigin  - labeliy;
			labeliy = xorigin  + tempi;
			break;
		case 2:
			xorigin= xpageorigin - xorig;
			yorigin= ypageorigin - yorig;
			labelix = xorigin - labelix;
			labeliy = yorigin - labeliy;
			break;
		case 3:
			xorigin= xpageorigin - xorig;
			yorigin= ypageorigin + yorig;
			tempi = labelix;
			labelix = yorigin + labeliy;
			labeliy = xorigin - tempi;
			break;
	   }
	/* save the origin in case the user wants to modify it. */
	xbaseorig= xorig;
	ybaseorig= yorig;

	getpar("fat","d",&fatbase);

	if(getpar("grid","f",&xtemp)) grid= (int)(pixinch * xtemp);
	getpar("label","s",labelstr);
	if(getpar("pause","d",&pausetime)) do_pause=1;
	getpar("erase","b",&do_erase);
	getpar("window","b",&do_window);
	getpar("dolabel","b",&do_label);
   }
