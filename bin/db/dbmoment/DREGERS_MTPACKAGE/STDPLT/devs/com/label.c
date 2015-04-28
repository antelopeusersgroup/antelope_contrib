#include	<stdio.h>
#include	<pwd.h>

#define LABELMAX	88
char labelstore[LABELMAX+1];
char labelstr[256];
char labelbuf[256];
extern float textsize,textangle,textcosang,textsinang;
extern float pixinch;
extern int page,xorigin,yorigin;
extern int xpageorigin, ypageorigin;
extern int penmode,penfat;
extern int textcenter,textfont;
extern int ixwmin,iywmin,ixwmax,iywmax,xmax,ymax;
extern char devid;
int labelix, labeliy;

storelabel(str)
char str[];
   {
	int n;
	n = 0;
	while(n < LABELMAX && *str != '\0') labelbuf[n++]= *str++;
	labelbuf[n]= '\0';
   }

makelabel(str)
char str[];
   {
	int i, t, uid;
	char *date, *ctime();
	struct passwd *getpwuid(), *pw;

	uid= getuid();
	pw= getpwuid(uid);
	time(&t);
	date= ctime(&t);
	if(pw == NULL)	/* for some reason this person does not exist */
		sprintf(str,"%3d :: %s  %s",uid,date,labelstore);
	 else
		sprintf(str,"%s :: %s  %s",pw->pw_name,date,labelstore);
	
	/* scan string to remove newlines, tabs etc. The date function
	   introduces one newline.
	 */
	for(i=0; str[i] != '\0'; i++)
		if(str[i] < ' ' || str[i] > '~') str[i]= ' ';
   }

labelplot()
{
	float tempsize,tempang,tempcos,tempsin;
	int ix,iy,tempmode,tempfat,tempcenter,tempfont;
	int txmin,tymin,txmax,tymax;

	storelabel(labelstr); 	/*store runtime label*/
	if (strlen(labelbuf) == 0) storelabel(labelstore);/*user program label*/
        if (strlen(labelbuf) == 0) makelabel(labelbuf);	/* default label*/
	labelstore[0] = '\0';
	txmin = ixwmin;
	tymin = iywmin;
	txmax = ixwmax;
	tymax = iywmax;
	tempfont = textfont;
	tempsize = textsize;
	tempang = textangle;
	tempcos = textcosang;
	tempsin = textsinang;
	tempmode = penmode;
	tempfat = penfat;
	tempcenter = textcenter;
	ixwmin = 0;
	iywmin = 0;
	ixwmax = xmax;
	iywmax = ymax;
	textfont = 0;
	penfat = 0;
	textangle =0.0;
	textcosang = 1.0;
	textsinang = 0.0;
	textsize = .1;
	penmode = 0;
	textcenter = 0;
	switch (page)
	   {
		case 0:
			break;
		case 1:
			textangle = (90.0 * 2.0 * 3.141592)/360.0;
			textsinang = 1.0;
			textcosang = 0.0;
			break;
		case 2:
			textangle = (180.0 * 2.0 * 3.141592) / 360.0;
			textsinang = 0.0;
			textcosang = -1.0;
			break;
		case 3:
			textangle = (270.0 * 2.0 * 3.141592) / 360.0;
			textsinang = -1.0;
			textcosang =  0.0;
			break;
          }
	/* don't text label if device is versatec */
	if (devid != 'v')	 text(labelix,labeliy,labelbuf);
	ixwmin = txmin; 
	iywmin = tymin;
	ixwmax = txmax;
	iywmax = tymax;
	textsize = tempsize;
	textangle = tempang;
	textcosang = tempcos;
	textsinang = tempsin;
	penmode = tempmode;
	penfat = tempfat;
	textcenter = tempcenter;
	textfont = tempfont;
}
