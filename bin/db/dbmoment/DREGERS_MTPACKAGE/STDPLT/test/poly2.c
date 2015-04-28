#include	<stdio.h>
#define NPOLY	  9
#define MAXPOLY	 10
float size	= 3.0;
struct poly { float x, y; };
struct poly *plist[NPOLY], pbuf[NPOLY][MAXPOLY];
int nv[NPOLY];
struct poly protobox[4] = {
	 1.0,  1.0,
	 1.0, -1.0,
	-1.0, -1.0,
	-1.0,  1.0 };
main()
   {
	struct poly *p;
	int i, j;
	double ang, sin(), cos(), C, S;

	setorig(size+1.0,size+1.0);
	setscl(size,size);
	size= 1.0;
	ang= 0.0;
	for(i=0; i< NPOLY; i++)
	   {
		S= size * sin(ang * 3.14159/180.0);
		C= size * cos(ang * 3.14159/180.0);
		for(j=0; j<4; j++)
		   {
			pbuf[i][j].x = C* protobox[j].x -S * protobox[j].y;
			pbuf[i][j].y = S* protobox[j].x +C * protobox[j].y;
		   }
		plist[i]= pbuf[i];
		nv[i]= 4;
		size -= 0.1;
		ang += 5.0;
	   }
	for(i=0; i< NPOLY; i++)
	   {
		p= plist[i];
		for(j=0; j<nv[i]; j++)
			uplot(p[j].x,p[j].y,j);
		uplot(p[0].x,p[0].y,1);
	   }
	upolyfilln(NPOLY,nv,plist);
   }
