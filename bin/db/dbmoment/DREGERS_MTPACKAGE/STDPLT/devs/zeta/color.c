#include <stdio.h>
#include "devpar.h"

int color;

/* Initial Mapping of IGL colors to Zeta pens.	*/
int penmap[NCOLORS] = {1,2,3,4,5,6,7,8};

setcolor(icolor)
int icolor;
{
/*	Set the color to this color.		*/
	int i;
	char str[3];

	color = icolor % 8;
	fputs (PEN_SELECT_CMD, stdout);
	i = stocrb (str, penmap[color]);
	str[i] = '\0';
	fputs (str, stdout);
}

defcolor(icolor,ir,ig,ib)
int icolor,ir,ig,ib;
{
/*	No user-defined color mapping for this device.	*/
}
