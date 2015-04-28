#include "devpar.h"
#include "xigl.h"

setpattern(ipat) 
int ipat;
{
	ipat %= NPATTERNS;
	if (pattern_info[ipat].pixmap_is_current) return;

	/* If we had a previous pixmap for this pattern number, free it	*/
	if ( pattern_info[ipat].p_pattern != (Pixmap) NULL )
		XFreePixmap (display, pattern_info[ipat].pixmap);

	/* Define a pixmap for the pattern from the bitmap data.	*/
	pattern_info[ipat].pixmap = 
		XCreatePixmapFromBitmapData (display, xid, &patterns[ipat][0],
					     32, 32, 0, 1, 1);
	pattern_info[ipat].pixmap_is_current = 	
		(pattern_info[ipat].pixmap != (Pixmap) NULL);
}

void
defpattern(ipat) 
int ipat;
{ 
	ipat %= NPATTERNS;
	pattern_info[ipat].pixmap_current = 0;
}
