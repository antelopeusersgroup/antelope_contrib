#include	"../h/igl.h"
extern struct globalinfo *glcur;

erase()
   {
	output1(IGL_ERASE);
   }

pause()
   {
	output1(IGL_PAUSE);
   }

endplot()
   {
	setintbytes(DEFAULT_INTBYTES);
	output1(IGL_ENDPLOT);
   }
