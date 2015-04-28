#include	"../h/igl.h"

#ifdef FORTRAN
erase_()
#else
erase()
#endif
   {
	output1(IGL_ERASE);
   }

#ifdef FORTRAN
pause_()
#else
pause()
#endif
   {
	output1(IGL_PAUSE);
   }

#ifdef FORTRAN
endplot_()
#else
endplot()
#endif
   {
	output1(IGL_ENDPLOT);
   }
