#include "tclExtend.h"
#include "ucp2_tcl.h"
#include "tk.h"
#include <math.h>

EXTERN int main _ANSI_ARGS_((int     argc,
                             char  **argv));
int (*tclXDummyMainPtr)() = (int (*)()) main;


#if defined(DOMAIN) && defined(SING)
EXTERN int matherr _ANSI_ARGS_((struct exception *));
int (*tclDummyMathPtr)() = (int (*)()) matherr;
#endif

int
Ucp2_Init(Tcl_Interp *interp)
{
    Tk_Window main;

    Tcl_PkgProvide(interp, "Ucp2", "3.0" ) ; 

    main = Tk_MainWindow(interp);
    Tcl_CreateCommand(interp, "dcrt", dcrtCmd, main, NULL) ; 
    Tcl_CreateCommand(interp, "dprt", dprtCmd, main, NULL) ; 
    Tcl_CreateCommand(interp, "dcc", dccCmd, main, NULL) ; 

    return TCL_OK;
}
