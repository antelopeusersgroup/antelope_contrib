/* @(#)usage.c	1.1 12/26/96  */

#include "par2db.h"

void
usage ()
{
    fprintf (stderr, "Usage: %s [-c] [-g] [-m srcmatch] [-i interval] [-v] [-w wfname] orb db [start-time [window]]\n", Program_Name);
    banner (Program_Name, "Version 1.1 12/26/96" ) ; 
    exit (1);
}
