/*
 *   dirops_ew.c  Solaris version
 *                Contains system-specific functions for 
 *                dealing with directories
 *
 *   960112:LDD
 */
#include <unistd.h>

/*****************************************************************************
 *  chdir_ew( )  changes current working directory; Solaris version          *
 *****************************************************************************/
int chdir_ew( char *path )
{
     return( chdir( path ) );
}

