/*
 *   dirops_ew.c  Solaris version
 *                Contains system-specific functions for 
 *                dealing with directories
 *
 *   960112:LDD
 * Alex added Will's GetFileName() 10/20/99
 */

#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include "earthworm.h"
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <sys/types.h>


/*****************************************************************************
 *  chdir_ew( )  changes current working directory; Solaris version          *
 *****************************************************************************/
int chdir_ew( char *path )
{
     return( chdir( path ) );
}


/*****************************************************************************
 *  CreateDir ()  Creates a directory. Solaris version.                      *
 *                                                                           *
 *  if dirname exists and is accessible, EW_SUCCESS is returned. Otherwise,  *
 *  we attempt to create it. If it all goes well, we return EW_SUCCESS,      *
 *  otherwise we report error and return EW_FAILURE.                         *
 *       LV 8/16/1999                                                        *
 *****************************************************************************/
int		CreateDir (char *dirname)
{

	struct	stat	buf;

	if (dirname == NULL)
	{
		logit ("e", "Invalid argument passed in; exitting!\n");
		return EW_FAILURE;
	}

	/* does it already exist? */
	if (stat (dirname, &buf) != 0)
	{
		if (errno == ENOENT)
		{

			if (mkdir (dirname, (mode_t)0775) != 0)
			{
				logit ("e", "CreateDir: Cannot create %s: %s\n",
								dirname, strerror(errno) );
				return( EW_FAILURE);
			}

		}
		else
		{
			logit ("e", "CreateDir: Cannot stat %s - %s \n", 
								dirname, strerror (errno));
			return EW_FAILURE;
		}

	}
	else
	{
		/* do nothing - hope that this is a directory */
	}

	return EW_SUCCESS;

}


       /***************************************************************
        *                        GetFileName                          *
        *                                                             *
        *  Function to get the name of a file in the current          *
        *  directory.                                                 *
        *                                                             *
        *  Returns 0 if all ok                                        *
        *          1 if no files were found                           *
	* from sendfile, by Will Kohler	                              *			
        ***************************************************************/


int GetFileName( char fname[] )
{
   DIR         *dp;
   struct stat ss;

   dp = opendir( "." );
   if ( dp == NULL ) return 2;

   do
   {
      struct dirent *dentp = readdir( dp );
      if ( dentp == NULL )
      {
         closedir( dp );
         return 1;
      }
      strcpy( fname, dentp->d_name );

      if ( stat( fname, &ss ) == -1 )
      {
         closedir( dp );
         return 1;
      }
   } while ( !S_ISREG(ss.st_mode) );

   closedir( dp );
   return 0;
}
