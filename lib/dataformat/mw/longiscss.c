
/* @(#)longiscss.c      820/91 */

#include <sys/stat.h>
#include <sys/types.h>
longiscss_(filename)
/*  for a given css2 file returns the length,
    or -1 if the file could not be found or has a different owner and
    is not read-permitted    */

char filename[80];
{
   int terms;
   struct stat finfo;
   int i;

   for(i =0; i < 80; i++) 
     if((filename[i] == ' ') || (filename[i] == '\0') || (filename[i] == '\n') || (filename[i] == '\t') ) break;
   filename[i] = '\0';


   if(stat(filename,&finfo)==-1)
      return(-1);             /*error return on failed status request*/
   else if( (getuid()!=finfo.st_uid) && ((finfo.st_mode&04)==0) )
      return(-1);             /*error return on no read permission*/

   terms = finfo.st_size/2;     /*short integer*/
   return(terms);
}



/* $Id$ */
