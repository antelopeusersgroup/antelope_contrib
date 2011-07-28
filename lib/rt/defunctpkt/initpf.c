/* $Name $Revision$ $Date$  */ 
/*********************************************************************
 *
 *
 * initpf.c
 *
 * Get parameter file location for a current time&system            
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ********************************************************************/
#include <dirent.h>
#include "defunctpkt.h"
 
char *DASPF = NULL;       
 
void initpf( char *pf )
 
{
   double epoch, sec;
   Pf  *Param;
   Tbl *Inputs;
   Arr *pfarr;
   DIR *dirp;
   struct dirent *direntp;
   char *istr;
   char *path, exten[132], name[132], *pfile;
   int getone = 0;
   int yr, day, hr, min;
   int i, n, ninputs;
 
 
   if( (pfile = (char *) malloc(256)) == NULL)  
     elog_die( 1, "initpf(): malloc error\n");
 
/* Get today's time  */
 
   if( pf == NULL )  {
      epoch = now();
      e2h(epoch, &yr, &day, &hr, &min, &sec);
      
      /* Get List of Parameter Files  */
 
      if( (path = getenv("DASPF_PATH") ) == NULL || (int) strlen(path) <= 0 )  { 
            DASPF = "pkt"; 
     
      }  else  {
         sprintf( pfile, "%04d%03d%02d%02d\0", yr, day, hr, min);
         dirp = opendir(path);
         if(dirp == NULL) 
            elog_die(1, "initIP():Can't open %s directory.\n", path);
         pfarr = newarr( 0 );
         while ( (direntp = readdir(dirp)) != NULL ) {
            istr = direntp->d_name;
            fexten( istr, exten);
            if( strncmp( exten, "pf", strlen("pf")) == 0)
               setarr( pfarr, direntp->d_name, "Y" );
         }
 
       /* Get the most recent PF name  */
 
         Inputs = keysarr( pfarr );
         ninputs = maxtbl( Inputs );
         for( i = 0; i < ninputs; i++ )  {
           istr = ( char *) gettbl( Inputs, i );
           if( strcmp( istr, pfile ) == 0 ) {
              sprintf( pfile, "%s/%s\0", path, istr);
              getone = 1; break;
           }
           else if( strcmp( istr, pfile ) > 0 )  {
              if( i == 0 )
                 istr = ( char *) gettbl( Inputs, (0));
              else 
                 istr = ( char *) gettbl( Inputs, (i -1));
              strncpy( name, istr, strlen(istr));
              name[strlen(istr)-strlen(".pf")] = '\0';
              for(n = 0; n < (int) strlen(name); n++)
                 if(!isdigit(name[n])) break;
              if( n == (int) strlen(name))
                   sprintf( pfile, "%s/%s\0",path, istr);
              else 
                  sprintf( pfile, "pkt.pf" );
              getone = 1; break;
           }
         }  /* end for  */
         if( !getone ) {
            istr = ( char *) gettbl( Inputs, (ninputs -1));
            sprintf( pfile, "%s/%s\0",path, istr);
         }
 
/* Read configuration file  */
      
         pfile[strlen(pfile)-strlen(".pf")] = '\0'; 
         DASPF = pfile;
         freearr( pfarr, 0 ); 
         closedir( dirp ); 
      }
   }  else {
 
/* Read configuration file  */

      fexten( pf, exten);
      if( strncmp( exten, "pf", strlen("pf")) == 0)
         pf[strlen(pf) - strlen(".pf")] = '\0';
      DASPF =  pf;
   }
  
}
 


/* $Id$ */
