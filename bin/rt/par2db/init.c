/*********************************************************************
 *
 *
 * init.c
 *
 * Read Configuration File; Initialize neccessary structure.
 *
 ********************************************************************/
#include <string.h>
#include "par2db.h"

 
extern char *ucase();

void init( char *pfile )

{
   Pf  *Param;
   Tbl *Site;
   struct PktPar packet;
   Ste site, *sta;
   char *istr, key[64];
   int pkttype;
   int i, nst;
   int nsite;

	/* Read configuration file  */

   nst = 0;

   initpf( pfile );
   if(pfread( DASPF, &Param) != 0)
       die(0, "Can't read parameter file\n");
 
	/* Get Input & Network tables  */
	 
   Site = pfget_tbl(Param, "Site");
   nsite = maxtbl(Site);
    
   if( nsite <= 0  )
     die( 0, "init(): parameter file is not complete.\n");
 

	/* Initialize RBData structure  */
	 
  Pid = newarr( 0 );
  
  for( i = 0, nst=0; i < nsite; i++ )  {

	istr = (char *) gettbl(Site, i);
	sscanf(istr, S_SCS,  S_RVL(&site));
	if( !strcmp(site.up, "Y") )  {
	      
	   sprintf( key, "%s_%s/%s\0", site.net, site.name, site.pkttype );

           if( (sta = ( Ste *) getarr( Pid, key)) == 0 )
               allot( Ste *, sta, 1 );
	  
	   memcpy( (char *) sta, (char *) &site, sizeof( Ste ) ); 
	   setarr( Pid, key, ( char *) sta );
        }
    }
}           

