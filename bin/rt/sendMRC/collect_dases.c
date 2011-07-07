/*********************************************************************
 *
 *  collect_dases.c 
 *
 *
 *
 ********************************************************************/
#include "defunctpkt.h"
#include "mrc.h"

extern char *pfile;

 DAS *new_das( int id, char *name )
 {
    DAS *das;
    allot( DAS *, das, 1 );
    das->unit = id;
    strcpy(das->name, name);
    return das;
 }

void init( )

{
   Pf  		*Param;
   Tbl 		*Site;
   DAS *das;
   struct Site 	 site;
   char 	*istr;
   char         dasid[6];
   int 		pkttype;
   int 		ntrec, i, j;
   int 		nsite;
   int 		*num = 0;

   if(pfread( pfile, &Param) != 0)
       elog_die(0, "Can't read parameter file\n");
 
	/* Get Input & Network tables  */
	 
   Site = pfget_tbl(Param, "Site");
   nsite = maxtbl(Site);
    
   if( nsite <= 0  )
     elog_die( 0, "init(): parameter file is not complete.\n");
 
  Dases = newarr( 0 );
  Dasid = newarr( 0 );
	   
  for( i = 0; i < nsite; i++ )  {

	istr = (char *) gettbl(Site, i);
	sscanf(istr, STE_SCS,  STE_RVL(&site));
	if( !strcmp(site.up, "Y") )  {
	   sprintf( dasid, "%d\0", site.sid );
	   if( ( das = ( DAS *) getarr( Dases, dasid ) ) == NULL )  {
	         das = new_das( site.sid, site.name );
	         setarr(Dases, (char *) &dasid[0], (char *) das );
           }
	   
	   if( (num = ( int *) getarr( Dasid, site.name ) ) == NULL )  {
	      allot( int *, num, 1 );
	      *num = site.sid;
	      setarr(Dasid, (char *) site.name, (char *) num );
           }
        }
   }
}           


	   

