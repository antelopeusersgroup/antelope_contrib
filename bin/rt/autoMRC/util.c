/*********************************************************************
 *
 * util.c
 *
 ********************************************************************/
#include "mrc.h"

#define MAX_SORT    1 

int *new_dasid( int id )

{
    int *num;
	  
    allot( int *, num, 1 );
    *num = id;

    return num;
}

void collect_dases( char *pfile )

{
   Pf  		*Param;
   Tbl 		*Site;
   struct Site 	 site;
   char 	*istr;
   int 		i;
   int 		nsite;
   int 		*num = 0;

   if(pfread( pfile, &Param) != 0)
       die(0, "Can't read parameter file\n");
 
	/* Get Input & Network tables  */
	 
   Site = pfget_tbl(Param, "Site");
   nsite = maxtbl(Site);
    
   if( nsite <= 0  )
     die( 0, "init(): parameter file is not complete.\n");
 
  Dasid = newarr( 0 );
	   
  for( i = 0; i < nsite; i++ )  {

	istr = (char *) gettbl(Site, i);
	sscanf(istr, STE_SCS,  STE_RVL(&site));
	if( !strcmp(site.up, "Y") )  {
	   if( (num = ( int *) getarr( Dasid, site.name ) ) == NULL )  {
	      num = new_dasid(site.sid);
	      setarr(Dasid, (char *) site.name, (char *) num );
           }
        }
   }
}           


char *getsta( srcid )
char *srcid;

{

    char *tmp;
    char original[64];
   
    memcpy( original, srcid, strlen( srcid ) ); 
    tmp = strtok( original, "/");
    tmp = strtok( NULL, "_");
    tmp[strlen(tmp)] = '\0';
    return tmp;

}


Das *new_das (
       char *srcid,
       double ttime )
     
{
     Das *das ;
     	      
     allot ( Das *, das, 1 ) ;
     das->apipe = new_orbpipe (MAX_SORT);
     das->time = ttime ;
     strcpy( das->srcid, srcid) ;
     
     return das ;
}

ChPipe *new_chpipe ( maxtbl ) 
{
    ChPipe *achan ; 
    
    allot ( ChPipe *, achan, 1 ) ;
    achan->time = 0.0; 
    achan->nsamp = 0; 
    achan->maxtbl = maxtbl; 
    achan->tbl = inittbl( 0, maxtbl+1, 1, 0, sizeof(Ch) ); 
    achan->crnt_chan.lta = 0;
    return achan;
}

ChRec *new_ch (
       char *srcid,
       char *sta,
       double ttime,
       int maxtbl )
     
{
     ChRec *ch ;
     int *dasid;
     	      
     allot ( ChRec *, ch, 1 ) ;
     ch->time = ttime ;
     ch->lta = 0;
     ch->nsamp = 0;
     dasid = (int *)getarr(Dasid, sta );
     ch->dasid = *dasid ;
     strcpy( ch->srcid, srcid) ;
     ch->chpipe = (ChPipe *) new_chpipe ( maxtbl );
     
     return ch ;
}
	     


void free_chpipe ( ChPipe *achan )
{
    int i, ntbl; 
    Ch *ch; 

    freetbl( achan->tbl, 0)  ;
    free( achan ) ; 
}

