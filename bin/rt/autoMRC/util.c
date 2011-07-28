/*********************************************************************
 *
 * util.c
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ********************************************************************/
#include "mrc.h"

void usage ()
{
        fprintf (stderr,
        "Usage: %s [-p pfile ]  orb dcname1[,dcname2,dcname3...] \n",
        Program_Name);
        exit (1);
}
                
char *new_name( char *name )  {
      
     char *new;
	    
    new=strdup( name );
    return new;
		 
}
  
#define MAX_SORT    1

int *new_dasid( int id )

{
    int *num;
	  
    allot( int *, num, 1 );
    *num = id;

    return num;
}
 
typedef struct Site {
   double calib;           /* calibration coef  */
   int sid;                /* DAS ID  */
   int sensid;             /* Sensor ID  */
   char pkttype[12];       /* Packet type - CBBHS, CBBLS, etc */
   char name[8];           /* Site name */
   char up[3];             /* Y/N site is up/down  */
   char sens[12];          /* Sensor name  */
   char trg[3];            /* Y/N trigger on this site  */
   char pick[3];            /* Y/N trigger on this site  */
} Site;

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
       elog_die(0, "Can't read parameter file\n");
 
	/* Get Input & Network tables  */
	 
   Site = pfget_tbl(Param, "Site");
   nsite = maxtbl(Site);
    
   if( nsite <= 0  )
     elog_die( 0, "init(): parameter file is not complete.\n");
 
  Dasid = newarr( 0 );
	   
  for( i = 0; i < nsite; i++ )  {

	istr = (char *) gettbl(Site, i);

#define STE_SCS " %s %d %s %s %d %s %s %s %lf[^\n] \n"
 
#define STE_RVL(SP)  \
(SP)->pkttype,&(SP)->sid,(SP)->name,(SP)->up,&(SP)->sensid,(SP)->sens, \
(SP)->trg, (SP)->pick, &(SP)->calib

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
    achan->tbl = inittbl( 0, maxtbl, 1, 0, sizeof(Ch) ); 
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
     ch->pktlta = 0;
     ch->lta = 0;
     ch->mrcnum = 0;
     ch->nsamp = 0;
     dasid = (int *)getarr(Dasid, sta );
     ch->dasid = *dasid ;
     strcpy( ch->srcid, srcid) ;
     ch->chpipe = (ChPipe *) new_chpipe ( maxtbl );
     
     return ch ;
}
	     


void free_chpipe ( ChPipe *achan )
{

    freetbl( achan->tbl, 0)  ;
    free( achan ) ; 
}

