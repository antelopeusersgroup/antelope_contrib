#include "ref2db.h"

extern Arr *StaCh;
extern Tbl *DasList;

void init_daslist( Tbl *dases, int ndas )

{
     struct das {
        int dasid;
        char stime[24];
 	char etime[24];
	char name[8];
     };
     struct das newdas;
     struct Daslist *indas;
     char *istr;
     int i;

     DasList = inittbl( 0, ndas+1, 1, 0, sizeof(Daslist) );
     for( i = 0; i < ndas; i++ )  {
        istr = (char *) gettbl(dases, i);
        sscanf(istr, DAS_SCS,  DAS_RVL(&newdas));
 
        allot( Daslist *, indas, 1 );
        indas->dasid = newdas.dasid;
        indas->stime = str2epoch( newdas.stime);
        indas->etime = str2epoch( newdas.etime);
        strcpy( indas->name, newdas.name );
        pushtbl( DasList, (char *) indas ); 
    }


}
void 
init_pkt ( SpecPar *par  )
{

   struct PktPar *pkt;
   int pkttype;
   char key[64];

   Packets = newarr(0);
   pkt = (PktPar *) pnew();

   pkt->srate = 0.0;
   pkt->nsamp = 0.0;
   pkt->size = 1024;
   pkt->timtag = 6;
   pkt->nsta = 1;
   pkt->nchan = 1;
   pkt->hdrsiz = 16;
   strcpy( pkt->datatype, "xx");
   strcpy( pkt->pkttype, "PSCLIP");
   strcpy( pkt->hdrtype, "IPH");
   strcpy( pkt->net_type, par->network );

   pkttype =  (ushort_t) decode( pkt->pkttype);
   sprintf(key, "%d\0", pkttype);
   setarr( Packets, (char *) &key[0], (char *) pkt );

   pkt = (PktPar *) pnew();
   pkt->srate = 0.0;
   pkt->nsamp = 0.0;
   pkt->size = 1024;
   pkt->timtag = 6;
   pkt->nsta = 1;
   pkt->nchan = 1;
   strcpy( pkt->datatype, "xx");
   strcpy( pkt->net_type, par->network );
   
   pkt->hdrsiz = 64;
   strcpy( pkt->pkttype, "CPSCLHS");
   strcpy( pkt->hdrtype, "BBA");

   pkttype =  (ushort_t) decode( pkt->pkttype);
   sprintf(key, "%d\0", pkttype);
   setarr( Packets, (char *) &key[0], (char *) pkt );

   pkt = (PktPar *) pnew();
   pkt->srate = 0.0;
   pkt->nsamp = 0.0;
   pkt->size = 1024;
   pkt->timtag = 6;
   pkt->nsta = 1;
   pkt->nchan = 1;
   strcpy( pkt->datatype, "xx");
   strcpy( pkt->net_type, par->network );
   pkt->hdrsiz =64;
   strcpy( pkt->pkttype, "CPSCLLS");
   strcpy( pkt->hdrtype, "BBA");

   pkttype =  (ushort_t) decode( pkt->pkttype);
   sprintf(key, "%d\0", pkttype);
   setarr( Packets, (char *) &key[0], (char *) pkt );

   pkt = (PktPar *) pnew();
   pkt->srate = 0.0;
   pkt->nsamp = 0.0;
   pkt->size = 1024;
   pkt->timtag = 6;
   pkt->nsta = 1;
   pkt->nchan = 1;
   strcpy( pkt->datatype, "xx");
   strcpy( pkt->net_type, par->network );
   pkt->hdrsiz = 24;
   strcpy( pkt->pkttype, "PSCLHS");
   strcpy( pkt->hdrtype, "BBA");

   pkttype =  (ushort_t) decode( pkt->pkttype);
   sprintf(key, "%d\0", pkttype);
   setarr( Packets, (char *) &key[0], (char *) pkt );

   pkt = (PktPar *) pnew();
   pkt->srate = 0.0;
   pkt->nsamp = 0.0;
   pkt->size = 1024;
   pkt->timtag = 6;
   pkt->nsta = 1;
   pkt->nchan = 1;
   strcpy( pkt->datatype, "xx");
   strcpy( pkt->net_type, par->network );
   pkt->hdrsiz = 24;
   strcpy( pkt->pkttype, "PSCLLS");
   strcpy( pkt->hdrtype, "BBA");

   pkttype =  (ushort_t) decode( pkt->pkttype);
   sprintf(key, "%d\0", pkttype);
   setarr( Packets, (char *) &key[0], (char *) pkt );

}
void 
update_site ( )
{

   struct Site   *ste, site;
   char key[64], *name;

   if( StaCh == 0 ) StaCh = newarr(0);
   sprintf( key, "%s_%d_%d\0", Par.packet.pkttype, Par.staid, Par.chan );
    
   ste = ( struct Site *)  getarr( StaCh, key );
   if( ste == 0 )  {
       ste = ( Site *) new();
       ste->sid = Par.staid;
       ste->sensid = Par.chan;
       strcpy( ste->up, "Y");
       strcpy( ste->trg, "Y" );
       strcpy( ste->pick, "Y" );
       strcpy( ste->pkttype, Par.packet.pkttype);
       sprintf( ste->sens, "%d", Par.chan);
       sprintf( ste->name, "%d", Par.staid );
       if( DasList != 0 )  {
           int dasnum, i;
           Daslist *das;

           dasnum = maxtbl(DasList);
           for( i = 0; i < dasnum; i++ )  {
              das = (Daslist *) gettbl( DasList, i );
	      if( das == 0 ) elog_die( 0, "DasList is corrupted\n");
              if( das->dasid == Par.staid &&             
                  Par.time >= das->stime &&
                  Par.time <= das->etime )          
                  strcpy( ste->name, das->name );
           }
      }

      setarr( StaCh, (char *) &key[0], (char *) ste );
   }
           
}
