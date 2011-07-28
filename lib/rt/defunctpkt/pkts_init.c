/******************************************************************
 *
 *  init_pkts.c
 *
 *  Init different structure which will be used later.  
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *
 ********************************************************************/
#include "defunctpkt.h"

Arr *StaCh = 0;
Arr *StaID = 0;
Arr *Packets = 0;
Arr *RawPkts = 0;
extern Arr *PsclSTRM;
extern Prm Par;

static struct Raw raw_init[] = { 
     
  {  CBBHS, (int(*)()) parse_bba_DP, (int(*)()) read_DP },
  {  CBB1S, (int(*)()) parse_bba_DP, (int(*)()) read_DP },
  {  CBBLS, (int(*)()) parse_bba_DP, (int(*)()) read_DP },
  {  CAHS,  (int(*)()) parse_bba_DP, (int(*)()) read_DP },
  {  CALS,  (int(*)()) parse_bba_DP, (int(*)()) read_DP },
  {  ASP,   (int(*)()) parse_anza_SP, (int(*)()) read_anza_SP },
  {  BSP,   (int(*)()) parse_anza_SP, (int(*)()) read_anza_SP },
  {  PSCLDT,  (int(*)()) parse_pscl_DP, (int(*)()) read_psclDP },
  {  PSCLAD,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_AD },
  {  PSCLCD,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_CD },
  {  PSCLDS,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_DS },
  {  PSCLEH,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_EH },
  {  PSCLET,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_ET },
  {  PSCLOM,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_OM },
  {  PSCLSH,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_SH },
  {  PSCLSC,  (int(*)())  parse_pscl_IP, (int(*)()) read_pscl_SC },
  {  B3S2DP,  (int(*)()) parse_b3s2, (int(*)()) read_b3s2_DP },
  {  B3S2SP,  (int(*)())  parse_b3s2, (int(*)()) read_b3s2_SP },
  {  ORBDBUG,  (int(*)()) none, (int(*)()) un_dbug },
  {  IWTB,  (int(*)()) none, (int(*)()) unstuff_iw_tracebuf },
  {  LISSPKT,  (int(*)()) none, (int(*)()) unstuff_liss },
  {  0, 0, 0 }
};


Site *new()
{
    Site *site;

    allot( Site *, site, sizeof( Site ) );

    return site;
}
PktPar *pnew()
{
    PktPar *pkt;

    allot( PktPar *, pkt, sizeof( PktPar ) );

    return pkt;
}

void init_StaCh()
{
 
   Pf  *Param;          /* Parameter file  */
   Tbl *Ste;           /* An odered list of packets parameters  */
   char *istr;
   char key[32];
   struct Site site, *new_site;   /* Data packets paraneters structure  */
   int i, elnum;
 
/* Read configuration file  */

   if( DASPF == 0 ) initpf( 0 );
 
   if(pfread( DASPF, &Param) != 0)  {
       elog_die(0, "Can't read parameter file\n");
   }
 
    Ste = pfget_tbl(Param, "Site");
    elnum = maxtbl(Ste);

    if ( elnum > 0 ) StaCh = newarr( 0 );
    else elog_die( 0, "Can't get site parameters. Check parameter file!\n");
 
    for(i = 0; i < elnum; i++)  {
        istr = (char *) gettbl(Ste, i);
        sscanf(istr, STE_SCS,  STE_RVL(&site));
        sprintf(key, "%s_%d_%d\0", site.pkttype, site.sid, site.sensid);

        if( (new_site = (Site *) getarr( StaCh, key ) ) == 0 )
	    new_site = (Site *) new();
        memcpy( (char *) new_site, (char *) &site, sizeof( Site ) );
        setarr(StaCh, (char *)&key[0], (char *) new_site );

    }
}
 
void init_StaID()
{
 
   Pf  *Param;          /* Parameter file  */
   Tbl *Ste;           /* An odered list of packets parameters  */
   char *istr;
   char key[32];
   struct Site site, *new_site;   /* Data packets paraneters structure  */
   int i, elnum;
 
/* Read configuration file  */
   
   if( DASPF == 0 ) initpf( 0 );
 
   if(pfread( DASPF, &Param) != 0)  {
       elog_die(0, "Can't read parameter file\n");
   }
 
    Ste = pfget_tbl(Param, "Site");
    elnum = maxtbl(Ste);
   
    if ( elnum > 0 ) StaID = newarr( 0 );
    else elog_die( 0, "Can't get packet parameters. Check parameter file!\n");
 
    for(i = 0; i < elnum; i++)  {
        istr = (char *) gettbl(Ste, i);
        sscanf(istr, STE_SCS,  STE_RVL(&site));
        sprintf(key, "%s_%s\0", site.pkttype, site.name);
        if( (new_site = (Site *) getarr( StaCh, key ) ) == 0 )
	    new_site = (Site *) new();
        memcpy( (char *) new_site, (char *) &site, sizeof( Site ) );
        setarr(StaID, (char *)&key[0], (char *) new_site); 
    }
}

void init_RawPkts()
{

     int num = 0; 
     char key[64];

     if( RawPkts == 0 ) {
     	RawPkts = newarr( 0 );
     }

     do {
           sprintf(key, "%d\0",  raw_init[num].pkttype );
           setarr( RawPkts, (char *) &key[0], (char *) &raw_init[num++]);
     } while ( raw_init[num].pkttype != 0 );
     
     setupqorbpkt (); 
	  

}


void init_packets()
{
 
   Pf  *Param;          /* Parameter file  */
   Tbl *Inputs;         /* An odered list of packets parameters  */
   char *istr;
   struct PktPar packet, *pkt;          /* Data packets paraneters structure  */
   int i, elnum;
   ushort_t pkttype;
   char key[64];
 
/* Read configuration file  */

   if( DASPF == 0 ) initpf( 0 );
 
   if(pfread( DASPF, &Param) != 0)  {
       elog_die(0, "Can't read parameter file\n");
   }
 
    Inputs = pfget_tbl(Param, "Inputs");
    elnum = maxtbl(Inputs);
 
    if ( elnum > 0 )  {
       Packets = newarr( 0 );
    } else elog_die( 0, "Can't get packet parameters. Check parameter file!\n");
   
    for(i = 0; i < elnum; i++)  {
        istr = (char *) gettbl(Inputs, i);
        sscanf(istr, PKT_SCS,  PKT_RVL(&packet));
        
        pkttype =  (ushort_t) decode( packet.pkttype);
        sprintf(key, "%d\0", pkttype);
        if( ( pkt = ( PktPar *) getarr( Packets, key ) ) == 0 )
	   pkt = (PktPar *) pnew();
	memcpy( pkt, (char *) &packet, sizeof( PktPar) );
	
	setarr( Packets, (char *) &key[0], (char *) pkt );
  
  
    } 
}
 
int get_site( 
    char *pkttype,
    int staid,
    int chid,
    struct Site *site )
 
{
   char key[32];
   struct Site *ste;

   if( StaCh == 0 ) init_StaCh();

   sprintf( key, "%s_%d_%d\0", pkttype, staid, chid);

   ste = ( struct Site *)  getarr( StaCh, key );
   if( ste == 0 )  {
       elog_complain( 0, "Can't get site parameters for %s_%d_%d.\n", pkttype, staid, chid);
       return 0;
    }  else 
       memcpy( (char *) site, (char *) ste, sizeof( struct Site ) );

   return 1;
}
 
 
int get_staid( 
    char *pkttype,
    char *sta,
    struct Site *site )
 
{
   char key[32];
 
   if( StaID == 0 ) init_StaID(); 
   sprintf( key, "%s_%s\0", pkttype, sta);
 
   site =  (Site *) getarr( StaID, key );
   if( site == 0 )  {
       elog_complain( 0, "Can't get site parameters for %s_%s.\n", pkttype, sta);
       return 0;
    }  
   return 1;
}
 
int get_packet( 
    ushort_t key,
    struct PktPar *pack )
 
{
   char pkttype[64];
   struct PktPar *tmp;

   if( Packets == 0 ) init_packets(); 
 
   sprintf( pkttype, "%d\0", key ); 
   tmp = ( struct PktPar *) getarr( Packets, (char *) &pkttype[0] );


  if( tmp == 0 ) return 0;
  else memcpy( (char *) pack, (char *) tmp, sizeof(PktPar) );

  return 1;
}

int
register_pkt_handler (int pkttype, int (*parse_handler)(), int (*read_handler)())

{
	char key[32];
	Raw *raw;

	if( RawPkts == 0 ) init_RawPkts();

	raw = (Raw *) malloc (sizeof(Raw));
	if (raw == 0) {
		elog_log(1, "register_pkt_handler: malloc() error.\n");
		return (-1);
	}
	raw->pkttype = pkttype;
	raw->parse = parse_handler;
	raw->read = read_handler;

        sprintf(key, "%d\0",  pkttype );
        setarr( RawPkts, key, (char *) raw);

	return (0);
}


