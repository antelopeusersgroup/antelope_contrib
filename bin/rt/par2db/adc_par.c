/******************************************************************
 *
 *  anza_dcpar.c
 *
 ********************************************************************/
#include "header.h"

extern int DCINT;    
extern char *Pfile;
Tbl *DasId=0;

#define DAS_RVL(SP) \
  &(SP)->up, (SP)->name

#define DAS_TRIM(SP) \
  TRIM((SP)->name,8)
#define DAS_SCS "%d %s[^\n] \n"

int
adc_par( uchar_t *packet,
          double pkttime,
          char *srcname,
	  struct Packet **Pkt
	  )

{

    struct PreHdr *hdr;
    struct PktChannel *achan;
    char *istr, net[64], dcname[8];
    long lval;
    int i, uid, off, ch, val;
    int dasnum, das;
    short sval;
    Pf *pf;
    struct dasid dasid;

    if( DasId == 0 ) { 
       if(pfread( Pfile, &pf) != 0)
           elog_die(0, "Can't read %s parameter file\n", Pfile );
	      
       DasId = pfget_tbl( pf, "DasId" );
       
       if( DasId == 0 ) 
         elog_die(0, "can't get DasId table from a %s parameter file.\n", Pfile);
       dasnum = maxtbl( DasId );
       if( dasnum <= 0 )  
         elog_die(0, "can't get DasId table from a %s parameter file.\n", Pfile);
    } 

    hdr = ( struct PreHdr *) packet;
    
    (*Pkt)->pkttype = ntohl (hdr->pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->hdrtype);

    off = hdr->hdrsiz + 4;
    memcpy( (char *) &sval, (char *) &packet[off], 2);
    uid = sval;
    sprintf( dcname, "%d\0", uid);

    parse_srcname( srcname, &net[0], 0, 0, 0 );

    for( i = 0, ch = 0; i < ANZA_DCDC; i++, ch++ )  {

        achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
        if ( achan == 0 ) {
              allot ( PktChannel *, achan, 1 ) ;
              achan->data = (void *) malloc( sizeof(int)  );
        }
        strcpy( achan->chan, ANZA_DCDC_NAME[i] ) ;
        strcpy( achan->net, net);
        strcpy( achan->sta, dcname);
        achan->samprate = 1.0/DCINT;                                
        achan->calib = 0;                     
        achan->datatype = trINT;
        achan->nsamp = 1;                   
        achan->nbytes = sizeof(int);
        achan->time = pkttime;   
        
	off = hdr->hdrsiz + ANZA_DCDC_OFF[i];

        if( strncmp(ANZA_DCDC_NAME[i], "STAT", 4) == 0 ) { 
            val = packet[off];
        } else  if( strncmp(ANZA_DCDC_NAME[i], "LLOCK", 5) == 0 )  {
            memcpy( (char *) &lval, (char *) &packet[off], 4 );
	    val = lval;                        
	} else { 
            memcpy( (char *) &sval, (char *) &packet[off], 2);
            val = sval;
        }
	memcpy(achan->data, (char *) &val, sizeof(int) );

        settbl((*Pkt)->chan, ch, achan ) ;
     }

     for( i = 0; i < ANZA_DCDAS; i++ )  {
          for( das=0; das < DasNum; das++ )  {
              if( ( istr =  gettbl( DasId, das )) != 0 )  {
                   sscanf( istr, DAS_SCS, DAS_RVL(&dasid));
		   if( dasid.up )  {
                       achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
                       if ( achan == 0 ) {
                            allot ( PktChannel *, achan, 1 ) ;
                            achan->data = (void *) malloc( sizeof(int)  );
                       }
                       strcpy( achan->chan, ANZA_DCDAS_NAME[i] ) ;
                       strcpy( achan->net, net);
                       strcpy( achan->sta, dasid.name);
                       achan->samprate = 1.0/DCINT;                                  
                       achan->calib = 0;                     
                       achan->datatype = trINT;
                       achan->nsamp = 1;                   
      	               achan->nbytes = sizeof(int);
                       achan->time = pkttime;   
                       
                       off = hdr->hdrsiz + ANZA_DCDAS_OFF[i]+das*2;
                       memcpy( (char *) &sval, (char *) &packet[off], 2);
	               val = sval;
                       memcpy(achan->data, (char *) &val, sizeof(int) );

                       settbl((*Pkt)->chan, ch, achan ) ;
                       ch++;
                   }
              }
          }
     }
  
     (*Pkt)->nchannels = ch;

  return 1; 
}




