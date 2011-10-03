#include "pkt2.h"

struct  net_time {
   ushort_t year;
   ushort_t day;
   uchar_t hour;
   uchar_t minute;
   uchar_t seconds;
   uchar_t dummy;
   ushort_t fracs; 
}; 

typedef struct lisspkt { 
	uchar_t		Seq_ID[6];		/* Sequence number of record */
	uchar_t		Record_Type;		/* Always a 'D' */
	uchar_t		Filler;			/* Always a space */

	uchar_t		Station_ID[5];		/* Station id (space filled) */
	uchar_t		Location_ID[2];		/* Array/Extended Station (filled) */
	uchar_t		Channel_ID[3];		/* Channel Id (space filled) */
	uchar_t		Network_ID[2];		/* Extended Network Type */

	struct net_time	Start_Time;		/* Start time of record */
	ushort_t 	Number_Samps;		/* Number of samples in record */

	short		Rate_Factor;		/* Sample rate factor */
	short		Rate_Mult;		/* Rate Multiplier */

	uchar_t		Activity_Flags;		/* Activity Information */

	uchar_t		IO_Flags;		/* I/O Information */

	uchar_t		Qual_Flags;		/* Data Quality Information */

	uchar_t		Total_Blockettes;	/* Number blockettes to follow */
	int		Time_Correction;	/* Number of .0001 sec correction */
	ushort_t 	Data_Start;		/* Byte where data starts */
	ushort_t 	First_Blockette;	/* Byte of first blockette */
} LissPkt;


struct Data_only {
	ushort_t	Blockette_Type;		/* Blockette identifier */
	ushort_t	Next_Begin;		/* Byte where next blockette begins */

	uchar_t		Encoding;
	uchar_t   	Order;
	uchar_t   	Length;
	uchar_t   	Resv1;
};


#define TRIM(s,l) {int i;for(i=l-1;i>0,s[i-1]==' ';i--);s[i]='\0';}

typedef struct Liss  {
        struct PreHdr prehdr;
        ushort_t nsamp;
        ushort_t doff;
        float    samprate;
} LissHdr;

struct blkhdr {
	short blktype;
	short blkxref;
};

extern int STEIM_Decode();

int liss_ucompress( int *data,
                int pktsize,
                int doff,
                char *indata, 
		ushort_t nsamp, 
		uchar_t dtype )

{

        register int lp,ct;
        register int *oarray, l_data;
        int ret;
        Steim *conf ;
        int *sud, npts;


        oarray=(int *) data;
        ct=0;

      switch ( dtype )  {

         case 1:
         case 32:
  	      for (lp=0; lp<nsamp; lp+=2) {
    	          register short j;
 
      	          j  = (indata[lp] & 0xFF) << 8;
      	          j |= (indata[lp+1] & 0xFF);
     
    	          *oarray++ = (int) j;
    	          ct++;
  	      }
  	      return(ct);
 
         case 3:
              for (lp=0; lp<nsamp; lp+=4) {
                   register int *pt;
                   pt = (int *) &indata[lp];
                   *oarray++ = *pt;
                   ct++;
              }
              return(ct);

	
         case 10:
              conf = ( Steim *) newsteim();
              conf->record = indata;
              conf->record_size = pktsize-doff; 
              conf->sdh.data_offset = 0;
              conf->sdh.nsamp = nsamp;
              conf->level = 1;
              if ( usteim (conf, &sud, &npts) ) {
                      elog_log(0, "unstuffqorbpkt: usteim() error.\n");
                      return (0);
              }
              memcpy (data, sud, nsamp*4);
              conf->record = 0;
              freesteim(conf);
              return (nsamp*4); 
         
         case 11:
              conf = ( Steim *) newsteim();
              conf->record = indata;
              conf->record_size = pktsize-doff; 
              conf->sdh.data_offset = 0;
              conf->sdh.nsamp = nsamp;
              conf->level = 2;
              if ( usteim (conf, &sud, &npts) ) {
                      elog_log(0, "unstuffqorbpkt: usteim() error.\n");
                      return (0);
              }
              memcpy (data, sud, nsamp*4);
              conf->record = 0;
              freesteim(conf);
              return (nsamp*4); 
         
         case 16:
         case 33:
                for (lp=0; lp<(nsamp*2); lp+=2) {
 
                    register short j;
                    register int gr;
    
                    j  = ( indata[lp] & 0x3F) << 8;
                    j |= ( indata[lp+1] & 0xFF);
    
                    j -= 8191;
                    
                    l_data = (int) j;
    
                    gr = ( indata[lp] & 0xC0) >> 6;
    
                    switch(gr) {
                    case 1:
                      l_data <<= 2;
                      break;
                    case 2: 
                      l_data <<= 4;
                      break;
                    case 3:
                      l_data <<= 7;
                      break;
 
                    }
    
                    *oarray++ = l_data;
                    ct++;
                  }
                  return(ct);

         case 20:
               
              conf = ( Steim *) newsteim();
              conf->record = indata;
              conf->record_size = pktsize-doff; 
              conf->sdh.data_offset = 0;
              conf->sdh.nsamp = nsamp;
              conf->level = 2;
              if ( usteim (conf, &sud, &npts) ) {
                      elog_log(0, "unstuffqorbpkt: usteim() error.\n");
                      return (0);
              }
              memcpy (data, sud, nsamp*4);
              conf->record = 0;
              freesteim(conf);
              return (nsamp*4); 
         
         case 30:
              for (lp=0; lp<(nsamp*2); lp+=2) {
 
                  register int j, gr;
 
                  j  = (indata[lp] & 0x0F) << 8;
                  j |= (indata[lp+1] & 0xFF);
 
                  if (j >= 0x800) j -= 4096;
 
                  gr = (indata[lp] & 0xF0) >> 4;
 
                  l_data = (int) j;
                  l_data <<= (10-gr);
 
                  *oarray++ = l_data;
                  ct++;
             }
              
             return ct;

         case 50:
             for (lp=0; lp<nsamp; lp++) {
 
                *oarray++ =  indata[lp];
                ct++;
             }
             return(ct);
              
         default:
           elog_complain(0, "unknown data format %c\n", dtype );
           return -1; 
 
         break;
     }
}

int unstuff_liss(  double time, 
                   char *srcname, 
                   uchar_t *packet, 
                   Packet **Pkt, 
                   void *par )

{

        LissHdr *hdr;
        struct Data_only *tmp;
        PktChannel *achan;
        int *adata;
        int udata[4096];
        int chnum, doff, nbytes;
        char data[4096], *cptr;
        char net[12], sta[12], chan[12];
        uchar_t dformat; 
        LissPkt  *seed_hdr;
        int tmpoff, blktype, gotone; 
        struct blkhdr *newblk;

        hdr = ( LissHdr *) packet;
        doff = hdr->prehdr.hdrsiz+hdr->doff;
    

        seed_hdr = ( LissPkt *) (packet + hdr->prehdr.hdrsiz);
        tmpoff = hdr->prehdr.hdrsiz + seed_hdr->First_Blockette;
        gotone = 0;
	while ( tmpoff > 0 && tmpoff < 4095 )  {
	    newblk = (struct blkhdr *) (packet + tmpoff);
	    blktype = newblk->blktype;
	    if( blktype == 1000 ) {
		tmp = (struct Data_only *) (packet + tmpoff);
		gotone = 1;
		break;
	    }
	    tmpoff = hdr->prehdr.hdrsiz + newblk->blkxref;
        }

	if( !gotone ) return 2;

	dformat = tmp->Encoding;
 
        cptr = &data[0];
        memcpy ( cptr, packet + doff, hdr->prehdr.pktsiz - hdr->doff );

        if( (nbytes = liss_ucompress( 
	    &udata[0], hdr->prehdr.pktsiz, hdr->doff, cptr, hdr->nsamp, dformat) )<= 0 )  {
             elog_complain( 0, "Can't uncompress LISS data.\n");
             hexdump( stderr, packet, 16+hdr->doff+16 );
             fflush(stderr);
             return 0;
        }

          /* Get net and sta names from srcname  */
 
        net[0] = '\0'; sta[0] = '\0'; chan[0] = '\0'; 
        parse_srcname( srcname, &net[0], &sta[0], &chan[0], 0 );

        if( *Pkt == 0 ) *Pkt = newpkt();
        (*Pkt)->pkttype = (int) hdr->prehdr.pkttype;
        (*Pkt)->hdrtype = (int) hdr->prehdr.hdrtype;
        (*Pkt)->nchannels = 1; 

        chnum = 0; 
        achan = (PktChannel *) gettbl((*Pkt)->chan, chnum) ;
        if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
          achan->data = 0;
          strcpy (achan->segtype, "V");
        }
        strcpy( achan->net,  net);
        strcpy( achan->sta,  sta);
        strcpy( achan->chan,  chan);
        achan->time = time; 
        achan->calib = 0.0;
	achan->datatype = trINT;
        achan->nsamp = hdr->nsamp;
        achan->samprate = hdr->samprate;
        if (achan->data == 0) {
            allot ( int *, adata, achan->nsamp ) ;
            achan->data = adata;
        } else if (achan->nsamp * sizeof(int) > achan->nbytes) {
            adata = achan->data;
            reallot ( int *, adata, achan->nsamp ) ;
            achan->data = adata;
        }
        achan->nbytes = achan->nsamp * sizeof(int);
        memcpy (achan->data, (char *) &udata[0], achan->nbytes );
        settbl((*Pkt)->chan, chnum, achan ) ;
        
     return 1;        

}
