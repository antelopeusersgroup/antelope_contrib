/* $Name $Revision$ $Date$  */ 
/*************************************************************************
 *
 *  pkt/uncompress.c
 *
 *  Routines to uncompress 'ucsd_compressed' and PASSCAL steim compressed data.
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ***********************************************************************/

#include "pkt2.h"
 
union ptr{
    int  *l;
    short *s;
    char  *c;
};
 

/*
 * The following is a section of C code which will ucompress a UCSD packet.
 * It has has not been optimized for speed.
 *  
 */ 
 
int ucsd_ucompress(
    int *smpptr,        
    char *buf,
    int smpcnt,                     /* number of sample in packet (100 or 40)*/
    int nchan )

{
int i, cnt, type, ch; 
int sample_count;
int sample, lword;
short word;
char  byte;
union ptr pckptr;       /* points to start of data section in a packet */
uchar_t ubyte;

        pckptr.c = buf;

/* process packet for all samples */

 for( ch = 0; ch < nchan; ch++ )  {
      if (((unsigned int)pckptr.l) & 1) {
           pckptr.c++;
      }
      memcpy( (char *)&sample, (char *) pckptr.l, sizeof(int));   
      sample = ntohl (sample);
      *pckptr.l++;
      *smpptr++ = sample;                     /* output 1st sample */
      sample_count = 1;

      while (sample_count < smpcnt) {

/* compression tags are on 2 byte boundaries so even up address */
 
          if (((unsigned int)pckptr.l) & 1) pckptr.c++;
 
/* the data block is broken up into sections.
        the first two bytes of each section is a tag.
        this tag contains a type code and a length (in samples).
*/
/* get compression type and sample count */
 
              cnt = type = ntohs (*pckptr.s++);
              cnt &= 0xff;
              type &= 0xf000;
 
/* there are 5 types of data samples:
        4 bit differences
        8 bit differences
        12 bit differences
        16 bit differences
        32 bit samples
*/

            switch (type) {
               case 0x1000:                    /* 4 bit diffs */
                   for (i=0; i<cnt; i++) {
                      if (i&1) {                        /* high nibble */
                          byte = *pckptr.c++;
                          byte = byte << 4;
                          byte = byte >> 4;
                          *smpptr++ = sample = sample - byte;
                       } else    {                        /* low nibble */
                          byte = *pckptr.c;
                          byte = byte >> 4;
                          *smpptr++ = sample = sample - byte;
                      }
                      sample_count += 1;  
                   }
                   if( cnt&1) pckptr.c++;

                   break;
                case 0x2000:            /* 8 bit differences */
                   for (i=0; i<cnt; i++) {
                       byte = *pckptr.c++;
                       *smpptr++ = sample = sample - byte;
                       sample_count++;     
                   }
                   break;
                case 0x3000:            /* 12 bit differences */
                   for (i=0; i<cnt; i++) {
                      if (i&1)   {                    /* highh nibble */
                        word = *pckptr.c++;
                        word = word << 12;
                        word = word >> 4;
                        ubyte = *pckptr.c++;
                        word |= word | ubyte;
                        *smpptr++ = sample = sample - word;
                      } else  {                         /* low nibble */
                        word = *pckptr.c++;
                        word = word << 8;
                        ubyte = *pckptr.c;
                        word |= ubyte;
                        word = word >> 4;
                        *smpptr++ = sample = sample - word;
                      }
                      sample_count++;     
                   }
                   if( cnt&1) pckptr.c++;
                   break;
               case 0x4000:            /* 16 bit differences */
                   for (i=0; i<cnt; i++) {
                       word = ntohs (*pckptr.s++);
                       *smpptr++ = sample = sample - word;
                       sample_count++;     
                   }
                   break;
               default:                                /* 24 bit diferences */
                   for (i=0; i<cnt; i++) {
                      lword = *pckptr.c++;
                      lword = lword << 8;
                      ubyte = *pckptr.c++;
                      lword |= ubyte;
                      lword = lword << 8;
                      ubyte = *pckptr.c++;
                      lword |= ubyte;
                      *smpptr++ = sample = lword;
                      sample_count++;     
                   }
                   break;
           }
       }

   }
   return sample_count;

}

int pscl_ucompress(
    char **data,
    int nsamp,
    int doff )


{

   Steim *conf ;
   int *sud;
   int npts, nerr;
   int psize;



       npts = 0;
       conf = (Steim *) newsteim() ;
       conf->frames_per_record = 15;
       conf->level = 1;
       conf->sdh.data_offset = 0;
       conf->sdh.nsamp = nsamp;
       conf->data_offset = 0;
       conf->record_size = 960;
       conf->record = *data;
       conf->data = 0;


       if ( nerr = usteim ( conf, &sud, &npts) )  {
         freesteim( conf ) ;
         elog_complain( 0, "Steim decompression reports %d errors\n", nerr ) ;
         return 0;
       }
       psize =  npts*sizeof(int) ;
       memcpy( (char *) *data, (char *)sud, psize );
       freesteim( conf ) ;
       return psize;

}

/* $Id$ */
