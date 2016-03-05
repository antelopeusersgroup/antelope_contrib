/****************************************************************************
 *
 *   util.c 
 *
 *   utilities to manipulate raw/ORB packets  
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *****************************************************************************/
#include <stdio.h>
#include <string.h>
#include "pkt2.h"

extern struct Prm Par;
Arr *Err_msg=0;

int str2int(
    uchar_t *str,
    int i )
{
    char tmp[50];
    strncpy(tmp, (char *) str, i);
    tmp[i]='\0';
    return atoi(tmp);
}
 
/* convert string to UPPER case  */

char *ucase( char *string)
 {
     int i;
  
     for (i = 0; i < strlen(string); i++)
      if (islower(string[i])) string[i] = toupper(string[i]);
         return string;
 }
  

/* Get a basename from the path  */

void bsname(
       char *fname,             
       char *name,             
       char *exten )          
{
int i, j;
char tmp_name[132];       
char *tmp;       

        name[0] = '\0';
 
/* Get file name from full name     */

        for(j = 0,i = strlen(fname)-1; i >= 0; j++,i --)
             if (fname[i] == '/') break;
       
        strncpy(tmp_name, fname + i + 1, j);
        tmp_name[j] = '\0';
        strcpy(name, tmp_name);
        
/* Cut extention if such is specified */

        if(exten != NULL)  {
           tmp = strstr(name, exten);
           if(strcmp(tmp, exten) == 0)
              name[strlen(name)-strlen(exten)] = '\0';
        }
}

/* Get path from the file name */

void pathfrname( char *path, char *name )
{
int i;

        for(i = strlen(path)-1; i >= 0; i --)
             if (path[i] == '/') break;
        if(i < 0) *name = NULL;
        else  {
             strncpy(name, path, i);
             name[i] = '\0';
        }
}

/* Get file extension  */

void fexten( char *fname, char *name ) 
{
int i;
 
 
        for(i = strlen(fname); i > 0; i --)
             if (fname[i] == '.') break;
        if( i == 0 && fname[i] == '\0' )  {
           name[0] = '\0';
        }  else  {
           strncpy(name, fname + i + 1, strlen(fname) - i - 1);
           name[strlen(fname) - i - 1] = '\0';
        }
        
}

int parse_srcname(
    char *srcname,
    char *net,
    char *sta,
    char *chan,
    char *pkttype ) 
    
  { 
    char *src;
    int i, ptype = 0;
    int nelem = 0;
    char tmp[64];

    if( !strncmp( srcname, "/", 1 ) ) return 0;

    if( net != 0 ) src = &net[0];
    else src = &tmp[0];
    for( i= 0; i <  strlen( srcname ); i ++)  {
      if ( srcname[i] == '_' ) break;
      if( srcname[i] == '/' ) { ptype = 1 ; break; } 
      *src = srcname[i]; 
      src++;
    } 
    *src = '\0';
    nelem++;

    if( ptype ) return nelem;
    else  {

       if( sta != 0 ) src = &sta[0];
       else src = &tmp[0];
       for( i++; i <  strlen( srcname ); i ++)  {
         if ( srcname[i] == '_' ) break;
         if( srcname[i] == '/' ) { ptype = 1 ; break; } 
         *src = srcname[i]; 
         src++;
       } 
       *src = '\0';
       nelem++;
    }
    if( ptype ) return nelem;
    else  {

       if( chan != 0 ) src = &chan[0];
       else src = &tmp[0];
       for( i++; i <  strlen( srcname ); i ++)  {
         if ( srcname[i] == '_' ) break;
         if( srcname[i] == '/' ) { ptype = 1 ; break; } 
         *src = srcname[i]; 
         src++;
       } 
       *src = '\0';
       nelem++;
    }

    if( pkttype != 0 ) src = &pkttype[0];
    else src = &tmp[0];
    for( i++; i <  strlen( srcname ); i ++)  {
         if ( srcname[i] == '/' ) break;
         *src = srcname[i]; 
         src++;
    }
    *src = '\0';
    nelem++;

return nelem;

}
    /* Call SPECIFIC routine to read packets  */


int none( ) {
    return 1;
}

int read_raw( 
    double time, 
    char *srcid, 
    uchar_t *packet,
    Packet **Pkt,
    void *private,
    int (*rd) () )
{
       return ( (*rd) ( time, srcid, packet, Pkt, private ) );
}

/* Call SPECIFIC routine to parse packet  */

int parse_raw( 
    uchar_t *packet,
    int (*parse) (),
    ushort_t  pkttype ) 
{
     
       return ( (* parse)( packet, pkttype ));
}

/* Free Packet structure  */
 
void freePktCh( PktChannel *PktCh)
{
    
        if( PktCh->data != NULL ) free( PktCh->data ); 
        free( (void *) PktCh );
}
 
void freePkt( Packet *Pkt)
 
{
        freetbl( Pkt->chan, freePktCh );
	    free( (void *) Pkt ); 
}


int stuffpktcalib( char *packet, double calib) 

{
  struct BBAHdr *hdr;
  IW_ORB_TRACE_HEADER *iwhdr;
  int retcode ;
  int pkttype ;

  hdr = ( struct BBAHdr *) packet;

  pkttype = ntohs (hdr->prehdr.pkttype) ;

  if( pkttype == -1 )  {
    elog_complain( 0, "Can't get packet type.\n" );
    return 0;
  }
 
  switch (pkttype) {
    case DAAB:
    case CBBHS:
    case CBBLS:
    	  hdr->calib = calib;
    	  retcode = 1;
	  break; 
    case IWTB:
    	  iwhdr = (IW_ORB_TRACE_HEADER *) (packet + sizeof(struct PreHdr));
    	  iwhdr->calib = calib;
    	  retcode = 1;
	  break;

    default:
	elog_complain( 0, "stuffpktcalib: Unknown packet type - %d\n", pkttype);
	retcode = 0 ; 
	break ;
  } 

    return retcode ; 
}



int stapar( 
	uchar_t *data,
	char *sta, 
	char *chan,
	int *chanlen,
	double *calib )

{

  Site site;
  short staid;
  int i, nchan;
  int len = 0;

  if( Par.staid < 0  )  {
      elog_complain( 0, "Wrong STAID - %d\n", Par.staid);
      return 0;
  }
   
  memset( chan, 0, 64);
  memcpy( chan, "_", strlen("_"));
  len++;
 
  for( i = 0, nchan = 0; i < Par.packet.nchan; i++, nchan++ )  {
      if( Par.packet.nchan == 1 ) i = Par.chan - 1;
      if( !get_site( Par.packet.pkttype, Par.staid, i + 1, &site))  {
        elog_complain( 0, "can't get site info for STAID:%d CHID:%d PKTTYPE:%s\n", 
                      Par.staid, i+1, Par.packet.pkttype);
        return 0;
      } 
      strcat( chan, site.sens);
      len += strlen( site.sens);
      strcat( chan, "_");
      len++;
  }  
  strcpy( sta, site.name);
  *calib = site.calib;
 
  *chanlen = len;
  return nchan;

}

int get_sta_name()
{
   char key[64], *sta, *staerr;

   sprintf(key, "%d\0", Par.staid);
   if(StaName == 0) init_StaName();
   if((sta = getarr(StaName, key)) != 0) 
      strcpy(Par.staname, sta);
   else {
      sprintf(Par.staname, "%d\0", Par.staid );
      if(Err_msg == 0)  Err_msg = newarr(0);
      staerr = (char *) getarr(Err_msg, Par.staname);
      if(staerr == 0)  {
          elog_complain(0, "can't get sta name for staid=%d\n", Par.staid);
          staerr = strdup(Par.staname);
	  setarr(Err_msg, Par.staname, staerr);
      }  
      return(0);
   }
   return(1);
} 

char *get_chname_from_id(ushort_t pkttype, char *ptype, int staid, int chid)
{
   char key[64], *name=0, *cherr=0;
   struct Site *site;

   switch(pkttype)  {

      case 0xdaab:  /* Data packets */
          
         sprintf(key, "%s_%d_%d\0", ptype, staid, chid);
         if(StaCh == 0) init_StaCh();
         if((site = (Site *) getarr(StaCh, key)) != 0)  { 
             name = strdup(site->sens);
	     return(name);
         } else  {
             if(Par.packet.srate < 10)
	     	sprintf(&key[0], "LS_%d\0", chid );
	     else if(Par.packet.srate >= 100)
	     	sprintf(&key[0], "HS_%d\0", chid);
	     else sprintf(&key[0], "BS_%d\0", chid);
	     name = strdup(key);
             if(Err_msg == 0)  Err_msg = newarr(0);
             cherr = (char *) getarr(Err_msg, key);
             if(cherr == 0)  {
	        elog_complain(0, "can't get channel name (pkt=%s staid=%d, chid=%d)\n",
                   ptype, staid, chid);
                 cherr = strdup(key);
	         setarr(Err_msg, key, cherr);
             }  
             return(name);
         }
   
      case 0xdabc:  /* DAS status packets */
 
         sprintf(key, "%d\0", chid);
         if(DasID == 0) init_DasID();
         if((name = getarr(DasID, key)) == 0) 
            elog_complain(0, "can't get DAS parameter name (staid=%d, chid=%d)\n", 
                staid, chid);
         break;
   
      case 0xdacd:  /* DC status packets */
 
         sprintf(key, "%d\0", chid);
         if(DcID == 0) init_DcID();
         if((name = getarr(DcID, key)) == 0) 
            elog_complain(0, "can't get DC parameter name (staid=%d, chid=%d)\n", 
                staid, chid);
         break; 
      case 0xdade:  /* DAS RTX packets */
 
         sprintf(key, "%d\0", chid);
         if(RTXID == 0) init_RTXID();
         if((name = getarr(RTXID, key)) == 0) 
            elog_complain(0, "can't get RTX parameter name (staid=%d, chid=%d)\n", 
                staid, chid);
         break;
   }
   if(name)
       return(strdup(name));
   else return(0);
} 

 /* 
 *  Decode an ASCII representation of a Raw packet type, pkttype to
 *  specific value.
 */

int decode( char *token )

{

    if (token == NULL) return -1;
 
    ucase(token);
 
/* Packet's Type definitions  */

    if (strncmp(token,"BBA/HS",4) == 0)    return DAAB;
    if (strncmp(token,"BBA/BS",4) == 0)    return DAAB;
    if (strncmp(token,"BBA/LS",4) == 0)    return DAAB;
    if (strncmp(token,"BBA/DAS",4) == 0)    return DABC;
    if (strncmp(token,"BBA/DC",4) == 0)    return DACD;
    if (strncmp(token,"BBA/RTX",4) == 0)    return DADE;
    if (strncmp(token,"CBBHS",5) == 0)    return CBBHS;
    if (strncmp(token,"CBBLS",5) == 0)    return CBBLS;
    if (strncmp(token,"CBB1S",5) == 0)    return CBB1S;
    if (strncmp(token,"BSP",3) == 0)      return BSP;
    if (strncmp(token,"IWTB",4) == 0)      return IWTB;

    if (strncmp(token,"PSCLIP",6) == 0)     return PSCLIP;
    if (strncmp(token,"CPSCLLS",7) == 0)    return CPSCLLS;
    if (strncmp(token,"CPSCLHS",7) == 0)    return CPSCLHS;
    if (strncmp(token,"PSCLLS",6) == 0)     return PSCLLS;
    if (strncmp(token,"PSCLHS",6) == 0)     return PSCLHS;
    
    if (strncmp(token,"B3S2DP",6) == 0)     return B3S2DP;
    if (strncmp(token,"B3S2SP",6) == 0)     return B3S2SP;
    if (strncmp(token,"PSCLDT",6) == 0)     return PSCLDT;
    if (strncmp(token,"PSCLAD",6) == 0)     return PSCLAD;
    if (strncmp(token,"PSCLCD",6) == 0)     return PSCLCD;
    if (strncmp(token,"PSCLDR",6) == 0)     return PSCLDR;
    if (strncmp(token,"PSCLDS",6) == 0)     return PSCLDS;
    if (strncmp(token,"PSCLEH",6) == 0)     return PSCLEH;
    if (strncmp(token,"PSCLET",6) == 0)     return PSCLET;
    if (strncmp(token,"PSCLOM",6) == 0)     return PSCLOM;
    if (strncmp(token,"PSCLSH",6) == 0)     return PSCLSH;
    if (strncmp(token,"PSCLSC",6) == 0)     return PSCLSC;

/* Header type  */

    if (strncmp(token,"BBA",3) == 0)      return BBA;
    if (strncmp(token,"IPH",3) == 0)      return IPH;
    if (strncmp(token,"IWH",3) == 0)      return IWH;
    if (strncmp(token,"SPP",3) == 0)      return SPP;
     
  return -1;

}

 /*
 *  Conver BCD values to int, char *, hex .
 *
 */

void *bcdval( 
     uchar_t *data,
     int code,
     int nbyte )

{
       int i, n ; 
       static int ival;
       static uchar_t unbcd[1024]; 

       switch ( code )  {
         case valINT:
            ival = 0;
            n = 2*nbyte - 1;
            for( i = 0; i < nbyte; i++, n--)  {
              unbcd[2*i] = ( data[i] >> 4 ) + 0x30;
              unbcd[2*i + 1] = ( data[i] & 0x0F ) + 0x30;
            }
            unbcd[2*i] = '\0';
            sscanf( (char *)&unbcd[0], "%d", &ival);
            return &ival;
         case valSTR:

            for( i = 0; i < nbyte; i++)  {
              unbcd[2*i] = ( data[i] >> 4 ) + 0x30;
              unbcd[2*i + 1] = ( data[i] & 0x0F ) + 0x30;
            } 
            unbcd[2*i] = '\0'; 
            return (char *) &unbcd[0];
       } 


}
int bcd2hex( 
    uchar_t *data,
    int nbytes )

{
    int val, i;
   
    for( i = 0, val = 0; i < nbytes; i+= 2 )  {
       val = 10 * ( 10 * val + (*data) / 16 ) + (*data)%16;
       data++;
    }

  return val;
}

