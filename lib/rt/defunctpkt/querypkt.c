/* $Name $Revision$ $Date$ */ 
/*********************************************************************
 *
 *
 * querypkt.c
 *
 * Read parameters for specified packet type.
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ********************************************************************/
#include "defunctpkt.h"


int querypkt(
    int pkttype,
    int pcode, 
    void *result )    
{
   Dbvalue *val;	/* RB parameter value */
   PktPar packet;  	/* Data packets paraneters structure  */
   char name[512];
   int smpsize;

   val = (Dbvalue *) result;

/* set default RTDAS parameters value  */
  
   switch (pcode)  {
 	case pktSTANUM:
 	case pktBYTES:
	case pktHDRSIZE:
	case pktCHANNEL_COUNT:
	case pktCHANNEL_NSAMP:
	case pktCHANNEL_BYTES:
	case pktDATATYPE:
           (*val).i = -999;
           break;
	case pktSOURCE:
	case pktSMPRATE:
           (*val).d = -9999999999.999;
           break;
	case pktNET:
           strcpy((*val).t, "NONE");
           break;
        default :
          elog_complain(0, "Invalid parameter code.\n");
          return 0;    
    }

    if( !get_packet(pkttype, &packet ) )  {
	elog_complain(0, "querypkt(): Can't get packet info for %d.\n", pkttype);
	return 0;
    }
   if( packet.pkttype == 0 || packet.net_type == 0 )  {
       elog_complain( 0, "Can't get parameters for %d packet type \n", pkttype);
       return 0;
    }  
    switch (pcode)  {
	case pktBYTES:
	   (*val).i = packet.size;
           return 1;
	case pktHDRSIZE:
	   (*val).i = packet.hdrsiz;
           return 1;
	case pktDATATYPE:
           if( strncmp(packet.datatype, "s2", strlen("s2") ) == 0)
	      (*val).i = trSHORT;
           else if( strncmp(packet.datatype, "s4", strlen("s4") ) == 0)
              (*val).i = trINT;
           else if( strncmp(packet.datatype, "t4", strlen("t4") ) == 0)
              (*val).i = trFLOAT;
           return 1;
	case pktCHANNEL_COUNT:
	   (*val).i = packet.nchan;
           return 1;
	case pktSTANUM:
	   (*val).i = packet.nsta;
           return 1;
        case pktCHANNEL_NSAMP:
           if( strncmp(packet.datatype, "s2", strlen("s2") ) == 0)
	      smpsize = 2;
           else if( strncmp(packet.datatype, "s4", strlen("s4") ) == 0)
	      smpsize = 4;
           else if( strncmp(packet.datatype, "t4", strlen("t4") ) == 0)
	      smpsize = 8;
           (*val).i = ( packet.size - packet.hdrsiz) / (packet.nchan*smpsize);
           return 1;
	case pktCHANNEL_BYTES:
	   (*val).i = ( packet.size - packet.hdrsiz )/packet.nchan;
           return 1;
	case pktSMPRATE:
	   (*val).d = packet.srate;
           return 1;
	case pktNET:
           strcpy(name, packet.net_type);
           name[strlen(packet.net_type)] = '\0';
	   (*val).t =  name;
           return 1;
    }
 
}



/* $Id$ */
