#include "diagp.h"

extern Arr *DPOrbs;
extern Arr *DP;
extern Arr *Staname;

int AnzaDas( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet )
{
    float batt;
    int i;
    char cmdstr[256], tmp[128];
    char key[64], net[16], sta[16];
    DPars *dp;
    int val;
    ulong_t *iptr;
    ushort_t *sptr;
    int off, alert;
    int ch, das, dc;
    uchar_t *tmphdr, pkt[64];
    struct PreHdr hdr;
    short tmpval;
    TclOrb *tclorb = (TclOrb *) clientData;
    

    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );

    memcpy( &tmp[0], packet, 128);
    tmphdr = (uchar_t *) &tmp[0];

    memcpy( &tmpval, tmphdr, sizeof(short ));
    tmphdr += sizeof(short );
    hdr.hdrsiz =  ntohs(  tmpval );
    
    memcpy( &tmpval, tmphdr, sizeof( short ));
    tmphdr += sizeof( short );
    hdr.pktsiz =  ntohs(  tmpval );

    memcpy( (char *) &pkt[0], packet+hdr.hdrsiz, 64);
        
    for( i = 0; i < ANZA_DAS; i++ )  {
     
        alert = 0;
        sprintf (key, "%s_%s\0", sta, ANZA_DAS_NAME[i] );
/*
printf( "%s %lf\n", key, pkttime);
*/

        dp = ( DPars *) getarr (tclorb->dpars, key);
        if ( dp != 0) {

           off = ANZA_DAS_OFF[i];
           if( strncmp(ANZA_DAS_NAME[i], "BUFDEL", 6) == 0 ) { 
                 val = pkt[off];
	         sprintf(&tmp[0], "%d\0", val );
	   }  else { 
                 sptr = (ushort_t *) (pkt + off);
                 val = *sptr;
		 batt = (float)val/100.0;
		 sprintf(&tmp[0], "%.2f\0", batt );
                 val = batt;
		if( batt > MAXBATT || batt < MINBATT ) alert = 1;
           }  
/*
           if (tclorb->verbose > 0) {
	       printf ("%lf: %s %d\n", pkttime, key, val );
	       fflush (stdout);
           }
 */
           dp->ptime = pkttime;
           if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {

               if( alert ) { 
                  dp->alert = 1;
                  sprintf ( &cmdstr[0], "%s {%s} {%s} {red} \0", 
                             tclorb->parcallback, dp->widget, &tmp[0] );
               }  else {
                     if( dp->alert ) { 
                          sprintf ( &cmdstr[0], "%s {%s} {%s} {LemonChiffon}\0", 
                                   tclorb->parcallback, dp->widget, &tmp[0] );
                     } else  { 
                          sprintf ( &cmdstr[0], "%s {%s} {%s} {none} \0", 
                                  tclorb->parcallback, dp->widget, &tmp[0] );
                     }
                     dp->alert = 0;
               } 
               if (tclorb->verbose > 0) {
	             printf ("%s\n", cmdstr );
	             fflush (stdout);
               }
               Tcl_Eval (tclorb->interp, cmdstr );
           } 
       }
    }
    return 1;
}
int AnzaDc( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet )
{
    int i;
    char cmdstr[256], tmp[128];
    char key[64], net[16], sta[16];
    char *staname, id_str[8];
    DPars *dp;
    int val;
    ulong_t *iptr;
    ushort_t *sptr;
    int off, alert;
    int ch, das, dc;
    struct PreHdr *hdr;
    TclOrb *tclorb = (TclOrb *) clientData;
    

    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
    hdr = ( struct PreHdr *) packet;
       
    for( i = 0; i < ANZA_DCDC; i++ )  {
        alert = 0;
	
        sprintf (key, "%s_%s\0", sta, ANZA_DCDC_NAME[i] );
/*
printf( "DCDC: %s %lf\n", key, pkttime );
*/
        dp = ( DPars *) getarr (tclorb->dpars, key);
        if ( dp != 0) {
                    
            dp->ptime = pkttime;

	    off = hdr->hdrsiz + ANZA_DCDC_OFF[i];
            if( strncmp(ANZA_DCDC_NAME[i], "STAT", 4) == 0 ) { 
	        val = packet[off];
	        sprintf(&tmp[0], "%d\0", val );
	    } else  if( strncmp(ANZA_DCDC_NAME[i], "LLOCK", 5) == 0 )  {
                iptr = (ulong_t *) (packet + off);
	        sprintf(&tmp[0], "%ld\0", *iptr );
	    } else { 
                sptr = (ushort_t *) (packet + off);
                val = *sptr;
	        sprintf(&tmp[0], "%d\0", val );
            }
/*
            if (tclorb->verbose > 0) {
	         printf ("%lf: %s %d\n", pkttime, key, val );
	         fflush (stdout);
            }
*/

            if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {

                if( alert ) { 
                    dp->alert = 1;
                    sprintf ( &cmdstr[0], "%s {%s} {%s} {red} \0", 
                             tclorb->parcallback, dp->widget, &tmp[0] );
                }  else {
                    if( dp->alert ) { 
                        sprintf ( &cmdstr[0], "%s {%s} {%s} {LemonChiffon}\0", 
                                 tclorb->parcallback, dp->widget, &tmp[0] );
                    } else  { 
                        sprintf ( &cmdstr[0], "%s {%s} {%s} {none} \0", 
                                  tclorb->parcallback, dp->widget, &tmp[0] );
                    }
                    dp->alert = 0;
                } 
                if (tclorb->verbose > 0) {
	            printf ("%s\n", cmdstr );
	            fflush (stdout);
                }
                Tcl_Eval (tclorb->interp, cmdstr );
            }
        }
    }


    for( i = 0; i < ANZA_DCDAS; i++ )  {
        for( ch = 0; ch < DasNum; ch++ )  {
	    alert = 0;
            sprintf( id_str, "%d\0", ch );
            if( (staname = getarr(Staname, id_str)) == 0 ) continue;

            sprintf (key, "%s_%s\0", staname, ANZA_DCDAS_NAME[i] );
/*
printf( "DCDAS: %s %lf ch: %d \n", key, pkttime, ch );
*/
            dp = ( DPars *) getarr (tclorb->dpars, key);
            if ( dp != 0) {

                 off = hdr->hdrsiz + ANZA_DCDAS_OFF[i]+ch*2;
                 sptr = (ushort_t *) (packet + off);
                 val = *sptr;

/*                 
                 if (tclorb->verbose > 0) {
	             printf ("%lf: %s %d\n", pkttime, key, val );
	             fflush (stdout);
                 }
*/
                 dp->ptime = pkttime;
                 if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {

		      sprintf(&tmp[0], "%d\0", val );
 
                      if( alert ) { 
                          dp->alert = 1;
                          sprintf ( &cmdstr[0], 
                                     "%s {%s} {%s} {red} \0", 
                                    tclorb->parcallback, dp->widget, &tmp[0] );
                      }  else {
                          if( dp->alert ) { 
                              sprintf ( &cmdstr[0], 
                                           "%s {%s} {%s} {LemonChiffon}\0", 
                                       tclorb->parcallback, dp->widget, &tmp[0] );
                          } else  { 
                              sprintf ( &cmdstr[0], 
                                          "%s {%s} {%s} {none} \0", 
                                      tclorb->parcallback, dp->widget, &tmp[0] );
                          }
                          dp->alert = 0;
                      } 
                      if (tclorb->verbose > 0) {
	                   printf ("%s\n", cmdstr );
	                   fflush (stdout);
                      }
                      Tcl_Eval (tclorb->interp, cmdstr );
                 }
	     }
	 }
 
    }
    return 1;

}
