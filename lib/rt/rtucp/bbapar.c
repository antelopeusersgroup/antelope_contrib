#include "diagp.h"

#define DC_MINBATT  2400
#define MAX_FOR_AVR 360

extern Arr *Das_ID_toName;
extern Arr *DPOrbs;
extern Arr *DP;
extern Arr *BatRec;

int BBA_DasPar( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet )
{
    char cmdstr1[256], 
         cmdstr2[256], 
         cmdstr3[256];
    char sta[12], net[12], key[64];
    ushort_t  *sptr, sval;
    struct PreHdr hdr;
    DPars *dp;
    int i, ch, off;
    int alert;
    float fval = 0.0;
    int val = 0;
    uchar_t *tmphdr, pkt[64];
    short tmpval;
    TclOrb *tclorb = (TclOrb *) clientData;
 

        parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
     
        memcpy( &key[0], packet, 64);
        tmphdr = (uchar_t *) &key[0];
 
        memcpy( &tmpval, tmphdr, sizeof(short ));
        tmphdr += sizeof(short );
        hdr.hdrsiz =  ntohs(  tmpval );
 
        memcpy( &tmpval, tmphdr, sizeof( short ));
        tmphdr += sizeof( short );
        hdr.pktsiz =  ntohs(  tmpval );
 
        memcpy( (char *) &pkt[0], packet+hdr.hdrsiz, 64);
 
 
        for( i = 0; i < BBA_DAS; i++ )  {
 
             sprintf (key, "%s_%s\0", sta, BBA_DAS_NAME[i] );
             alert = 0;
             dp = ( DPars *) getarr (tclorb->dpars, key);
             if ( dp != 0) {

                 off = BBA_DAS_OFF[i];
                 if( strncmp(BBA_DAS_NAME[i], "BUFDEL", 6) == 0 ) { 
                        val = pkt[off];
                        dp->crnt_val = val;
                 } else if( strncmp(BBA_DAS_NAME[i], "LLOCK", 5) == 0 ) { 
                        val = pkt[off];
                        dp->crnt_val = val;
                        alert = check_llock( dp, val );
                 }  else  if( strncmp(BBA_DAS_NAME[i], "BATT", 4) == 0 )  {
                        memcpy( (char *) &sval, pkt + off, 2 );
                        fval = (float) sval / 100.0;
                        dp->crnt_val = fval;
                        alert = check_batt( dp, fval );
                 } else {

                        memcpy( (char *) &sval, pkt + off, 2 );
                        dp->crnt_val = sval;
                 }
                if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {

                     dp->min_val = ( dp->crnt_val < dp->min_val )?dp->crnt_val:dp->min_val;
                     dp->max_val = ( dp->crnt_val > dp->max_val )?dp->crnt_val:dp->max_val;
                     dp->ptime = pkttime;

                     if( alert ) { 
                          dp->alert = 1;
                          sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {red} \0", 
                                    tclorb->parcallback, dp->widget, dp->crnt_val );
                          if( alert < 0 )
                               sprintf ( &cmdstr1[0], "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->minwidget, dp->min_val );
                          else 
                               sprintf ( &cmdstr3[0], "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->maxwidget, dp->max_val );
                     }  else {
                          sprintf ( &cmdstr1[0], "%s {%s} {%.2f} {none}\0", 
                                     tclorb->parcallback, dp->minwidget, dp->min_val );
                          sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {none}\0", 
                                     tclorb->parcallback, dp->widget, dp->crnt_val );
                          sprintf ( &cmdstr3[0], "%s {%s} {%.2f} {none}\0", 
                                     tclorb->parcallback, dp->maxwidget, dp->max_val );
                          if( dp->alert ) { 
                              sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {LemonChiffon}\0", 
                                           tclorb->parcallback, dp->widget, dp->crnt_val );
                          } 
                          dp->alert = 0;
                     } 
                     if (tclorb->verbose > 0) {
	                 printf ("%s\n", cmdstr1 );
	                 printf ("%s\n", cmdstr2 );
	                 printf ("%s\n", cmdstr3 );
	                 fflush (stdout);
                     }
                     Tcl_Eval (tclorb->interp, cmdstr1 );
                     Tcl_Eval (tclorb->interp, cmdstr2 );
                     Tcl_Eval (tclorb->interp, cmdstr3 );
                } 
            }
        }
     return 1;  

}

Batrecord *new_batrec( char *key )
{
	Batrecord *new;
        allot( Batrecord *, new, 1) ;
        new->avr = 0.0;
        new->key = strdup( key );
        new->vpipe = inittbl( 0, MAX_FOR_AVR+1, 1, 0, sizeof(int) ); 
        return new;
}

int check_dcbatt( char *key, int sval, ushort *avr )
{
      Batrecord *acmp;
      Tbl *vpipe;
      char *tmp;
      int old_batt = 0;
      int maxnum=1, alert=0;

      if( BatRec == 0 )  BatRec = newarr(0);
          
      acmp = ( Batrecord *) getarr( BatRec, key );
      if( acmp == 0 )  
         acmp = ( Batrecord *) new_batrec( key );
          
      maxnum = maxtbl( acmp->vpipe ); 
      if( maxnum > MAX_FOR_AVR )  {
         if( (tmp = shifttbl( acmp->vpipe)) != 0 )  {
             memcpy( (char *) &old_batt, tmp, sizeof(int) );
         }
      }
      pushtbl( acmp->vpipe, (char *) &sval );
      maxnum = maxtbl( acmp->vpipe ); 
      
/*fprintf( stderr, "%d + %d -  %d \n", acmp->avr, sval, old_batt );  */

      acmp->avr = acmp->avr + sval - old_batt;

/* fprintf( stderr, "%d dev %d \n", acmp->avr, maxnum );  */

      *avr = acmp->avr / maxnum; 
/*
fprintf( stderr, "%s %d %d %d %d %d\n", key, maxnum, sval, old_batt, acmp->avr, *avr );
fflush(stderr);
*/
      setarr( BatRec, key, (char *) acmp);

      if( maxnum >= MAX_FOR_AVR && *avr < DC_MINBATT ) alert = 1;
    
      return alert;

}

int check_batt( dp, val ) 
DPars *dp; 
float val ;
{

  int alert;

  if ( val > MAXBATT )
     alert = 1;
  else if(  val < MINBATT )
     alert = -1;
  else alert = 0; 

  return alert;
}

int check_llock( DPars *dp, int val )  {

  int alert;
 
  if( dp->last_lock <= 0.0 ) {
      dp->last_lock = dp->ptime;
      return 0;
  } 
  if ( val != 0 ) {
     dp->last_lock = dp->ptime;
     alert = 0;
  } else if (  dp->ptime - dp->last_lock > MAXUNLOCK ) {
     alert = 1;
  } else alert = 0; 

  return alert;
}

int BBA_DcDc( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet )
{
    char cmdstr1[256], 
         cmdstr2[256], 
         cmdstr3[256];
    char key[64], sta[12], net[12];
    char *staname;
    DPars *dp;
    float fval;
    ulong *iptr, lval;
    ushort_t *sptr, sval;  
    uchar_t *daspkt; 
    int alert;
    int i, ch, off;
    struct PreHdr *hdr;
    int val = 0;
    int numdas = 0;
    TclOrb *tclorb = (TclOrb *) clientData;
 


        parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
        hdr = ( struct PreHdr *) packet;
       
        off = UID_DC+hdr->hdrsiz;
        memcpy( (char *) &sval, (char *) &packet[off], 2 );
        sprintf( (char *) &sta[0], "%d\0", sval );
             
      
        for( i = 0; i < BBA_DCDC; i++ )  {
            alert = 0;    
            sprintf (key, "%s_%s\0", sta, BBA_DCDC_NAME[i] );
 
            dp = ( DPars *) getarr (tclorb->dpars, key);
            if ( dp != 0) {
 
                 off = hdr->hdrsiz + BBA_DCDC_OFF[i];
                 if( strncmp(BBA_DCDC_NAME[i], "TCPSND", 6) == 0 ) { 
                     memcpy ( (char *) &lval, packet + off, 4 );
                     dp->crnt_val = lval;
	             alert = 0;
                 }  else  if( strncmp(BBA_DCDC_NAME[i], "BATT", 4) == 0 )  {
                     memcpy ( (char *) &sval, packet + off, 2 );
                     fval = (float) sval / 100.0;
                     dp->crnt_val = fval;
                     alert = check_dcbatt( key, sval, &sval );
                 } else { 
                     memcpy ( (char *) &sval, packet + off, 2 );
                     dp->crnt_val = sval;
	             alert = 0;
                 }
                 if (tclorb->verbose > 0) {
                     printf ("%lf: %s %d\n", pkttime, key, val );
                     fflush (stdout);
                 }
 
                 dp->ptime = pkttime;
                 if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {
 
                     dp->min_val = ( dp->crnt_val < dp->min_val )?dp->crnt_val:dp->min_val;
                     dp->max_val = ( dp->crnt_val > dp->max_val )?dp->crnt_val:dp->max_val;
                     if( alert ) { 
                         dp->alert = 1;
                         sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->widget, dp->crnt_val );
                         if( alert < 0 )
                              sprintf ( &cmdstr1[0], "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->minwidget, dp->min_val );
                         else 
                              sprintf ( &cmdstr3[0], "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->maxwidget, dp->max_val );
                     }  else {
                         sprintf ( &cmdstr1[0], "%s {%s} {%.2f} {none} \0", 
                                          tclorb->parcallback, dp->minwidget, dp->min_val );
                         sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {none} \0", 
                                          tclorb->parcallback, dp->widget, dp->crnt_val );
                         sprintf ( &cmdstr3[0], "%s {%s} {%.2f} {none} \0", 
                                          tclorb->parcallback, dp->maxwidget, dp->max_val );
                         if( dp->alert ) { 
                              sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {LemonChiffon}\0", 
                                           tclorb->parcallback, dp->widget, dp->crnt_val );
                         } 
                         dp->alert = 0;
                    } 
                    if (tclorb->verbose > 0) {
	                  printf ("%s\n", cmdstr1 );
	                  printf ("%s\n", cmdstr2 );
	                  printf ("%s\n", cmdstr3 );
	                  fflush (stdout);
                    }
                    Tcl_Eval (tclorb->interp, cmdstr1 );
                    Tcl_Eval (tclorb->interp, cmdstr2 );
                    Tcl_Eval (tclorb->interp, cmdstr3 );
                }       
            }
       } 
      return 1;
}
      
int BBA_DcDas( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet )
{
    char cmdstr1[256], 
         cmdstr2[256], 
         cmdstr3[256];
    char key[64], sta[12], net[12];
    char *staname;
    DPars *dp;
    ulong *iptr;
    ushort_t *sptr, sval;  
    uchar_t *daspkt; 
    int alert;
    int i, ch, off;
    struct PreHdr *hdr;
    int val = 0;
    int numdas = 0;
    TclOrb *tclorb = (TclOrb *) clientData;
 


       parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
       hdr = ( struct PreHdr *) packet;
       
       off = DASNUM+hdr->hdrsiz;
       sptr = ( ushort_t *) (packet + off);
       numdas = *sptr; 
 
       for( i = 0; i < numdas; i++ )  {
                daspkt = (uchar_t *) (packet + hdr->hdrsiz + DCSIZ + i * DASSIZ);     
                off = UID_DAS;
                memcpy( (char *) &sval, (char *) &daspkt[off], 2 );
                sprintf( (char *) &sta[0], "%d\0", sval );
                if( ( staname = getarr( Das_ID_toName, sta ) ) == 0 ) {
                   printf( "Can't get DAS name for %d DAS id\n", sval );
                   continue;
                }

                for( ch = 0; ch < BBA_DCDAS; ch++ )  {
                    sprintf (key, "%s_%s\0", staname, BBA_DCDAS_NAME[ch] );
 
                    dp = ( DPars *) getarr (tclorb->dpars, key);
                    if ( dp != 0) {

                        dp->ptime = pkttime;
			alert = 0;
 
                        off = BBA_DCDAS_OFF[ch];
                        memcpy( (char *) &sval, &daspkt[off], 2 );
                        dp->crnt_val = sval;
                        dp->min_val = ( dp->crnt_val < dp->min_val )?dp->crnt_val:dp->min_val;
                        dp->max_val = ( dp->crnt_val > dp->max_val )?dp->crnt_val:dp->max_val;
                        if( alert ) { 
                             dp->alert = 1;
                             sprintf ( &cmdstr2[0], 
                                       "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->widget, dp->crnt_val );
                             if( alert < 0 )
                                 sprintf ( &cmdstr1[0], 
                                       "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->minwidget, dp->min_val );
                             else 
                                 sprintf ( &cmdstr3[0], 
                                       "%s {%s} {%.2f} {red} \0", 
                                       tclorb->parcallback, dp->maxwidget, dp->max_val );
                         }  else {
                             sprintf ( &cmdstr1[0], "%s {%s} {%.2f} {none} \0", 
                                       tclorb->parcallback, dp->minwidget, dp->min_val );
                             sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {none} \0", 
                                       tclorb->parcallback, dp->widget, dp->crnt_val );
                             sprintf ( &cmdstr3[0], "%s {%s} {%.2f} {none} \0", 
                                       tclorb->parcallback, dp->maxwidget, dp->max_val );
                             if( dp->alert ) { 
                                 sprintf ( &cmdstr2[0], "%s {%s} {%.2f} {LemonChiffon}\0", 
                                           tclorb->parcallback, dp->widget, dp->crnt_val );
                             } 
                             dp->alert = 0;
                        } 
                        if (tclorb->verbose > 0) {
	                     printf ("%s\n", cmdstr1 );
	                     printf ("%s\n", cmdstr2 );
	                     printf ("%s\n", cmdstr3 );
	                     fflush (stdout);
                        }
                        Tcl_Eval (tclorb->interp, cmdstr1 );
                        Tcl_Eval (tclorb->interp, cmdstr2 );
                        Tcl_Eval (tclorb->interp, cmdstr3 );
                } 
           }       
     }

     return 1;
        
}
