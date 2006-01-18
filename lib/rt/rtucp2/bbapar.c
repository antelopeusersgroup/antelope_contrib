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
                char *packet, 
	 	int nbytes )
{
    char cmdstr1[256], 
         cmdstr2[256], 
         cmdstr3[256], 
	 key[128];
    PktChannel *achan;
    DPars *dp;
    int i, alert, val;
    float fval = 0.0;
    TclOrb *tclorb = (TclOrb *) clientData;
 

        switch (unstuffPkt (srcname, pkttime, packet, nbytes, &unstuff)) {
             case Pkt_wf:
                for (i = 0; i < unstuff->nchannels; i++) {
                achan = (PktChannel *) gettbl (unstuff->channels, i);
                sprintf (key, "%s_%s\0", achan->sta, achan->chan );
                alert = 0;
                dp = ( DPars *) getarr (tclorb->dpars, key);
                if ( dp != 0) {
                    val = achan->data[0];
		    dp->crnt_val = val;
                    if( strncmp(achan->chan, "LLOCK", 5) == 0 ) { 
                           alert = check_llock( dp, val );
                    }  else  if( strncmp(achan->chan, "BATT", 4) == 0 )  {
                           fval = (float) val / 100.0;
                           dp->crnt_val = fval;
                           alert = check_batt( dp, fval );
                    }  else  if( strncmp(achan->chan, "TEMP", 4) == 0 )  {
                           fval = (float) val / 100.0;
                           dp->crnt_val = fval;
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
        break;
        
      default:
		
	    break;
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
      
/*
fprintf( stderr, "%d + %d -  %d \n", acmp->avr, sval, old_batt ); 
*/
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

int BBA_DcDas( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet, int nbytes )
{
    char cmdstr1[256], 
         cmdstr2[256], 
         cmdstr3[256];
    char key[64];
    PktChannel *achan;
    char *staname;
    DPars *dp;
    ulong_t *iptr;
    ushort_t *sptr, sval;  
    uchar_t *daspkt; 
    int alert;
    int i, ch, off;
    struct PreHdr *hdr;
    int val = 0;
    int numdas = 0;
    TclOrb *tclorb = (TclOrb *) clientData;
 


     switch (unstuffPkt (srcname, pkttime, packet, nbytes, &unstuff)) {
         case Pkt_wf:
             for (i = 0; i < unstuff->nchannels; i++) {
                achan = (PktChannel *) gettbl (unstuff->channels, i);
                sprintf (key, "%s_%s\0", achan->sta, achan->chan );
                dp = ( DPars *) getarr (tclorb->dpars, key);
                if ( dp != 0) {
                    val = achan->data[0];
                    dp->ptime = pkttime;
                    dp->crnt_val = val;
                    dp->min_val = ( dp->crnt_val < dp->min_val )?dp->crnt_val:dp->min_val;
                    dp->max_val = ( dp->crnt_val > dp->max_val )?dp->crnt_val:dp->max_val;
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
            break;

       default:
            break;
    }      

return 1;
        
}
