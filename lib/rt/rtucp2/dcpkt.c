#include <curses.h>
#include "diagp.h"

extern int StopBeep;
char bp=0x07;

extern Arr *DC_ID;

int BBA_Dc( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet, int nbytes )
{
    PktChannel *achan;
    char cmdstr[256], txtval[16]; 
    char tmp[32];
    char key[96];
    char errmsg[512], msg[512];
    DPars *dp;
    float fval;
    int i, alert, avr=0;
    int val = 0;
    char *s=0;
    double ptime, ytime, sec;
    TclOrb *tclorb = (TclOrb *) clientData;
 
    

        if(DC_ID == 0)  {
	    sprintf( errmsg, "Don't have DC parameters from Pf.");
            sprintf( &msg[0], "%s {%s} \0", ERR_PROC, errmsg);
            Tcl_Eval ( tclorb->interp, msg );
            while ( Tk_DoOneEvent (TK_DONT_WAIT) )
                 ;
            return 0;
        }
        switch (unstuffPkt (srcname, pkttime, packet, nbytes, &unstuff)) {
             case Pkt_wf:
                for (i = 0; i < unstuff->nchannels; i++) {
                   achan = (PktChannel *) gettbl (unstuff->channels, i);
	           sprintf (key, "%s_%s\0", achan->sta, achan->chan);
                   dp = ( DPars *) getarr (tclorb->dpars, key);
                   if ( dp != 0) {
                      val = achan->data[0]; 
/*
fprintf(stderr, "%s - %d\n", key, val);
fflush(stderr);
*/
                       if(strncmp(achan->chan, "CLOCK", 5) == 0 ||
	                  strncmp(achan->chan, "ACLK", 4) == 0 ||
	                  strncmp(achan->chan, "MCLK", 4) == 0 ||
	                  strncmp(achan->chan, "ACFAIL", 6) == 0 ||
	                  strncmp(achan->chan, "HAZARD", 6) == 0 ||
	                  strncmp(achan->chan, "MSTAT", 5) == 0 ||
	                  strncmp(achan->chan, "M1", 2) == 0 ||
	                  strncmp(achan->chan, "M2", 2) == 0 ||
	                  strncmp(achan->chan, "M3", 2) == 0 )  {
 
                             dp->crnt_val = val;
                             alert = 0; 
                             if (tclorb->verbose > 0) {
                                 printf ("%lf: %s\n", pkttime, key );
                                 fflush (stdout);
                             }
                             dp->ptime = pkttime;
                             if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {
             
                                 if( strncmp( achan->chan, "ACLK", 4 ) == 0  ||
			             strncmp( achan->chan, "MCLK", 4 ) == 0 )  {
                                     switch(val) {
                                        case 0:
                                             alert = 1;
                                             sprintf(&tmp[0], "OFF\0");
	                                     break;
                                        case 1:
                                             alert = 1;
                                             sprintf(&tmp[0], "TS FAIL\0");
	                                     break;
                                        case 2:
                                             alert = 1;
                                             sprintf(&tmp[0], "UNLOCKED\0");
	                                     break;
                                        case 3:
                                             sprintf(&tmp[0], "LOCKED +/- 1000 Us\0");
	                                     break;
                                        case 4:
                                             sprintf(&tmp[0], "LOCKED +/- 100 Us");
	                                     break;
                                        case 5:
                                             sprintf(&tmp[0], "LOCKED +/- 10 Us\0");
	                                     break;
                                        case 6:
                                             sprintf(&tmp[0], "LOCKED +/- 1 Us\0");
	                                     break;
                                        case 50:
                                             alert = 1;
                                             sprintf(&tmp[0], "OC FAIL\0");
	                                     break;
                                        case 100:
                                             alert = 1;
                                             sprintf(&tmp[0], "PPS FAIL\0");
	                                     break;
                                     }
               
                                }   else if( strncmp( achan->chan, "ACFAIL", 6 ) == 0 ) {
                                    if( val != 0 )  {
                                         alert = 1;
                                         sprintf(&tmp[0], "POWER_OFF\0");
                                         if( tclorb->alarmcall != 0 ) {
                                               
                                               if( !StopBeep ) { 
                                                  fprintf(stderr,"%c", bp);
                                                  fflush( stderr);
                                               }
                                               sprintf ( &cmdstr[0], "%s {%s} {ACFAIL} {%s} \0", 
                                                    tclorb->alarmcall, achan->sta, s=strtime(pkttime) );
                                               free(s);
                                               Tcl_Eval (tclorb->interp, cmdstr );
                                         }
                                    } else  {
                                         sprintf(&tmp[0], "POWER_ON\0");
                                         if( tclorb->alarmcall != 0 ) {
                                               sprintf ( &cmdstr[0], "%s {%s} {ACOK} {%s} \0", 
                                                    tclorb->alarmcall, achan->sta, s=strtime(pkttime) );
                                               free(s);
                                               Tcl_Eval (tclorb->interp, cmdstr );
                                         }
                                         alert = 0;
                                    }
                                }   else if( strncmp( achan->chan, "HAZARD", 6 ) == 0 ) {
                                    if( val != 0 )  {
                                         alert = 1;
                                         sprintf(&tmp[0], "PRESENT\0");
                                    } else {
                                         alert = 0;
                                         sprintf(&tmp[0], "ABSENT\0");
                                    }
             
                                }   else if(strncmp(achan->chan, "CLOCK", 3 ) == 0 ) {
                                  switch(val) {
                                     case 0:
                                        sprintf(&tmp[0], "UTC MAIN\0");
			                break;
                                     case 1:
                                        sprintf(&tmp[0], "UTC AUX\0");
			                break;
                                     case 2:
                                        sprintf(&tmp[0], "RT MAIN\0");
			                break;
                                     case 3:
                                        sprintf(&tmp[0], "RT AUX\0");
			                break;
		                  }
                                }   else if( strncmp( achan->chan, "M1", 2 ) == 0 ||
				             strncmp( achan->chan, "M2", 2 ) == 0 ||
				             strncmp( achan->chan, "M3", 2 ) == 0 ) {
                                    switch(val)  {
                                         case 0:
			                    alert = 1;
                                            sprintf(&tmp[0], "OFF\0");
 			                    break;
                                         case 1:
                                            sprintf(&tmp[0], "ON\0");
 			                    break;
                                         case 2:
			                    alert = 1;
                                            sprintf(&tmp[0], "OC FAIL\0");
 			                    break;
                                         case 3:
			                    alert = 1;
                                            sprintf(&tmp[0], "REBOOT\0");
 			                    break;
             
                                    }
    
                                }   
    
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
      
                     }  else {
                          alert = 0; 
                          sprintf( txtval, "%d\0", val);
                          if( strncmp(achan->chan, "BATT", 4) == 0 ) { 
	                      alert = check_dcbatt( key, val, &avr) ;
                              fval = (float) avr/100.0;
                              sprintf( txtval, "%.2f\0", fval);
                              if( alert )  {
                                  dp->alert = 1;
                                  if( tclorb->balarmcall != 0 ) {
                                           
                                     if( !StopBeep ) { 
                                         fprintf(stderr,"%c", bp);
                                         fflush( stderr);
                                     }
                                     sprintf ( &cmdstr[0], "%s {%s} {LOWBATT} {%s} {%s} \0", 
                                              tclorb->balarmcall, achan->sta,txtval,s=strtime(pkttime) );
                                     free(s);
                                     Tcl_Eval (tclorb->interp, cmdstr );
                                  }
                               } else  {
                                  if( dp->alert && tclorb->balarmcall != 0 ) {
                                      sprintf ( &cmdstr[0], "%s {%s} {BATTOK} {%s} {%s} \0", 
                                               tclorb->balarmcall, achan->sta, txtval, s=strtime(pkttime) );
                                       free(s);
                                       Tcl_Eval (tclorb->interp, cmdstr );
                                  }
                                  dp->alert = 0;
                               }
                               fval = (float) val/100.0;
                               sprintf( txtval, "%.2f\0", fval);
                           } 
                           if (tclorb->verbose > 0) {
                               printf ("%lf: %s %d\n", pkttime, key, val );
                               fflush (stdout);
                           } 
	                   dp->ptime = pkttime;
                           if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {
    
                               if( alert ) { 
                                   dp->alert = 1;
                                   sprintf ( &cmdstr[0], "%s {%s} {%s} {red} \0", 
                                             tclorb->parcallback, dp->widget, txtval );
                           }  else {
                               sprintf ( &cmdstr[0], "%s {%s} {%s} {none} \0", 
                                                tclorb->parcallback, dp->widget, txtval );
                               if( dp->alert ) { 
                                    sprintf ( &cmdstr[0], "%s {%s} {%s} {LemonChiffon}\0", 
                                                 tclorb->parcallback, dp->widget, txtval );
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
           break;
       default:
           break;
   
     } 
     return 1;
}

