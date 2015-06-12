#include <curses.h>
#include "diagp.h"

extern int StopBeep;
char bp=0x07;

#define CLC_NPAR   10 
 
static char *CLC_NAME[CLC_NPAR] =  {
 "ACFAIL",  "HAZARD", "M1OC", "M2OC", "M3OC", "CLOCK", "MSTAT", "ASTAT", "MOC", "AOC"
};
 
extern Arr *DP;

int BBA_Dc( ClientData clientData ,
                char *srcname,
                double pkttime,
                char *packet )
{
    char cmdstr[256], txtval[16]; 
    char key[96], sta[12], net[12];
    char *s, *staname;
    DPars *dp;
    ushort_t *sptr, sval;  
    float fval;
    int alert, avr;
    int i, ch, off;
    struct PreHdr *hdr;
    ulong lval;
    int val = 0;
    int numdas = 0;
    double ptime, ytime, sec;
    ulong ysec;
    int year, day, hour, min;
    TclOrb *tclorb = (TclOrb *) clientData;
 


        parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
        hdr = ( struct PreHdr *) packet;
       
        off = UID_DC+hdr->hdrsiz;
        memcpy( (char *) &sval, (char *) &packet[off], 2 );
        sprintf( (char *) &sta[0], "%d\0", sval );
             
   /* record LAST COMMAND  */
                 
        if( strlen(tclorb->lcmdwidget) > 0 && tclorb->lcmdcall != 0 )  {
 
            ytime = now();
            e2h( ytime, &year, &day, &hour, &min, &sec);
            ytime = epoch( year * 1000 );
            off = DCCMD_TIM +  hdr->hdrsiz;
            memcpy( (char *) &ysec, (char *) &packet[off], 4);
            ptime = ytime + ysec;

            if( ptime - tclorb->lcmd_time > 1 )  { 
                 tclorb->lcmd_time = ptime;
                 off = DCCMD +  hdr->hdrsiz;
                 for( i = 0; i < 40 ; i++)   {
                    sprintf( &cmdstr[i], "%c", packet[off+i]);
                 }
                 sprintf( key, "%s - %s : %-40s\0", sta, s=strtime(ptime), cmdstr );
                 free(s);
                 sprintf( cmdstr, "%s {%s} {%s}\0", tclorb->lcmdcall, tclorb->lcmdwidget, key );
             
                 Tcl_Eval (tclorb->interp, cmdstr );
            } 

        }
        for( i = 0; i < BBA_DCDC; i++ )  {
            alert = 0;    
            sprintf (key, "%s_%s\0", sta, BBA_DCDC_NAME[i] );
 
            dp = ( DPars *) getarr (tclorb->dpars, key);
            if ( dp != 0) {
 
                 off = hdr->hdrsiz + BBA_DCDC_OFF[i];
                 if( strncmp(BBA_DCDC_NAME[i], "TCPSND", 6) == 0 ) { 
	             alert = 0;
                     memcpy( (char *) &lval, packet + off, 4 );
                     val = lval;
                     sprintf( txtval, "%d\0", val);
                 } else if( strncmp(BBA_DCDC_NAME[i], "BATT", 4) == 0 ) { 
                     memcpy( (char *) &sval, packet + off, 2 );
	             alert = check_dcbatt( key, sval, &avr) ;
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
                                       tclorb->balarmcall, sta,txtval,s=strtime(pkttime) );
                              free(s);
                              Tcl_Eval (tclorb->interp, cmdstr );
                         }
                      } else  {
                         if( dp->alert && tclorb->balarmcall != 0 ) {
                              sprintf ( &cmdstr[0], "%s {%s} {BATTOK} {%s} {%s} \0", 
                                       tclorb->balarmcall, sta, txtval, s=strtime(pkttime) );
                               free(s);
                               Tcl_Eval (tclorb->interp, cmdstr );
                         }
                         dp->alert = 0;
                     }
                     fval = (float) sval/100.0;
                     sprintf( txtval, "%.2f\0", fval);
                 } else { 
	             alert = 0;
                     memcpy( (char *) &sval, packet + off, 2 );
                     val = sval;
                     sprintf( txtval, "%d\0", val);
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
       
      return 1;
}
int BBA_DcPar( ClientData clientData, 
                char *srcname, 
                double pkttime,
                char *data )
 
{
    struct PreHdr *hdr;
    DPars *dp;
    int i, clcsel,mon, aon;
    int aclc_val, mclc_val;
    ushort_t *sval;
    uchar_t mclc_stat, aclc_stat;
    int val, alert;
    char chan[12], net[12], sta[12], cmdstr[256], key[64];
    char tmp[64];
    char *s;
    uchar_t  *packet;
    TclOrb *tclorb = (TclOrb *) clientData;


    hdr = ( struct PreHdr *) data;
    packet = (uchar_t *) (data + hdr->hdrsiz + DCCLOCK );
    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
 
/*
hexdump( stderr, packet, 4 );
*/
 
 
    aclc_stat = packet[1];
    switch( aclc_stat )  {
        case ' ' :
           aclc_val = 1;
           break;
        case '.' :
           aclc_val = 10;
           break;
        case '*' :
           aclc_val = 100;
           break;
        case '#' :
           aclc_val = 1000;
           break;
        case '?' :
           aclc_val = -1;
           break;
        case '!' :
           aclc_val = 0;
           break;
        default:
           aclc_val = -999;
           break;       
    } 
    mclc_stat = packet[0];
    switch( mclc_stat )  {
        case ' ' :
           mclc_val = 1;
           break;
        case '.' :
           mclc_val = 10;
           break;
        case '*' :
           mclc_val = 100;
           break;
        case '#' :
           mclc_val = 1000;
           break;
        case '?' :
           mclc_val = -1;
           break;
        case '!' :
           mclc_val = 0;
           break;
        default:
           mclc_val = -999;
           break;       
    } 
    clcsel = (packet[3]&0x20) ? 1:0;
    mon = ( packet[3]&0x80) ? 1:0;     
    aon = (packet[3]&0x40) ? 1:0 ;       
    
    for( i=0; i < CLC_NPAR; i++ )  {
      sprintf (key, "%s_%s\0", sta, CLC_NAME[i] );
      dp = ( DPars *) getarr (tclorb->dpars, key);
            if ( dp != 0) {
                alert = 0; 
                if (tclorb->verbose > 0) {
                    printf ("%lf: %s\n", pkttime, key );
                    fflush (stdout);
                }
                dp->ptime = pkttime;
                if( strlen(dp->widget) > 0 && tclorb->parcallback != 0 )  {

                    if( strncmp( CLC_NAME[i], "MSTAT", 4 ) == 0 ) {
                        val = mclc_val;
                        if( val == 0 ) {
                                alert = 1;
                                sprintf(&tmp[0], "NO_CLOCK\0");
                        } else if( val == -1 )  {
                                alert = 1;
                                sprintf(&tmp[0], "UNLOCKED\0");
                        } else if( val == -999 )  {
                                alert = 1;
                                sprintf(&tmp[0], "UNKNOWN\0");
                        } else  {
                                alert = 0;
                                sprintf(&tmp[0], "LOCKED +/- %duS\0", val);
                        }
 
                    } else if( strncmp( CLC_NAME[i], "ASTAT", 4 ) == 0 ) {
                       val = aclc_val;
                       if( val == 0 ) {
                                alert = 1;
                                sprintf(&tmp[0], "NO_CLOCK\0");
                        } else if( val == -1 )  {
                                alert = 1;
                                sprintf(&tmp[0], "UNLOCKED\0");
                        } else if( val == -999 )  {
                                alert = 1;
                                sprintf(&tmp[0], "UNKNOWN\0");
                       } else  {
                                alert = 0;
                                sprintf(&tmp[0], "LOCKED +/- %duS\0", val);
                       }
  
                   }   else if( strncmp( CLC_NAME[i], "ACFAIL", 6 ) == 0 ) {
                       val = (packet[2]&0x80) ? 1:0;
                       if( val != 0 )  {
                            alert = 1;
                            sprintf(&tmp[0], "POWER_OFF\0");
                            if( tclorb->alarmcall != 0 ) {
                                  
                                  if( !StopBeep ) { 
                                     fprintf(stderr,"%c", bp);
                                     fflush( stderr);
                                  }
                                  sprintf ( &cmdstr[0], "%s {%s} {ACFAIL} {%s} \0", 
                                       tclorb->alarmcall, sta, s=strtime(pkttime) );
                                  free(s);
                                  Tcl_Eval (tclorb->interp, cmdstr );
                            }
                       } else  {
                            sprintf(&tmp[0], "POWER_ON\0");
                            if( tclorb->alarmcall != 0 ) {
                                  sprintf ( &cmdstr[0], "%s {%s} {ACOK} {%s} \0", 
                                       tclorb->alarmcall, sta, s=strtime(pkttime) );
                                  free(s);
                                  Tcl_Eval (tclorb->interp, cmdstr );
                            }
                            alert = 0;
                       }
                   }   else if( strncmp( CLC_NAME[i], "HAZARD", 6 ) == 0 ) {
                       val = (packet[2]&0x40) ? 1:0;
                       if( val != 0 )  {
                            alert = 1;
                            sprintf(&tmp[0], "PRESENT\0");
                       } else {
                            alert = 0;
                            sprintf(&tmp[0], "ABSENT\0");
                       }

                   }   else if( strncmp( CLC_NAME[i], "CLOCK", 3 ) == 0 ) {
                     val = clcsel;
                     if( val != 0 ) {
                         if( mon ) sprintf(&tmp[0], "MAIN ON \0");
                         else sprintf(&tmp[0], "MAIN OFF \0");
                     } else
                         if( aon ) sprintf(&tmp[0], "AUX ON \0");
                         else   sprintf(&tmp[0], "AUX OFF \0");

                   }   else if( strncmp( CLC_NAME[i], "M1OC", 4 ) == 0 ) {
                       val = (packet[2]&0x20) ? 1:0;
                       if( val != 0 )  {
                            alert = 0;
                            sprintf(&tmp[0], "NORMAL\0");
                       } else {
                            alert = 1;
                            sprintf(&tmp[0], "FAILURE\0");
                       }

                   }   else if( strncmp( CLC_NAME[i], "M2OC", 4 ) == 0 ) {
                     val = (packet[2]&0x10) ? 1:0;
                       if( val != 0 )  {
                            alert = 0;
                            sprintf(&tmp[0], "NORMAL\0");
                       } else {
                            alert = 1;
                            sprintf(&tmp[0], "FAILURE\0");
                       }

                   }   else if( strncmp( CLC_NAME[i], "M3OC", 4 ) == 0 ) {
                     val = (packet[2]&0x08) ? 1:0;
                       if( val != 0 )  {
                            alert = 0;
                            sprintf(&tmp[0], "NORMAL\0");
                       } else {
                            alert = 1;
                            sprintf(&tmp[0], "FAILURE\0");
                       }

                   }   else if( strncmp( CLC_NAME[i], "MOC", 3 ) == 0 ) {
                          val = (packet[2]&0x04) ? 1:0;
                       if( val != 0 )  {
                            alert = 0;
                            sprintf(&tmp[0], "NORMAL\0");
                       } else {
                            alert = 1;
                            sprintf(&tmp[0], "FAILURE\0");
                       }

                   }   else if( strncmp( CLC_NAME[i], "AOC", 3 ) == 0 ) {
                          val = (packet[2]&0x02) ? 1:0;
                       if( val != 0 )  {
                            alert = 0;
                            sprintf(&tmp[0], "NORMAL\0");
                       } else {
                            alert = 1;
                            sprintf(&tmp[0], "FAILURE\0");
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

         }
             
   
     }
     return 1;             
 
}

