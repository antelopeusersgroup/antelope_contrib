/************************************************************************
  *
  *  
  *
***********************************************************************/
#include "mrc.h"

extern char *new_name();

send_alarm(int mrcnum, int dasid, char *who )

{

  int 	nadd, i;
  char    alarm_str[512],
          key[8], mail_str[512], 
	  *madd, *das ;
   
  
  nadd = maxtbl(MailAdd);
  if( nadd <= 0 ) 
     elog_die( 0, "can't get a mail recipients addresses\n");
 
  sprintf( key, "%d\0", dasid);
  if( ( das = getarr( MailSent, key)) == 0 )  {
       das = (char *) new_name( key );
       setarr( MailSent, key, das );
				                 

       sprintf( alarm_str, 
           "WARNING %d MRC commands was sent to %d.\nFailed to do \'mass re-centering\'.\n", 
            mrcnum, dasid) ;
       for( i = 0; i < nadd; i++ )  {
           madd = (char *) gettbl( MailAdd, i );
           sprintf( mail_str, "echo \"%s\" | mailx -v -s \"%s:MRC FAILED\" %s\0", alarm_str, key, madd );

           system( mail_str );
       }
   } 

}

