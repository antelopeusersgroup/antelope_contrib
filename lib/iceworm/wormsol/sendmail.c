
                     /*******************************
                      *          sendmail           *
                      *                             *
                      *   Function to send email.   *
                      *******************************/

#include <stdio.h>
#include <malloc.h>
#include "earthworm.h"


#define DEFAULT_MAIL_PROG	"/usr/ucb/Mail"

/*
 * SendMail ()
 *   list of receipients
 *   number of emails to send
 *   mail program to use
 *   subject of the message
 *   message to send
 *   message prefix
 *   message suffix
 *   mail server to use
 *
 *
 * Modified by Lucky Vidmar Tue Jan 19 16:05:00 MST 1999
 *  to include the message and the configurable mail program
 *
 */


int SendMail( char person[][60], int nmail, char *mailProg, 
                      char *subject, char *msg, char *msgPrefix, 
                      char *msgSuffix, char *mailServer )
{
   FILE *f;
   char *cmnd;          /* Command line */
   char *msg_ptr;	
   int  i, max_char;

   /* Check incomming arguments */
   if (mailServer == NULL)
   {
      fprintf( stderr, "sendmail: Invalid arguments passed in\n" );
      return( -1 );
   }

   if (mailProg == NULL)
   {
      mailProg = (char *) calloc ((strlen (DEFAULT_MAIL_PROG) + 1), 
                          sizeof (char));
      strcpy (mailProg, DEFAULT_MAIL_PROG);
   }

   for ( i = 0; i < nmail; i++ )
   {
      /* Mail message without a subject */
      if (subject == NULL)
      {
         cmnd = (char *) calloc( 2 + strlen( mailProg ) + 
                                       strlen( person[i] ), 1);
         (void) sprintf( cmnd, "%s %s", mailProg, person[i] );
      }
      /* Mail message with the given subject */
      else
      {
         cmnd = (char *) calloc( 6 + strlen( mailProg ) + 
                        strlen( subject ) + strlen( person[i] ), 1 );

         (void) sprintf( cmnd, "%s -s \"%s\" %s", mailProg, subject, person[i] );
      }

      if ( ( f = popen( cmnd, "w" ) ) == NULL )
      {
         fprintf( stderr, "sendmail: Can't run command \"%s\"\n", cmnd );
         free( cmnd );
         return( -1 );
      }
      free( cmnd );


     /* Add the prefix if it was defined in the .d file */
     if (msgPrefix != NULL)
     {
          msg_ptr = msgPrefix;
          max_char = MAX_MSG_PREFIX_SIZE;
          while ( *msg_ptr && max_char--)
             putc( *msg_ptr++, f );
     }
         

      /*  Body of the message  */
      msg_ptr = msg;
      max_char = MAX_EMAIL_MSG_SIZE;
      while ( *msg_ptr && max_char--)
         putc( *msg_ptr++, f );


     /* Add the suffic if it was defined in the .d file */
     if (msgSuffix != NULL)
     {
          msg_ptr = msgSuffix;
          max_char = MAX_MSG_PREFIX_SIZE;
          while ( *msg_ptr && max_char--)
             putc( *msg_ptr++, f );
     }

      pclose( f );
   }
   return( 0 );
}

