
                     /*******************************
                      *          sendmail           *
                      *                             *
                      *   Function to send email.   *
                      *******************************/

#include <stdio.h>
#include <malloc.h>
#define MAIL "/usr/ucb/Mail"            /* Mail-sending program */


int SendMail( char person[][60], int nmail, char *msg, char *mailServer )
{
   FILE *f;
   char *cmnd;          /* Command line */
   char *msg_ptr;	
   int  i, max_char;

   for ( i = 0; i < nmail; i++ )
   {
      cmnd = (char *) calloc( 2 + strlen( MAIL ) + strlen( person[i] ), 1 );
      (void) sprintf( cmnd, "%s %s", MAIL, person[i] );

      if ( ( f = popen( cmnd, "w" ) ) == NULL )
      {
         fprintf( stderr, "sendmail: Can't run command \"%s\"\n", cmnd );
         free( cmnd );
         return( -1 );
      }
      free( cmnd );
      msg_ptr = msg;
      max_char = 32000;
      while ( *msg_ptr && max_char--)
         putc( *msg_ptr++, f );
      pclose( f );
   }
   return( 0 );
}

