
/***************************************************************
 *                            pipe.c                           *
 *                                                             *
 *      Routines for writing to and reading from a pipe        *
 *      under Solaris.                                         *
 ***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static FILE  *Pipe;  /* "write-handle" to pipe to "child" process */


/*****************************************************************/
/* pipe_init() starts a process and opens a pipe to it.  The  	 */
/*             pipe replaces stdin of the new process.		 */
/*    Returns:   0 on success					 */ 
/*		-1 on failure 					 */
/*****************************************************************/
int pipe_init( char  *nextproc,  /* command to start new process */
      unsigned long   pipesize ) /* how big (bytes) to make pipe */ 
				 /* NOT USED under Solaris       */
{
   Pipe = popen( nextproc, "w" );
   if ( Pipe == (FILE *) NULL ) return (-1);
   return ( 0 );
} 


/*****************************************************************/
/* pipe_put() writes a msg to a pipe, terminating with null byte */
/* Returns 0 if there were no errors, 				 */
/*	  some number if there were errors			 */
/*****************************************************************/
#define MAXWRITE 500

int pipe_put( char *msg, 	/* null-terminated char string 	*/
	      int   msgtype )	/* type of message (0-255)	*/      
{
   char     str[4];
   char     *m;
   unsigned n;
   unsigned nwrite;
   unsigned nwritten;    /* Number of chars written to pipe    */

/* Write message type to pipe
   **************************/
   if ( (msgtype > 255) || (msgtype < 0) )
   {
      fprintf( stderr, "msgtype out of range.  msgtype: %d\n", msgtype );
      return( -1 );
   }
   sprintf( str, "%3d", msgtype );
   nwritten = fwrite( str, sizeof(char), (size_t)3, Pipe );
/* printf("pipe_put:  type:%3d.   msg:\n%s\n", msgtype, msg ); */
   if ( nwritten < 3 )
   {
      fprintf( stderr, "Write error. nwritten: %d  Should be 3.\n", nwritten );
      return( -1 );
   }

/* Write message string to pipe, including the null character
   **********************************************************/
   m        = msg;
   nwrite   = strlen( msg ) + 1;
   nwritten = 0;

   while ( nwrite > 0 )
   {
      n = fwrite( m, sizeof(char), ( nwrite > MAXWRITE ) ?
                  (size_t)MAXWRITE : (size_t)nwrite, Pipe );
      if ( n == 0 )
      {
         fprintf( stderr, "pipe_put(): Write error!\n" );
         return( -1 );
      }
      nwritten += n;
      nwrite -= n;
      m += n;
      fflush( Pipe );
   }

   if ( nwritten != strlen( msg ) + 1 )
   {
      fprintf( stderr, "Pipe write error. nwritten != strlen(msg)+1\n" );
      return( -1 );
   }

   return( 0 );
}

/*****************************************************************
 * pipe_get() reads a msg from a pipe (stdin) and writes it as a *
 *            null-terminated char string to the given address	 *
 * Returns  # of chars written to msg (not including null-byte)	 * 
 *         -1 if the message was longer than maxlen              *
 *         -2 if EOF encountered reading message type            *
 *****************************************************************/

int pipe_get( char *msg,	/* address to copy msg to 	*/
	      int   maxlen, 	/* size of msg buffer		*/
	      int  *type )	/* type of message returned 	*/
{
   char  typestr[4];
   char *m;
   char  ch;
   int   i;

/* Start by reading message type from pipe
 *****************************************/
   m = typestr;     /* Use a working copy of the target address */

   for ( i = 0; i < 3; i++ )
   {
      *m = fgetc( stdin );
      if ( *m == '\0' )  
      {
         *m = '\0';
     	 return ( -2 );
      }
      if ( *m == (char)EOF )  
      {
         *m = '\0';
     	 return ( -3 );
      }
      m++;
   }
   *type = atoi(typestr);

/* Now read the message (terminated by null byte)
 ************************************************/
   m = msg;     /* Use a working copy of the target address */

   for ( i = 0; i < maxlen; i++ )
   {
      *m = fgetc( stdin );
      if ( *m == '\0' || *m == (char)EOF )  
      {
         *m = '\0';
     	 return ( i );
      }
      m++;
   }
	
/* If you got here, the message was too long!  Skip to the end of it
 *******************************************************************/
   while( 1 )
   {
      ch = fgetc( stdin );
      if ( ch == '\0' || ch == (char)EOF )
         break;
   }
   return( -1 );
}


/*****************************************************************/
/* pipe_close()  Closes the pipe			  	 */
/*****************************************************************/
void pipe_close( void )   
{
   pclose( Pipe );
} 
