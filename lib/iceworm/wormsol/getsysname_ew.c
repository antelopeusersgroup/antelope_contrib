/*
 *  getsysname_ew.c  - Solaris version
 *
 *  Earthworm utility for getting the system name from the system
 * 
 */

#include <stdio.h>
#include <string.h>
#include <sys/utsname.h>

int getsysname_ew( char *sysname, int length )
{
   struct utsname name;

   if( uname( &name ) == -1 ) 
   {
      fprintf( stderr,
              "getsysname_ew: error on uname() call.\n" ); 
      return( -1 );
   }

   if( strlen( name.nodename ) >= (size_t) length ) 
   {
      fprintf( stderr,
              "getsysname_ew: system name too long for target address.\n");
      return( -2 );
   }

   strcpy( sysname, name.nodename );
   return( 0 );
}


