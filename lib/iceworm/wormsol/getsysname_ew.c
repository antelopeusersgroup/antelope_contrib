
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.2  2003/06/01 08:25:38  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.1  2000/02/14 18:46:17  lucky
 *     Initial revision
 *
 *
 */

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


