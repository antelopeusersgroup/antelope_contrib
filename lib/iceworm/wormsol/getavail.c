
       /****************************************************
        *                   GetDiskAvail                   *
        *                                                  *
        *  DiskAvail = Available disk space in kilobytes.  *
        *  This function returns -1 if error;              *
        *                         0 if no error.           *
        *  SOLARIS 2.4 version.                            *
        ****************************************************/

#include <sys/types.h>
#include <sys/statvfs.h>

int GetDiskAvail( char *disk, unsigned *DiskAvail )
{
   struct statvfs buf;

   if ( statvfs( disk, &buf ) == -1 )
      return( -1 );

/* printf( "f_bsize:  %u\n", buf.f_bsize );       Block size
   printf( "f_blocks: %u\n", buf.f_blocks );      Total blocks
   printf( "f_bfree:  %u\n", buf.f_bfree );       Available to super user
   printf( "f_bavail: %u\n", buf.f_bavail );      Available to others */

   *DiskAvail = buf.f_bavail;
   return( 0 );
}
