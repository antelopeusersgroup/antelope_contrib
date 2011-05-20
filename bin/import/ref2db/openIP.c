/*************************************************************************
 *
 *  openIP.c
 *
 *  Open input port(s) - SCSI, serial, HSI, socket.                          
 *
 *
 *************************************************************************/
#include "ref2db.h"
#include <termio.h>

int 
open_IN_ports( RunArg *par )

{
 
	struct stat buf;

	if((stat(par->iport, &buf)) != 0)  {
  	   if(ENOENT)  {
                elog_complain(0, "open_IN_ports():Port:%s doesn't exist!\n", par->iport );
                return 0; 
           } else {
                elog_complain(1, "open_IN_ports():can't stat %s.", par->iport );
                return 0; 
           }
       }  else if( S_ISCHR(buf.st_mode) )  {
	  
	   if( !strncmp( par->iport , "/dev/rsd", strlen("/dev/rsd") ) )  
#ifdef sparc
               return open_disk( par );
#else
	       return 0;
#endif
   
               else return open_chr( par );

      }  else if( S_ISREG(buf.st_mode) || S_ISLNK(buf.st_mode) )  {
 
               return open_chr( par );
      }  else {
	       elog_complain(0, "open_IN_ports():%s can't be Input Port!", par->iport);
               return 0; 
      }
}

/* Input Port is Raw Disk.  */

#ifdef sparc

#include <sys/dkio.h>
#include <sys/dklabel.h>
#include <sys/vtoc.h>

int open_disk( RunArg *par )

{
 
	struct dk_geom geom;
	struct dk_allmap part;
	struct dk_label label;
	int val, nbytes;
	char buffer[1024];


	if (( par->ifp = open( par->iport, O_RDONLY ) ) < 0)  {
	  elog_complain(1,"open_IN_ports():Can't open %s disk.\n", par->iport );
	  return 0; 
        }
   
           /* get disk geometry  */

   	if( ioctl(par->ifp, DKIOCGGEOM, &geom) )  {
       	  elog_complain( 1, "openIP: Can't get disk geometry:" );
          return 0; 
        }

        /* get partition table of the disk */

	  if( ioctl( par->ifp, DKIOCGAPART, &part) )  { 
	     elog_complain( 1, " openIP: Can't get disk partition table:" );
             return 0; 
          }


        /* get disk label  */

        if( read( par->ifp, (char *) &label, sizeof(struct dk_label)) != 
                                   sizeof( struct dk_label))
      	read( par->ifp, (char *) &label, sizeof(struct dk_label));

  	if ( label.dkl_ncyl  != geom.dkg_ncyl  ||
       		label.dkl_nhead != geom.dkg_nhead ||
       		label.dkl_nsect != geom.dkg_nsect ) { 
                
	/* reset kerner table */

        	 geom.dkg_ncyl = label.dkl_ncyl;
        	 geom.dkg_pcyl = label.dkl_pcyl;
        	 geom.dkg_nhead = label.dkl_nhead;
        	 geom.dkg_nsect = label.dkl_nsect;
        	 geom.dkg_apc = label.dkl_apc;
        	 geom.dkg_rpm = label.dkl_rpm;
        	 geom.dkg_intrlv = label.dkl_intrlv;
        
		if (ioctl( par->ifp, DKIOCSGEOM, &geom) == -1) {
              	   elog_complain( 1, "openIP: Can't reset disk geometry");
         	   return 0; 
		}
 
	/* reset kernel's disk partition table */

        	if (ioctl( par->ifp, DKIOCSAPART, &label.dkl_map) == -1) {
                  elog_complain(  1, "openIP: Can't reset disk  partitoon" );
                  return 0; 
                }
  	}

  	close( par->ifp );

  	if (( par->ifp = open( par->iport, O_RDONLY ) ) < 0)  {
            elog_complain( 1,"open_IN_ports():Can't open %s disk.\n", par->iport);
            return 0; 
   	}
   
  	if( (nbytes = read( par->ifp, (char *) &buffer[0], 1024)) != 1024)  {
     	   elog_complain(1,"open_IN_ports():Can't read PASCAL label %s \n", buffer);
     	   return 0; 
  	}
    
  	if( (nbytes = read( par->ifp, (char *) &buffer[0], 1024)) != 1024)  {
     	   elog_complain(1,"open_IN_ports():Can't read PASCAL label %s \n", buffer);
     	   return 0; 
  	}

/* Find the MAX disk block index  */

  	memcpy( (char *) &val, (char *) &buffer[0], sizeof(int) );
  	val = ( val - 4 ) / 2;
  	PsclDk.maxblk = val;

/* Fine the Logical End of Disk  */
   
  	memcpy( (char *) &val, (char *) &buffer[32], sizeof(int) );
  	val = ( val - 4 ) / 2;
  	PsclDk.leod = val;

  	if( PsclDk.maxblk > PsclDk.leod ) PsclDk.maxblk = PsclDk.leod;
    
  
   	return IN_DISK;
}
#endif


/* Input Port is Character Special device.  */

int open_chr( RunArg *par )

{
  

	if (( par->ifp = open( par->iport, O_RDONLY ) ) < 0)  {
           elog_complain(1,"open_IN_ports():Can't open %s data stream.\n", par->iport);
           return 0; 
        }  else  return IN_CHR;

}

