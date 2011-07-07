/*********************************************************************
 *
 *  read_in_port.c 
 *
 *  Read data from input ports and pass packets to the ORB
 *
 *
 ********************************************************************/
#include "rddas.h"

int Log ;
extern struct Prm Par;


int read_packet ( struct Prts *ports, char **buffer)

{
        ushort_t val[2], code ;
	char chr, buf[DSK_BLK_SIZE];
 	int ntot, nbytes = 0; 
        int nread = DSK_BLK_SIZE;
	int first = 1,
	    done = 0;

	while( !done )  {
		    if( ( nbytes = read( ports->ifp, &chr, 1 ) ) > 0 )  {
		        if( first )  {

		           switch (chr)  {
		              case 'A':
		              case 'C':
		              case 'D':
		              case 'E':
		              case 'O':
		              case 'S':
		    	         first = 0;
			         buf[0] = chr;
			         break;
		              default:
			         break;
		         }
	               }  else {
		         buf[1] = chr;
		         memcpy( (char *) &code , &buf[0], 2);
		         switch ( code )  {
			     case PSCLAD:
			     case PSCLCD:
			     case PSCLDS:
			     case PSCLDT:
			     case PSCLEH:
			     case PSCLET:
			     case PSCLOM:
			     case PSCLSH:
			     case PSCLSC:
                                done = 1;
			        break;
			     default:
		                switch (chr)  {
		                    case 'A':
		                    case 'C':
		                    case 'D':
		                    case 'E':
		                    case 'O':
		                    case 'S':
		    	               first = 0;
			               buf[0] = chr;
			               break;
		                    default:
			               first = 1;
				       break;
		               }
			       break;
     
		         }
		     }
			
                 }
	}
	/* got a valid packet tag; read the rest of packet   */

        ntot = 2;
        nread -= 2;

        while( nread )  {
		
	       nbytes = read( ports->ifp, &buf[ntot], nread );
	       ntot += nbytes;
	       nread -= nbytes;
	
	}

	memcpy( *buffer, &buf[0], ntot );
/*	if( Log )
	   hexdump( stderr, &buf[0], 64 );  
*/
        return ntot;
}

int reset_das( struct Prts *ports)
{
	int 	port;
	char            att, cr,lf, c; 
	int             ntot, nsend, nbytes, i, j;
        char 		cmd[1024];


	/* open command port */
	
        if( open_IN_ports( ports ) < 0 ) { 
            elog_die( 0, "can't open %s command port\n", ports->ip_name );
	}
	/* build a command buffer - TA  */
        cmd[0] = 0x80;
	cmd[1] = 0;
	cmd[2] = 0;
	cr = 0x0D;
	lf = 0x0A;
        sprintf( &cmd[3], "TA00TA%c%c\0", cr, lf );
        nsend = 11;
	ntot = 0;
	while( nsend )  {
		nbytes = write( ports->ifp, &cmd[ntot], nsend );
	        nsend -= nbytes;
	        ntot += nbytes;
		nbytes = read( ports->ifp, &c, 1 );
	        if( c != 0x0 )  {
		  elog_die( 0, "Can't Stop Data Acquisition ( %0x ).\n", c);
		}
        }
	elog_complain( 0, "Stop Data Acquisition ... \n");

	/* 
	Restore Parameter Definition
        cmd[0] = 0x80;
	cmd[1] = 0;
	cmd[2] = 0;
	cr = 0x0D;
	lf = 0x0A;
        sprintf( &cmd[3], "RPRP%c%c\0", cr, lf );
        nsend = 9;
	ntot = 0;
	while( nsend )  {
           
		nbytes = write( ports->ifp, &cmd[ntot], nsend );
	        nsend -= nbytes;
	        ntot += nbytes;

        }
	*/
/*

        cmd[0] = 0x80;
        cmd[0] = 0x80;
	cmd[1] = 0;
	cmd[2] = 0;
	cr = 0x0D;
	lf = 0x0A;
        sprintf( &cmd[3], "RSRS%c%c\0", cr, lf );
	ntot = 0;
        nsend = 9;
	while( nsend )  {
           
		nbytes = write( ports->ifp, &cmd[ntot], 9 );
	        nsend -= nbytes;
	        ntot += nbytes;
		sleep(2);
		nbytes = read( ports->ifp, &c, 1 );
	        printf( "%0x - %c\n", c, c );

        }
*/


        cmd[0] = 0x80;
	cmd[1] = 0;
	cmd[2] = 0;
	cr = 0x0D;
	lf = 0x0A;
        sprintf( &cmd[3], "SA0005SA%c%c\0", cr, lf );
	ntot = 0;
        nsend = 13;
	while( nsend )  {
           
		nbytes = write( ports->ifp, &cmd[ntot], nsend );  
	        nsend -= nbytes;
	        ntot += nbytes;
	}

	
	elog_complain( 0, "Reset ... Start Data Acquisition. \n");
	close( ports->ifp);
	return 1;

}

void *read_in_ports( struct Prts *ports )

{
     
        unsigned char *buffer;   
 	double epoch;
 	char str[16], srcname[64];
	int pkttype;
	int i, j, num, cmp;
	int streamid, done, len;
	int in_err = -1,
 	    psize = 0 ; 
    
	allot ( unsigned char *, buffer, IBUF_SIZE );
	if( buffer == 0 )
	   elog_die( 1, " malloc error\n");

    	ports->orb = -1;
    	ports->ifp = -1; 
    
	/*  Open A Ring Buffer server  */
    
        if( ( ports->orb = orbopen( ports->orbname, "w" )) < 0)  {
         	elog_die(0,"ipd/read_in_port(): Can't open RB!\n");   
    	}

	/* Reset DASes  */

	if( ports->reset ) reset_das( ports );

    	if( open_IN_ports( ports ) < 0 ) 
	  elog_die( 0, "can't open %s input port\n", ports->ip_name );
    
        elog_complain( 0, "read %s write %s\n", ports->ip_name, ports->orbname);

       while(1) {
           if( !( len = read_packet( ports, &buffer )) )
               elog_die( 0, " can't read PASSCAL packet.\n");
           done = 0;

	   switch( pkttype = whatis_pkttype( buffer )) {
	        case -1:

                   elog_complain( 0, "Not valid PSCL packet\n");
	           hexdump( stderr, buffer, len);
	           in_err++;
		   if( in_err > 10 )  {
   	              orbclose( ports->orb );
	              if( close( ports->ifp ) != 0 )  
                         elog_complain( 0, "can't close %s.\n", ports->ip_name );
	              elog_die( 0, "Too many errors\n");
		   }
		break;
		    
		case 0:
		    break;

		default:

		    psize = hdr2packet( &buffer, Par.hdrtype, srcname );
                            
		    if( !psize )  {
	               elog_complain( 0, "Not a valid packet. Wrong Header?\n");
	               in_err++;
                    }  else  {  
                        in_err = 0;
                        if( Log )
                             elog_complain( 0, "SRCNAME: %s %lf %d \n", srcname, Par.time, psize );
         
		        if( orbput( ports->orb, srcname, Par.time, buffer, psize ) < 0 )  {
		           elog_complain( 0, " Can't send a packet to orbserver.\n");
		           exit(1);
		        }
	             }
		     break;
	   }
       }

}

