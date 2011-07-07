/*************************************************************************
 *
 *  openIP.c
 *
 *  Open serial port 
 *
 *
 *************************************************************************/
#include "rddas.h"
#include <termio.h>
#include <sys/termios.h>

int 
open_IN_ports( struct Prts *inport)

{
 
	struct stat buf;

	if((stat(inport->ip_name, &buf)) != 0)  {
  	   if(ENOENT)  {
                elog_complain( 1, "open_IN_ports():Port:%s doesn't exist!\n", inport->ip_name );
                return -1; 
           } else {
                elog_complain( 1, "open_IN_ports():can't stat %s.", inport->ip_name );
                return -1; 
           }
        }  else if( S_ISCHR(buf.st_mode) )  {
	  
	   if( !strncmp( inport->ip_name , "/dev/cua", strlen("/dev/cua") ) )  
               return open_serial( inport );
   
        } 
 
       elog_complain( 0, "open_IN_ports():%s can't be Input Port!", inport->ip_name);
       return -1; 
     
}


open_serial( inport )
struct Prts *inport;
{
	char            wchar, rchar;
	int             nbytes, i, j;
	char            buffer[1024];
        struct termios  termios;

	inport->ifp = open( inport->ip_name, O_RDWR | O_NOCTTY  );

	if ( inport->ifp == -1) {
		elog_die( 1, "FATAL ERROR: Cannot open data port... Exiting program\n");
	}

	if( (i = ioctl( inport->ifp, TCGETS, &termios)) < 0 )  {
	   elog_die( 1, "TCGETS error\n");
	}

	termios.c_iflag &= ~ICRNL;	/* map CR to NL on input  */
	termios.c_iflag &= ~ISTRIP;	/* set strip from 8 to 7 char */
	termios.c_iflag &= ~IXOFF;	/* enable start/stop input control  */
	termios.c_iflag &= ~IXON;	/* enable start/stop output control  */
	termios.c_iflag &= ~IMAXBEL;	/* if line too long - echo bell  */
  	termios.c_oflag &= ~OPOST;	/* postprocess output */
  	termios.c_cflag &= ~CSIZE;	/* character size  */
        termios.c_lflag = IEXTEN;
	termios.c_cflag |= CLOCAL;	/* local line not a dial-up  */

 	switch ( inport->brate )  {
	    
	    case 9600:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B9600;
		elog_complain(  0,"baudrate set to 9600\n");
		break;
	    case 19200:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B19200;
		elog_complain(  0,"baudrate set to 19200\n");
		break;
	    case 38400:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B38400;
		elog_complain( 0,"baudrate set to 38400\n");
		break;
	    case 57600:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B57600;
#if defined sun
	        termios.c_cflag &= ~CIBAUDEXT;
#elif defined __APPLE__

#else
	        termios.c_cflag &= ~CBAUDEX;
#endif
		elog_complain(  0,"baudrate set to 57600\n");
		break;
	
            default:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B19200;
		elog_complain(  0,"baudrate set to 19200\n");
                break;
	}
   	        

	switch( inport->parity )  {
	    case 'n':
                  
		break;

	    case 'e':

   	        termios.c_cflag &= ~PARODD;
   	        termios.c_cflag |= PARENB;
		break;

	    case 'o':

   	        termios.c_cflag |= PARODD|PARENB;
		break;
   	        
            default:
		
		termios.c_cflag &= ~PARENB;
		break;
        }

	switch( inport->stop_bits )  {
	
	    case 1:
		termios.c_cflag &= ~CSTOPB;
		break;

	    case 2:
		termios.c_cflag |= CSTOPB;
		break;
	  
	    default:
		termios.c_cflag &= ~CSTOPB;
		break;
	}


	switch( inport->data_bits )  {
	
	    case 8:
		termios.c_cflag |= CS8;
                break;

	    case 7:
		termios.c_cflag |= CS7;
                break;
	    default:
		termios.c_cflag |= CS8;
                break;

        }
	for (i = 0; i < NCCS; i++)
		termios.c_cc[i] = 0;	
	termios.c_cc[VMIN] = 1;
	termios.c_cc[VTIME] = 10;
	
	if( (i = ioctl( inport->ifp, TCSETS, &termios)) < 0 )  {
	   elog_die( 1, "TCSETA error on %s port\n", inport->ip_name );
	}
        if( ioctl( inport->ifp, TCGETS, &termios) < 0 )
	   elog_die( 1, "TCGETS error on %s port\n", inport->ip_name );
	
	if( (i = ioctl( inport->ifp, TCFLSH, TCIOFLUSH)) < 0 ) {
	   elog_die( 1, "TCFLSH error on %s port\n", inport->ip_name );

	}
	if( (i = ioctl( inport->ifp, TCXONC, TCOON))  < 0 ) {
	   elog_die( 1, "TCXONC/TCOON error on %s port\n", inport->ip_name );
	}
	if( (i = ioctl( inport->ifp, TCXONC, TCION)) < 0 ) {
	   elog_die( 1, "TCXONC/TCION error on %s port\n", inport->ip_name );
	
	}

		                             
    return IN_CHR;       
}

 
