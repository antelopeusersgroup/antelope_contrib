/*************************************************************************
 *
 *  openIP.c
 *
 *  Open serial port 
 *
 *
 *************************************************************************/
#include "rddas.h"
#include <sys/vtoc.h>
#include <termio.h>
#include <sys/termios.h>

int 
open_IN_ports( struct Prts *inport)

{
 
	struct stat buf;

	if((stat(inport->ip_name, &buf)) != 0)  {
  	   if(ENOENT)  {
                complain( 1, "open_IN_ports():Port:%s doesn't exist!\n", inport->ip_name );
                return -1; 
           } else {
                complain( 1, "open_IN_ports():can't stat %s.", inport->ip_name );
                return -1; 
           }
        }  else if( S_ISCHR(buf.st_mode) )  {
	  
	   if( !strncmp( inport->ip_name , "/dev/cua", strlen("/dev/cua") ) )  
               return open_serial( inport );
   
        } 
 
       complain( 0, "open_IN_ports():%s can't be Input Port!", inport->ip_name);
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
		die( 1, "FATAL ERROR: Cannot open data port... Exiting program\n");
	}

	if( (i = ioctl( inport->ifp, TCGETS, &termios)) < 0 )  {
	   die( 1, "TCGETS error\n");
	}

	termios.c_iflag &= ~ICRNL;
	termios.c_iflag &= ~ISTRIP;
	termios.c_iflag &= ~IXOFF;
	termios.c_iflag &= ~IXON;
	termios.c_iflag &= ~IMAXBEL;
  	termios.c_oflag &= ~OPOST;
  	termios.c_oflag &= ~CSIZE;
	termios.c_lflag = IEXTEN;
/*
        termios.c_lflag &= ~( ICANON | ISIG | ECHO );
        termios.c_lflag &= NOFLSH;
*/

 	switch ( inport->brate )  {
	    
	    case 9600:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B9600;
		complain(  0,"baudrate set to 9600\n");
		break;
	    case 19200:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B19200;
		complain(  0,"baudrate set to 19200\n");
		break;
	    case 38400:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B38400;
		complain( 0,"baudrate set to 38400\n");
		break;
	    case 57600:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B57600;
	        termios.c_cflag &= ~CIBAUDEXT;
		complain(  0,"baudrate set to 57600\n");
		break;
	
            default:
	        termios.c_cflag &= ~CBAUD;	
		termios.c_cflag |= B19200;
		complain(  0,"baudrate set to 19200\n");
                break;
	}
	termios.c_cflag |= PARENB;
	termios.c_cflag |= PARODD;
	termios.c_cflag |= CLOCAL;
	termios.c_cflag |= CS8;	

	for (i = 0; i < NCCS; i++)
		termios.c_cc[i] = 0;	
	termios.c_cc[VMIN] = 1;
	termios.c_cc[VTIME] = 10;
	
	if( (i = ioctl( inport->ifp, TCSETS, &termios)) < 0 )  {
	   die( 1, "TCSETA error on %s port\n", inport->ip_name );
	}
        if( ioctl( inport->ifp, TCGETS, &termios) < 0 )
	   die( 1, "TCGETS error on %s port\n", inport->ip_name );
	
	if( (i = ioctl( inport->ifp, TCFLSH, TCIOFLUSH)) < 0 ) {
	   die( 1, "TCFLSH error on %s port\n", inport->ip_name );

	}
	if( (i = ioctl( inport->ifp, TCXONC, TCOON))  < 0 ) {
	   die( 1, "TCXONC/TCOON error on %s port\n", inport->ip_name );
	}
	if( (i = ioctl( inport->ifp, TCXONC, TCION)) < 0 ) {
	   die( 1, "TCXONC/TCION error on %s port\n", inport->ip_name );
	
	}

		                             
    return IN_CHR;       
}

 
