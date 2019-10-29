/*********************************************************************
 *
 *  read_in_port.c 
 *
 *  Read data from input ports add specified header; pass packet to ORB
 *
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ********************************************************************/
#include "ipd.h"

extern int NoSP;
extern int Psize; 
static double          prev_time = 0.0;


static int fdready( fp, timeout )
int fp;
int timeout;


 {

     struct pollfd fds[1] ;
     unsigned long nfds = 1 ;
     int retval ;  
	      
     fds[0].fd = fp ;
     fds[0].events = POLLIN | POLLRDNORM | POLLPRI | POLLRDBAND | POLLERR ;
		      
     switch ( poll( fds, nfds, timeout ) ) {
         case 1:
            if ( fds[0].revents & POLLIN || 
	         fds[0].revents & POLLRDNORM ||
		 fds[0].revents & POLLRDBAND || 
		 fds[0].revents & POLLPRI ) {
                 retval = 1 ;
            } else
                 retval = -1 ;
            break ;
         case 0:
	    retval = 0;
	    break;
	 default:
	    retval = -1;
	    break;
    }


    return retval;
 }

void *read_in_ports( ports, hdrtype, timeout )
struct Prts *ports;
int hdrtype;
int timeout;
{
     
        unsigned char *buffer;   
 	double epoch;
 	char srcname[64];
        int ptype, len;
	int in_err = 0,
	    out_err = 0,
 	    psize = 0 ; 
    
        Pblcks = 0;

        elog_complain( 0, "read %s write %s\n", ports->ip_name, ports->orbname);

	allot ( unsigned char *, buffer, IBUF_SIZE );

    	ports->orb = -1;
    	ports->ifp = -1; 
    
	/*  Open A Ring Buffer server  */
    
        if( ( ports->orb = orbopen( ports->orbname, "w" )) < 0)  {
         	elog_die(0,"ipd/read_in_port(): Can't open RB!\n");   
         	ports->orb = -1;
    	}
    	ptype =  open_IN_ports( ports );
    
        for( ;; )  {
	    switch( ptype  )  {
		    case IN_HSI:
	    	      if( !(len = read_hsi( ports, &buffer )) )
		         exit(0);
	    	    break;

		    case IN_CHR:
	    	      if( !( len = read_chr( ports, &buffer )) )
		         exit( 0 );
	    	    break;
	
		    case IN_DISK:
	    	      while( Pblcks < PsclDk.maxblk )
		       if( !( len =  read_disk( ports, &buffer )) )
		          exit( 0 );
		       Pblcks++;
		       
	    	    break;

		    case IN_SOCKET:
	    	      if( !read_socket( ports , hdrtype, timeout )) exit( 0 );
	    	    break;
        
		    default:
   		      orbclose( ports->orb );
	    	      elog_die( 0, "Can't open input port %s\n", ports->ip_name );

     	    }			    

            if( ( in_err = valid_pkt( &buffer, &srcname[0], &epoch,
                               &psize, len, in_err, hdrtype )) > 0 )  {
                 if(in_err >= 30)  {
                     in_err = 0;
                     if( close( ports->ifp ) != 0 )  {
   		          orbclose( ports->orb );
                          elog_die( 0, "can't close %s.\n", ports->ip_name );
    	             }
                     ptype =  open_IN_ports( ports );
                 } else continue;
            }  else  
                 out_err = send2orb( &ports->orb, ports->orbname, (char *)buffer, &srcname[0], 
	                                              epoch, psize, out_err);
        }
}

int read_hsi( ports, buffer )
struct Prts *ports;
unsigned char **buffer;

{


 	struct strbuf dataptr;
        int timeout = INFTIM; 
 	int strflag = 0;


        dataptr.maxlen = IBUF_SIZE;

	dataptr.buf = (char *) *buffer;       

/*  Set options for HSI card responce   */
 
  	if(ioctl( ports->ifp, I_SETSIG, S_MSG|S_ERROR|S_HANGUP) < 0)  {
       	    elog_complain( 1, "Can't set S_MSG|S_ERROR|S_HANGUP to SIGPOOL.\n");
  	}


	while ( 1 )  { 
            switch ( fdready( ports->ifp, timeout ) )  {
              case 0:
/*
                elog_complain( 0, "POLL TIME OUT on %s.Try to re-start...\n", ports->ip_name );
                if( close( ports->ifp ) != 0 )
                    elog_die( 0, "can't close %s.\n", ports->ip_name );
    	        if( !open_IN_ports( ports ) )
		    elog_die( 1, "can't reopen %s.\n", ports->ip_name);
 */
               break;
              case -1:
                elog_complain( 0, "POLL ERROR on %s.Try to re-start...\n", ports->ip_name );
                if( close( ports->ifp ) != 0 )  {
   		    orbclose( ports->orb );
                    elog_die( 0, "can't close %s.\n", ports->ip_name );
    	        }
                if( !open_IN_ports( ports ) )   {
   		    orbclose( ports->orb );
		    elog_die( 1, "can't reopen %s.\n", ports->ip_name);
                }
                break;
       
	      case 1:

                if( getmsg( ports->ifp,  NULL, &dataptr, &strflag) >= 0  && 
                    dataptr.len > 0 )  {
                    memcpy( *buffer, dataptr.buf, dataptr.len ); 
                }  
                return dataptr.len;
	     }
       }
	
}
 
int read_disk( ports, buffer )
struct Prts  *ports;
unsigned char **buffer;

{

 	int nbytes = 0;
 

        switch( (nbytes = read( ports->ifp, *buffer, DSK_BLK_SIZE))  )  {

    	     case 0:
   	        orbclose( ports->orb );
		elog_die( 1, "read EOF form %s.\n", ports->ip_name);
    	     case -1:
   	        orbclose( ports->orb );
		elog_die( 1, "read() error on %s.\n", ports->ip_name);
	     default:
		return nbytes;
	}

}

 
int read_chr( ports, buffer )
struct Prts *ports;
unsigned char **buffer;
{

 
 	int nbytes = 0; 
 
        switch( (nbytes = read( ports->ifp, *buffer, DSK_BLK_SIZE))  )  {

    	     case 0:
    	        elog_complain( 1, "read EOF on %s\n", ports->ip_name );
                if( close( ports->ifp ) != 0 )  {
   	            orbclose( ports->orb );
                    elog_die( 0, "can't close %s.\n", ports->ip_name );
    	        }
                if( !open_IN_ports( ports ) )  {
   	            orbclose( ports->orb );
		    elog_die( 1, "can't reopen %s.\n", ports->ip_name);
                }
    	     case -1:
		elog_complain( 1, "read() error on %s.\n", ports->ip_name);
                if( close( ports->ifp ) != 0 )  {
   	            orbclose( ports->orb );
                    elog_die( 0, "can't close %s.\n", ports->ip_name );
    	        }
                if( !open_IN_ports( ports ) )  {
   	            orbclose( ports->orb );
		    elog_die( 1, "can't reopen %s.\n", ports->ip_name);
                }
	     default:
		return nbytes;
	}

}

int
read_socket ( ports, hdrtype, timeout )
struct Prts *ports;
int hdrtype;
int timeout;

{

    double          epoch;
    ulong	    ysec;
    struct hostent *hp;
    int             connected = 0;
    int             bufcnt,
                    pid,
                    plength;
    unsigned short  hdrsiz,
                    pchecksum,
                    checksum,
		    chs_val,
                   *sp;
    uchar_t        *packet;
    char            srcname[64];
    int             psize;
    int             code, state,
                    len,
                    j,
                    i;
    int             unit,
                    iunit;
    int             unit_list[NUMDAS],
                    lpid[NUMDAS],
                    missed_cnt[NUMDAS],
                    check_cnt[NUMDAS];
    char           *s, *pname;
    char           *inet_ntoa ();
    unsigned char  *newbuffer,
                   *buffer;
    int 	   cansend, err = 0;
    int            off_pid,
                   off_plen,
		   off_uid;
    int 	   poll_err = 0;
    int 	   SP = 0;
    
    
    state = 0;
    pid = 0;

    allot (unsigned char *, buffer, 1024);
    allot (unsigned char *, newbuffer, Psize);
    allot (unsigned char *, packet, Psize);

    for (i = 0; i < NUMDAS; i++) {
	unit_list[i] = 0;
	lpid[i] = 0;
	missed_cnt[i] = 0;
	check_cnt[i] = 0;
    }

    timeout *= 1000;
    prev_time = std_now();

    for (;;) {
	switch  ( fdready( Ls, timeout )  ) {
   	   case -1:
	     elog_die(1, " socket error from poll\n");
	   case 0:
	     elog_die( 1, "poll timeout \n" );
	   case 1:
            poll_err = 0;
	    if( !connected )  {
		
		hp = gethostbyaddr ((char *) &peer_in.sin_addr, sizeof (struct in_addr),
				    peer_in.sin_family);
		if (hp == NULL)
		    pname = inet_ntoa (peer_in.sin_addr);
		else
		    pname = hp->h_name;    /* point to host's name */

		elog_notify (0, "Connected to : %s on port: %u\n", pname,
			   ntohs (peer_in.sin_port));
	        connected = 1;
	    }
	      len = recv ( Ls, (char *) buffer, 500, 0 );
	      if (len == 0) {
	  	   elog_die(0, "end of file on input socket\n");
	      } else {
		 for (i = 0; i < len; i++) {
		    switch (state) {

		    case 0:	       /* waiting for sync character */
			SP = 0;
			if (buffer[i] == 0xab || buffer[i] == 0xbb)  {
			    off_pid = OFF_PIDB;
			    off_uid = OFF_UIDB;
			    off_plen = OFF_PLENB;
			    state = 1;
			} else
			    elog_complain(0, "state = 0 : discarding character '%c' = %x\n", buffer[i], buffer[i]);
			
			memcpy ((char *) newbuffer, (char *) &buffer[i], 1);
			break;

		    case 1:	       /* waiting for packet type character */
			memcpy ((char *) newbuffer + 1, (char *) &buffer[i], 1);
			bufcnt = 0;
			switch (buffer[i]) {
			case 0xcd:
			    state = 2;
			    break;

			case 0xde:
			case 0xbc:
			    state = 3;
			    break;

			case 0xef:
			case 0xdc:
			    state = 4;
			    break;

			default:
			    state = 0;
			    elog_complain(0, "state = 1 : discarding character '%c' = %x\n", buffer[i], buffer[i]);
			    break;
			}
			break;

		    case 2:
			packet[bufcnt++] = buffer[i];
			if (bufcnt == 44) {
			    unit = (packet[off_uid] * 256) + packet[off_uid+1];
			    for (iunit = 0; iunit < NUMDAS; iunit++) {
				if (unit == unit_list[iunit])
				    break;
			    }
			    if (iunit == NUMDAS) {
				for (iunit = 0; iunit < NUMDAS; iunit++) {
				    if (unit_list[iunit] == 0) {
					unit_list[iunit] = unit;
					break;
				    }
				}
			    }
			    pid = (packet[off_pid] * 256) + packet[off_pid + 1];
			    /* if (lpid[iunit] != 0) {
				if ((pid - lpid[iunit]) != 1)  {
				    elog_complain( 0, "missed packet for %d: %d %d\n", unit, lpid[iunit], pid );
				    missed_cnt[iunit] += 1;
				}
			    }
			    */
			    lpid[iunit] = pid;
			    plength = (packet[off_plen] * 256) + packet[off_plen+1];
			    if (plength == 0) {
				elog_complain(0, "bad plength = 0 for packet type 0xcd : discarding packet");
				hexdump (stderr, packet, 44);
				state = 0;
			    } else
				state = 5;
			}
			break;

		    case 3:
			packet[bufcnt++] = buffer[i];
			if (bufcnt == 44) {
			    unit = (packet[off_uid] * 256) + packet[off_uid+1];
			    for (iunit = 0; iunit < NUMDAS; iunit++) {
				if (unit == unit_list[iunit])
				    break;
			    }
			    if (iunit == NUMDAS) {
				for (iunit = 0; iunit < NUMDAS; iunit++) {
				    if (unit_list[iunit] == 0) {
					unit_list[iunit] = unit;
					break;
				    }
				}
			    }
			    pid = (packet[off_pid] * 256) + packet[off_pid+1];
			    /*if (lpid[iunit] != 0) {
				if ((pid - lpid[iunit]) != 1)  {
				    missed_cnt[iunit] += 1;
				    elog_complain( 0, "missed packet for %d: %d %d\n", unit, lpid[iunit], pid );
				    missed_cnt[iunit] += 1;
				}
			    }  */
			    lpid[iunit] = pid;
			    plength = (packet[off_plen] * 256) + packet[off_plen+1];
			    if (plength == 0) {
				elog_complain(0, "bad plength = 0 for packet type 0xabde : discarding packet");
				hexdump (stderr, packet, 44);
				state = 0;
			    } else
				state = 5;
			}
			break;

		    case 4:
			packet[bufcnt++] = buffer[i];
			if (bufcnt == 44) {
			    pid = (packet[off_pid] * 256) + packet[off_pid+1];
			    plength = (packet[off_plen] * 256) + packet[off_plen+1];
			    if (plength == 0) {
				elog_complain(0, "bad plength = 0 for packet type 0xabef : discarding packet");
				hexdump (stderr, packet, 44);
				state = 0;
			    } else
				SP = 1;
				state = 5;
			}
			break;

		    case 5:	       /* packet complete -- send to
				        * orbserver */
			packet[bufcnt++] = buffer[i];
			if (bufcnt >= plength - 2) {
			    sp = (unsigned short *) &packet[0];
			    pchecksum = *sp++;
			    checksum = 0;
			    for (j = 0; j < ((plength / 2) - 2); j++) {
				checksum ^= *sp++;
			    }
			    pid = (packet[off_pid] * 256) + packet[off_pid+1];
			    checksum ^= 0xABCD;
			    memcpy ((char *) newbuffer + 2, (char *) &packet[0], plength - 2);
			    if (pchecksum - checksum != 0) {
				elog_complain(0, 
				    "discarding packet with bad checksum  PCHK:%04X!=CHK:%04X %3d %04d %05d - %d\n",
				    pchecksum, checksum, unit, plength, pid, len );
				hexdump (stderr, newbuffer, plength);
				check_cnt[iunit] += 1;
			    } else {
				if( SP && NoSP ) {
				    state = 0;
				    break;
				}
				if( ( err = valid_pkt (&newbuffer, &srcname[0], &epoch, &psize, plength, err, hdrtype )) > 0 ) {
				    elog_complain(0, "read_socket(): Not valid packet \n");
				    elog_complain(0, "read_socket():Wrong HEADER?\n");
				} else {
				        cansend = 1;
					if( fabs( epoch - prev_time) > 86400.0 )  {
					    prev_time = std_now();
					    if( fabs( epoch - prev_time) > 86400.0 )  {
						sp = ( ushort_t * ) &newbuffer[0];
						hdrsiz = *sp;
						memcpy( (char *) &ysec, newbuffer+hdrsiz+10, 4 );
						elog_complain(0, 
						    "%s packet has bad time - %s (epoch:%lf - ysec:%ld). Will discard packet.\n",
						    srcname, s=strtime(epoch), epoch, ysec );
						free(s);
						if( Log) hexdump( stderr, newbuffer+hdrsiz, 48 );
						cansend = 0;
					    } else prev_time = epoch;
					}  else  prev_time = epoch;

			            if( ports->orb > 0 && cansend ) {
				       if (orbput ( ports->orb, &srcname[0], epoch, (char *) newbuffer, psize) < 0) {
				 	    orbclose( ports->orb );
                                            elog_die(1, "Can't send a packet to orbserver.\n");
				        }
			             }
				}
			    }
			    state = 0;
			}
			if (bufcnt >= Psize) {
			    elog_complain(0, "attempted to accumulate %d byte packet: too large for internal buffer\n", bufcnt);
			    state = 0;
			}
			break;
		    }

		}
	    }
	break;
      }
    }

}
