#include "uic_ui.h"


void usage()
{
      fprintf( stderr, "usage: %s [-d] [-c cmdport] [-i iport] [-o orbname] [-p pfile] [-s] [-v] \n", Program_Name );
      fprintf (stderr, "  DEFAULTS:\n");
      fprintf (stderr, "  \tiport    - /dev/ttyb.\n");
      fprintf (stderr, "  \tcmdport  - /dev/ttya.\n");
      fprintf (stderr, "  \torbname  - localhost.\n");
      banner (Program_Name, "$Revision$ $Date$");
      exit (1);

}


main(argc, argv)
	int             argc;
	char          *argv[];
{
        extern char    *optarg;
	extern int      optind;
	struct termios  termios;
	double 		crnt_time;
	char            *iport = DATA_PORT;
	char            *cmdport = CMND_PORT;
	char            *pfile = "b3s2orb";
	Display        *dpy;
	char            c;
	uchar_t		*buffer, packet_buffer[PLEN];
	uint_t		current_sec, last_sec[3], last_msec[3];
	int             flag, 
	                i, j,
			break_data;
	int 		data_port;
	int 		orberr = 0;
	char            sbuf[100];
	time_t		new_time, chk_time, old_time;

	elog_init (argc, argv) ;
        elog_notify (0, "$Revision$ $Date$") ;
        Program_Name = argv[0];


	/* init capture variables */

	Log = 0;
	orbfp = -1,
	orb = 0;
	capture_flag = 0;
	capture_count = 0;
	silent_flag = 0;
	break_data = 0;
	das_reset_count = 0;
	emla_set = cal_set = 0;
        orbname = "localhost";


	/* Set command line parameters default values  */
	   

	while ( ( i = getopt (argc, argv, "dc:i:o:p:vs")) != -1)
	            
	switch (i) {
	    case 'c':
		cmdport = optarg;
		break;

	    case 'd':
		capture_flag = 0;
		break ;
	    
	    case 'i':
		iport = optarg;             
		break;

	    case 'o':
		orb = 1;
		orbname = optarg;                  
		break;
	    
	    case 's':
		capture_flag = 1;
		silent_flag = 1;
		elog_complain( 0,
		  "!!! B3S2 will run in silent mode and start recording data !!!\n");
		break ;
	    
	    case 'p':
		pfile = optarg;         
		break;

	    case 'v':
		Log = 1;
		break ;
	    
	    default: 
           
		usage();
		
        }
			       

	if ( argc - optind != 0 )
             usage ();
		
	initpf(pfile);			          
                
	if( orb || silent_flag )  {
	    orb = 1;
	    if( ( orbfp = orbopen( orbname, "w" )) < 0)
                elog_die(0," Can't open ORB!\n");   
        }

	/* receive data port */
	data_port = open( iport, O_RDWR | O_NOCTTY);

	if (data_port == -1) {
		elog_die( 1, "FATAL ERROR: Cannot open data port... Exiting program\n");
	}

	/* make tty port RAW with no translation */

	i = ioctl(data_port, TCGETS, &termios);

	termios.c_iflag &= ~ICRNL;
	termios.c_iflag &= ~ISTRIP;
	termios.c_iflag &= ~IXOFF;
	termios.c_iflag &= ~IXON;
	termios.c_iflag &= ~IMAXBEL;
	termios.c_oflag &= ~OPOST;
	termios.c_lflag = IEXTEN;
	termios.c_cc[VMIN] = 0;	/* get one character */
	termios.c_cc[VTIME] = 0;/* Wait up to 0 secs */
	termios.c_cflag &= ~CBAUD;	/* set baud to 9600 */
	termios.c_cflag |= B4800;
	termios.c_cflag |= PARENB;	/* Odd parity */
	termios.c_cflag |= PARODD;
	termios.c_cflag |= CS8;	/* 8 data bits */

	for (i = 0; i < NCCS; i++)
		termios.c_cc[i] = 0;	/* clear special characters */
	i = ioctl(data_port, TCSETS, &termios);

	i = ioctl(data_port, TCGETS, &termios);


	/* init command output port */

	cmnd_port = open(cmdport, O_RDWR | O_NOCTTY);

	if (cmnd_port == -1) {
		elog_die(1, "FATAL ERROR: Cannot open command port... Exiting program\n");
	}

	/* make tty port RAW with no translation */

	i = ioctl(cmnd_port, TCGETS, &termios);

	termios.c_iflag &= ~ICRNL;
	termios.c_iflag &= ~ISTRIP;
	termios.c_iflag &= ~IXOFF;
	termios.c_iflag &= ~IXON;
	termios.c_iflag &= ~IMAXBEL;
	termios.c_oflag &= ~OPOST;
	termios.c_lflag = IEXTEN;
	termios.c_cc[VMIN] = 0;	/* get one character */
	termios.c_cc[VTIME] = 0;/* Wait up to 0 secs */
	termios.c_cflag &= ~CBAUD;	/* set baud to 9600 */
	termios.c_cflag |= B2400;
	termios.c_cflag |= PARENB;	/* No Odd parity */
	/* termios.c_cflag |= PARODD; */
	termios.c_cflag |= CS8;	/* 8 data bits */

	for (i = 0; i < NCCS; i++)
		termios.c_cc[i] = 0;	/* clear special characters */
	i = ioctl(cmnd_port, TCSETS, &termios);

	i = ioctl(cmnd_port, TCGETS, &termios);

	/*
	 * Set Signal Interrupts
	 */

	signal(SIGINT,  catchint);
	signal(SIGTERM, catchint);

	/*
	 * Initialize XView.
	 */

       if (!silent_flag) {

          xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
          INSTANCE = xv_unique_key();

          Uic_window1 = uic_window1_objects_initialize(NULL, NULL);
          dpy = (Display *) xv_get(Uic_window1->window1, XV_DISPLAY);

          XFlush(dpy);
          if (capture_flag) 
             xv_set(Uic_window1->record, PANEL_LABEL_STRING, ">>> RECORDING <<<", NULL);
       }


       time(&new_time);
       chk_time = old_time = new_time;
       allot( uchar_t *, buffer, PLEN );
       flag = -1;

       while (1) {
          time(&new_time);
          if ((new_time != chk_time) && !silent_flag) {
             chk_time = new_time;

             if (emla_set) {
                if (Uic_window1->emla_start != 
                    xv_get(Uic_window1->controls1, PANEL_CARET_ITEM)) {
                    emla_start_cmd();
                    emla_set = FALSE;
                }
             } else {
                if (Uic_window1->emla_start == 
                   xv_get(Uic_window1->controls1, PANEL_CARET_ITEM)) {
                   emla_set = TRUE;
 	        }
             }	

             if (cal_set) {
                if (Uic_window1->cal_start != 
                    xv_get(Uic_window1->controls1, PANEL_CARET_ITEM)) {
                    cal_start_cmd();
                    cal_set = FALSE;
                }
             } else {
               if (Uic_window1->cal_start == 
                   xv_get(Uic_window1->controls1, PANEL_CARET_ITEM)) {
                   cal_set = TRUE;
               }
            }
         }

        if (1 == read(data_port, &c, 1)) {
           switch (flag) {
              case -1:
                 if ((c == 'D'))
                   flag = 0;
                 else if ((c == 'S'))
                   flag = 1;
                 packet_buffer[0] = c;
                 break;
              case 0:
                 if (c == 'A') {
                     packet_buffer[1] = c;
                     flag = 2;
                 } else flag = -1;
                 break;
              case 1:
                 if (c == 'T') {
                    packet_buffer[1] = c;
                    flag = 2;
 	         } else flag = -1;
                 break;
              default: 

      /* now look to see if we have too many chars in packet */

                if ((flag > 80) || (flag < -1)) {
                   flag = -1;
                   break;
                }

/* store data, is packet full */

               packet_buffer[flag++] = c;
               if (flag == 80) {
/*
  printf("%c%c %02x %02x%02x%02x%02x%02x ", packet_buffer[0],
  packet_buffer[1],
  packet_buffer[5],
  packet_buffer[6],
  packet_buffer[7],
  packet_buffer[8],
  packet_buffer[12],
  packet_buffer[13]); 
  for (i=14;i<20;i++) 
     printf("%02x", packet_buffer[i]); printf("\n", packet_buffer);
*/ 

                    if ((packet_buffer[0] == 'S') && (packet_buffer[5] == 0)) {
		        if (capture_flag) {
		           if( orb )  {
			      memcpy( buffer, packet_buffer, PLEN );
		              if(!send2orb( orbfp, buffer )) {
                                  orberr++;
			          if( orberr > ORBERR ) 
			            elog_die( 0, "Too many errors! \n");
		              }  else orberr = 0;
                           }
			}

		        update_status_display(packet_buffer);
	            } else if ((packet_buffer[0] == 'D') && 
                                                
		          ( packet_buffer[5] == 0x01 || 
		            packet_buffer[5] == 0x02 || 
                            packet_buffer[5] == 0x03) ) {
					
		        update_display(&packet_buffer[0]);			
						
		        if (capture_flag) {
		             if( orb )  {
			        memcpy( buffer, &packet_buffer[0], PLEN );
		                if(!send2orb( orbfp, buffer )) {
                                    orberr++;
			            if( orberr > ORBERR ) 
			              elog_die( 0, "Too many errors! \n");
		                }  else orberr = 0;
		             }  else 
			     elog_complain( 0, "orbname is not specified. Restart 'b3s2orb -o orbname'\n");
		        }

 	                update_data_display(&packet_buffer[0]);
                    }  
	            flag = -1;			
               } /* end if length = 80 */
               break;

        } /* end switch(flag) */

     } /* if read */
		
     if (!silent_flag) {
	    notify_dispatch();
	    XFlush(dpy);
     }
  } /* while (1) */

	/*
	 * while (1) { notify_dispatch(); XFlush(dpy); printf("Here\n");
	 * xv_main_loop(Uic_window1->window1); }
	 */

/*	exit(0); */
}


/*
 * ==========================================================================
 * Notify callback function for `capture_cmnd'.
 * ==========================================================================
 */
void
capture_data(item, event)
	Panel_item      item;
	Event          *event;
{
	uic_window1_objects *ip = (uic_window1_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	/* fputs("uic: capture_data\n", stderr); */

	if (!capture_flag) {
		capture_flag = 1;
	        if( !orb || orbfp < 0 )  {
	            orb = 1;
	            if( ( orbfp = orbopen( orbname, "w" )) < 0)
                        elog_die(0," Can't open ORB!\n");   
                }
		xv_set(Uic_window1->record, PANEL_LABEL_STRING, ">>> RECORDING <<<", NULL);
	} else {
		capture_flag = 0;
	        if( orb || orbfp >= 0 )  {
	            if( orbclose( orbfp) )
                        elog_complain(1," Can't close ORB!\n");   
                }
		orb = 0;
		orbfp = -1;

		xv_set(Uic_window1->record, PANEL_LABEL_STRING, "", NULL);
	}
	/* xv_set(item, PANEL_BUSY, TRUE, NULL); */

}

/*
 * ==========================================================================
 * Notify callback function for `clear_cmnd'.
 * ==========================================================================
 */
void
clear_counters(item, event)
	Panel_item      item;
	Event          *event;
{
	send_das_cmd(cmnd_port, "RCRC");
}

/*
 * ==========================================================================
 * Notify callback function for `cmd_send'.
 * ==========================================================================
 */
void
cmd_send(item, event)
	Panel_item      item;
	Event          *event;
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];
	Panel           panel = (Panel) xv_get(item, PANEL_PARENT_PANEL);
	Xv_notice       notice;

	uic_window1_objects *ip = (uic_window1_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	/* fputs("uic: cmd_send\n", stderr); */

	if (item != Uic_window1->status_interval) {	/* do not prompt for
							 * statua interval
							 * update */
		notice = xv_create(panel, NOTICE,
				   XV_SHOW, TRUE,
		 NOTICE_MESSAGE_STRING, "Do You Really Want To Do This ???",
				   NOTICE_BUTTON_YES, "YES",
				   NOTICE_BUTTON_NO, "NO",
				   NOTICE_FOCUS_XY, 100, 100,
				   NOTICE_STATUS, &notice_status,
				   NULL);

		xv_destroy_safe(notice);
		if (!notice_status)
			return;
	}
	if (xv_get(Uic_window1->setting1, PANEL_VALUE) & 0x10)
		send_das_cmd(cmnd_port, "CL0001CL");
	else
		send_das_cmd(cmnd_port, "CL0000CL");

	if (xv_get(Uic_window1->setting1, PANEL_VALUE) & 0x08)
		send_das_cmd(cmnd_port, "CL0101CL");
	else
		send_das_cmd(cmnd_port, "CL0100CL");

	if (xv_get(Uic_window1->setting3, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0201CL");
	else
		send_das_cmd(cmnd_port, "CL0200CL");

/*	if (xv_get(Uic_window1->setting6, PANEL_VALUE) & 0x01)
		send_das_cmd(cmnd_port, "CL0301CL");
	else
		send_das_cmd(cmnd_port, "CL0300CL"); */

/*	if (1 == xv_get(Uic_window1->setting5, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0601CL");
	else
		send_das_cmd(cmnd_port, "CL0600CL");
*/
	i = (~xv_get(Uic_window1->setting1, PANEL_VALUE)) & 7;
	sprintf((char *)cbuf, "CL07%02.2dCL", i);
	send_das_cmd(cmnd_port, cbuf);

	i = xv_get(Uic_window1->status_interval, PANEL_VALUE);
	sprintf((char *)cbuf, "SI%04.4dSI", i);
	send_das_cmd(cmnd_port, cbuf);

/*	if (0 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0500CL");
	else if (1 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0501CL");
	else if (2 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0502CL");
	else if (3 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0503CL");
	else send_das_cmd(cmnd_port, "CL0500CL"); */
}

/*
 * ==========================================================================
 * Notify callback function for `cmd_send'.
 * ==========================================================================
 */
void
stat_int_send(item, event)
	Panel_item      item;
	Event          *event;
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];
	Panel           panel = (Panel) xv_get(item, PANEL_PARENT_PANEL);
	Xv_notice       notice;

	uic_window1_objects *ip = (uic_window1_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	i = xv_get(Uic_window1->status_interval, PANEL_VALUE);
	sprintf((char *)cbuf, "SI%04.4dSI", i);
	send_das_cmd(cmnd_port, cbuf);

}

/*
 * ==========================================================================
 * Notify callback function for `cmd_send'.
 * ==========================================================================
 */
void
mod_cmd_send(item, event)
	Panel_item      item;
	Event          *event;
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];
	Panel           panel = (Panel) xv_get(item, PANEL_PARENT_PANEL);
	Xv_notice       notice;

	uic_window1_objects *ip = (uic_window1_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	/* fputs("uic: cmd_send\n", stderr); */

	if (item != Uic_window1->status_interval) {	/* do not prompt for
							 * statua interval
							 * update */
		notice = xv_create(panel, NOTICE,
				   XV_SHOW, TRUE,
		 NOTICE_MESSAGE_STRING, "Do You Really Want To Do This ???",
				   NOTICE_BUTTON_YES, "YES",
				   NOTICE_BUTTON_NO, "NO",
				   NOTICE_FOCUS_XY, 100, 100,
				   NOTICE_STATUS, &notice_status,
				   NULL);

		xv_destroy_safe(notice);
		if (!notice_status)
			return;
	}

	if (1 == xv_get(Uic_window1->setting5, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0601CL");
	else
		send_das_cmd(cmnd_port, "CL0600CL");

}

/*
 * ==========================================================================
 * Notify callback function for `cmd_send'.
 * ==========================================================================
 */
void
cmd_cal_send(item, event)
	Panel_item      item;
	Event          *event;
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];
	Panel           panel = (Panel) xv_get(item, PANEL_PARENT_PANEL);
	Xv_notice       notice;

	uic_window1_objects *ip = (uic_window1_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	/* fputs("uic: cmd_send\n", stderr); */

	if (item != Uic_window1->status_interval) {	/* do not prompt for
							 * statua interval
							 * update */
		notice = xv_create(panel, NOTICE,
				   XV_SHOW, TRUE,
		 NOTICE_MESSAGE_STRING, "Do You Really Want To Do This ???",
				   NOTICE_BUTTON_YES, "YES",
				   NOTICE_BUTTON_NO, "NO",
				   NOTICE_FOCUS_XY, 100, 100,
				   NOTICE_STATUS, &notice_status,
				   NULL);

		xv_destroy_safe(notice);
		if (!notice_status)
			return;
	}

	if (0 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0500CL");
	else if (1 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0501CL");
	else if (2 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0502CL");
	else if (3 == xv_get(Uic_window1->setting4, PANEL_VALUE))
		send_das_cmd(cmnd_port, "CL0503CL");
	else send_das_cmd(cmnd_port, "CL0500CL");
}

/*
 * ==========================================================================
 * Notify callback function for `cmd_send'.
 * ==========================================================================
 */
void
cmd_mot_ic_send(item, event)
	Panel_item      item;
	Event          *event;
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];
	Panel           panel = (Panel) xv_get(item, PANEL_PARENT_PANEL);
	Xv_notice       notice;

	uic_window1_objects *ip = (uic_window1_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	if (item != Uic_window1->status_interval) {	/* do not prompt for
							 * statua interval
							 * update */
		notice = xv_create(panel, NOTICE,
				   XV_SHOW, TRUE,
		 NOTICE_MESSAGE_STRING, "Do You Really Want To Do This ???",
				   NOTICE_BUTTON_YES, "YES",
				   NOTICE_BUTTON_NO, "NO",
				   NOTICE_FOCUS_XY, 100, 100,
				   NOTICE_STATUS, &notice_status,
				   NULL);

		xv_destroy_safe(notice);
		if (!notice_status)
			return;
	}

	if (xv_get(Uic_window1->setting6, PANEL_VALUE) & 0x01)
		send_das_cmd(cmnd_port, "CL0301CL");
	else
		send_das_cmd(cmnd_port, "CL0300CL");

	if (xv_get(Uic_window1->setting6, PANEL_VALUE) & 0x02)
		send_das_cmd(cmnd_port, "CL0401CL");
	else
		send_das_cmd(cmnd_port, "CL0400CL");

}

/*
 * ==========================================================================
 * Notify callback function for `cmd_send'.
 * ==========================================================================
 */
void
cal_start_send(item, event)
	Panel_item      item;
	Event          *event;
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];
	Panel           panel = (Panel) xv_get(item, PANEL_PARENT_PANEL);
	Xv_notice       notice;

	uic_window1_objects *ip = (uic_window1_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	if (item != Uic_window1->status_interval) {	/* do not prompt for
							 * statua interval
							 * update */
		notice = xv_create(panel, NOTICE,
				   XV_SHOW, TRUE,
		 NOTICE_MESSAGE_STRING, "Do You Really Want To Do This ???",
				   NOTICE_BUTTON_YES, "YES",
				   NOTICE_BUTTON_NO, "NO",
				   NOTICE_FOCUS_XY, 100, 100,
				   NOTICE_STATUS, &notice_status,
				   NULL);

		xv_destroy_safe(notice);
		if (!notice_status)
			return;
	}

	sscanf((char *) xv_get(Uic_window1->cal_start, PANEL_VALUE), "%d%*c%d%*c%d", &i, &j, &k);
	sprintf((char *)cbuf, "ST%03d:%02d:%02d:00ST", i, j, k);
	send_das_cmd(cmnd_port, cbuf);

	xv_set(Uic_window1->cal_start, PANEL_VALUE, "", NULL);

}

emla_start_cmd()
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];

	sscanf((char *) xv_get(Uic_window1->emla_start, PANEL_VALUE), "%d%*c%d%*c%d", &i, &j, &k);
	sprintf((char *)cbuf, "ST%03d:%02d:%02d:00ST", i, j, k);
	send_das_cmd(cmnd_port, cbuf);

}

cal_start_cmd()
{
	int             i, j, k, notice_status;
	unsigned char   cbuf[40];

	sscanf((char *) xv_get(Uic_window1->cal_start, PANEL_VALUE), "%d%*c%d%*c%d", &i, &j, &k);
	sprintf((char *)cbuf, "SC%03d:%02d:%02d:00SC", i, j, k);
	send_das_cmd(cmnd_port, cbuf);

}

reconnect_bcu(item, event)
	Panel_item      item;
	Event          *event;
{
	char            cbuf[50], *p;

	strcpy(cbuf, "reconnect_BCU\r\n");

	p = &cbuf[0];

	while (*p != 0) {
		write(cmnd_port, p, 1);
		p++;
		mypause(1);
	}
}

/*
 * ==========================================================================
 * Write a string to the passed stream
 * ==========================================================================
 */
swrite(fd, string)
	int             fd;
	char           *string;
{
	return (write(fd, string, strlen(string)));
}


/*
 * ==========================================================================
 * Delay for n seconds
 * ==========================================================================
 */
mypause(delay)
	int             delay;
{
	long            ctime, ntime;

	time(&ctime);
	ntime = ctime + delay;
	while (ntime > ctime)
		time(&ctime);
}

/*
 * ==========================================================================
 * Send command string to DAS with proper CRLT termination
 * ==========================================================================
 */
send_das_cmd(fd, cmd)
	char           *cmd;
{
	char            buf[10];

	/* if (DEBUG > 5) printf("DAS_CMD:%s\n", cmd); */

	buf[0] = 0x80;
	buf[1] = 0;
	buf[2] = 0;
	write(fd, buf, 3);

	swrite(fd, cmd); printf("%s\n", cmd);
	swrite(fd, "\r\n");
	/* mypause(1); */
}

/*
 * ==========================================================================
 * Convert 3 byte data to 4 byte data
 * ==========================================================================
 */
cvt_byt_wrd(buffer)
	char           *buffer;
{

	int i;

	i = *buffer++;
	i = i << 8;
	i = i + *((unsigned char *) buffer++);
	i = i << 8;
	i = i + *((unsigned char *) buffer++);

	return (i);
}

/*
 * ==========================================================================
 * Convert 2 byte data to 4 byte data
 * ==========================================================================
 */
cvt_byt_int(buffer)
	unsigned char *buffer;
{
	unsigned int i;

	i = buffer[0];
	i = i << 8;
	i = i + buffer[1];

	return (i);
}

/*
 * ==========================================================================
 * Convert bcd packet time to tm struct time
 * ==========================================================================
 */
int cvt_bcd_time(buffer, stime)
	unsigned char  *buffer;
	struct tm      *stime;
{

	int i;

	i = (buffer[0] >> 4) & 0x0f;
	i = i * 10;
	i += buffer[0] & 0x0f;
	i = i * 10;
	i += (buffer[1] >> 4) & 0x0f;
	stime->tm_yday = i;

	i = buffer[1] & 0x0f;
	i = i * 10;
	i += (buffer[2] >> 4) & 0x0f;
	stime->tm_hour = i;

	i = buffer[2] & 0x0f;
	i = i * 10;
	i += (buffer[3] >> 4) & 0x0f;
	stime->tm_min = i;

	i = buffer[3] & 0x0f;
	i = i * 10;
	i += (buffer[4] >> 4) & 0x0f;
	stime->tm_sec = i;
	
	if ((stime->tm_yday > 366)  || (stime->tm_yday < 1)) return(FALSE);
	if ((stime->tm_hour > 23)  || (stime->tm_hour < 0)) return(FALSE);
	if ((stime->tm_min > 59)  || (stime->tm_min < 0)) return(FALSE);
	if ((stime->tm_sec > 59)  || (stime->tm_sec < 0)) return(FALSE);
	
	return(TRUE);
}

/*
 * ==========================================================================
 * Convert tm struct to seconds since beginning of year
 * ==========================================================================
 */
cvt_time(stime)
	struct tm      *stime;
{

	int             i;

	i = stime->tm_sec;
	i += stime->tm_min * 60;
	i += stime->tm_hour * 3600;
	i += (stime->tm_yday - 1) * 3600 * 24;

	return i;
}


/*
 * ==========================================================================
 * Update status packet fields on uic screen if not in silent mode.
 * ==========================================================================
 */
update_status_display(packet_buffer)
unsigned char *packet_buffer;
{
	int i;
	char sbuf[50];
		
	if (silent_flag) return;

	sprintf(sbuf, "%06d", cvt_byt_int(&packet_buffer[24]));
	xv_set(Uic_window1->das_miss1hz, PANEL_VALUE, sbuf, NULL);
	sprintf(sbuf, "%06d", cvt_byt_int(&packet_buffer[22]));
	xv_set(Uic_window1->das_tmstrerr, PANEL_VALUE, sbuf, NULL);
	sprintf(sbuf, "%06d", cvt_byt_int(&packet_buffer[26]));
	xv_set(Uic_window1->das_sec_tear, PANEL_VALUE, sbuf, NULL);
	sprintf(sbuf, "%06d", cvt_byt_int(&packet_buffer[28]));
	xv_set(Uic_window1->das_msec_tear, PANEL_VALUE, sbuf, NULL);
	sprintf(sbuf, "%06d", cvt_byt_int(&packet_buffer[30]));
	xv_set(Uic_window1->das_reset_count, PANEL_VALUE, sbuf, NULL);
/*	if ((packet_buffer[30] == 0xab) && (packet_buffer[31] == 0xcd))
		das_reset_count++;
	sprintf(sbuf, "%06d", das_reset_count); 
	xv_set(Uic_window1->das_reset_count, PANEL_VALUE, sbuf, NULL); */ /* Taken out 4-6-95 */
						
	/*
	 * if
	 * (xv_get(Uic_window1->settin
	 * g2, PANEL_VALUE))
	 */
						 
	xv_set(Uic_window1->status_interval,
		PANEL_VALUE,
		cvt_byt_int(&packet_buffer[34]),
		NULL);

	sprintf(sbuf, "%02x %6.2fC",
		(unsigned char) packet_buffer[41],
		(((float) ((unsigned char) (packet_buffer[41] ^ 0x80)) * 13.5) / 255.0) + 1.1);
	xv_set(Uic_window1->textfield4, PANEL_VALUE, sbuf, NULL);

	sprintf(sbuf, "%02x %6.2fC",
		(unsigned char) packet_buffer[42],
		(((float) ((unsigned char) (packet_buffer[42] ^ 0x80)) * 13.5) / 255.0) + 1.1);
	xv_set(Uic_window1->textfield5, PANEL_VALUE, sbuf, NULL);

	if ((Uic_window1->emla_start != xv_get(Uic_window1->controls1, PANEL_CARET_ITEM)) && !emla_set)
		{
		sprintf(sbuf, "%03d:%02d:%02d",
			(unsigned char) packet_buffer[36] * 256 + packet_buffer[37],
			(unsigned char) packet_buffer[47],
			(unsigned char) packet_buffer[49]);
		xv_set(Uic_window1->emla_start, PANEL_VALUE, sbuf, NULL);
		}

	if ((Uic_window1->cal_start != xv_get(Uic_window1->controls1, PANEL_CARET_ITEM)) && !cal_set)
		{
		sprintf(sbuf, "%03d:%02d:%02d",
			(unsigned char) packet_buffer[38] * 256 + packet_buffer[39],
			(unsigned char) packet_buffer[40],
			(unsigned char) packet_buffer[43]);
		xv_set(Uic_window1->cal_start, PANEL_VALUE, sbuf, NULL);
		}
}

/*
 * ==========================================================================
 * Update sonde status fields on uic screen if not in silent mode.
 * ==========================================================================
 */
update_display(packet_buffer)
unsigned char *packet_buffer;
{
	int i;
	char sbuf[100];
	
	if (silent_flag) return;

	sprintf(sbuf, "%02x%01x:%01x%01x:%01x%01x:%01x%01x",
		packet_buffer[14] & 255,
		packet_buffer[15] >> 4,
		packet_buffer[15] & 15,
		packet_buffer[16] >> 4,
		packet_buffer[16] & 15,
		packet_buffer[17] >> 4,
		packet_buffer[17] & 15,
		packet_buffer[18] >> 4);
	xv_set(Uic_window1->das_time, PANEL_VALUE, sbuf, NULL);
	
	sprintf(sbuf, "%6.3fV",
		(char)packet_buffer[9] * (float) 0.0783203125);
	xv_set(Uic_window1->textfield1, PANEL_VALUE, sbuf, NULL);
	sprintf(sbuf, "%6.3fV",
		(char)packet_buffer[10] * (float) 0.0783203125);

	xv_set(Uic_window1->textfield2, PANEL_VALUE, sbuf, NULL);
	sprintf(sbuf, "%6.3fV",
		(char)packet_buffer[11] * (float) 0.0783203125);
	xv_set(Uic_window1->textfield3, PANEL_VALUE, sbuf, NULL);

	i = 0;
	if (packet_buffer[6] & 0x01)
		i |= 0x10;
	if (packet_buffer[6] & 0x02)
		i |= 0x08;
	if (!(packet_buffer[6] & 0x04))
		i |= 0x04;
	if (packet_buffer[6] & 0x08)
		i |= 0x01;	/* mpist */
	if (packet_buffer[6] & 0x10)
		i |= 0x02;	/* flood */
	xv_set(Uic_window1->status, PANEL_VALUE, i, NULL);


	/*
	 * if
	 * (xv_get(Uic_window1->settin
	 * g2, PANEL_VALUE))
	 */

	if (packet_buffer[7] & 0x08)
		xv_set(Uic_window1->setting3, PANEL_VALUE, 1, NULL);
	else
		xv_set(Uic_window1->setting3, PANEL_VALUE, 0, NULL);


	if (packet_buffer[8] & 0x02)
		xv_set(Uic_window1->setting5, PANEL_VALUE, 1, NULL);
	else
		xv_set(Uic_window1->setting5, PANEL_VALUE, 2, NULL);

	i = 0;
	if (packet_buffer[7] & 0x80) i |= 1;
	if (packet_buffer[8] & 0x01) i |= 2;
	xv_set(Uic_window1->setting4, PANEL_VALUE, i, NULL);

	i = 0;
	if (packet_buffer[7] & 0x02)
		i |= 0x10;
	if (packet_buffer[7] & 0x04)
		i |= 0x08;
	if (!(packet_buffer[8] & 0x04))
		i |= 0x01;
	if (!(packet_buffer[8] & 0x08))
		i |= 0x02;
	if (!(packet_buffer[8] & 0x10))
		i |= 0x04;
	xv_set(Uic_window1->setting1, PANEL_VALUE, i, NULL);

	if (packet_buffer[8] & 0x20)
		xv_set(Uic_window1->calibrate, PANEL_LABEL_STRING, ">>> CALIBRATION ON <<<", NULL);
	else
		xv_set(Uic_window1->calibrate, PANEL_LABEL_STRING, "", NULL);


	i = 0;
	if (packet_buffer[7] & 0x20)
		i |= 0x01;
	if (packet_buffer[7] & 0x40)
		i |= 0x02;
	xv_set(Uic_window1->setting6, PANEL_VALUE, i, NULL);
}

/*
 * ==========================================================================
 * Update data fields on uic screen if not in silent mode.
 * ==========================================================================
 */
update_data_display(packet_buffer)
unsigned char *packet_buffer;
{
	int i;
	char sbuf[20];
	
	if (silent_flag) return;

	if (!capture_count) xv_set(Uic_window1->capture_cmnd, PANEL_BUSY, FALSE, NULL);

	i = cvt_byt_wrd(&packet_buffer[20]);
	sprintf(sbuf, "%08x", i);
	switch (packet_buffer[5]) {
		case 0x01:
			xv_set(Uic_window1->chan1, PANEL_VALUE, sbuf, NULL);
			break;
		case 0x02:
			xv_set(Uic_window1->chan2, PANEL_VALUE, sbuf, NULL);
			break;
		case 0x03:
			xv_set(Uic_window1->chan3, PANEL_VALUE, sbuf, NULL);
			break;
	}
}

/*
 * ==========================================================================
 * A signal termination will cause te process to close
 * all open data files and make a final wfdisc file.
 * ==========================================================================
 */
void catchint(signo)
int signo;
{
	printf("\n UIC Terminated by Signal %d\n", signo);

	fclose(outfile[0]);
	fclose(outfile[1]);
	fclose(outfile[2]);
	exit(0);
}
