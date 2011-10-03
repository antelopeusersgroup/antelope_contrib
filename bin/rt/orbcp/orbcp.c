#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "db.h"
#include "orb.h"
#include "coords.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

static int myputmsg (int orb, Dbptr db, char *prog, char *msg);

main (argc, argv)

int argc;
char **argv;

{
	char *orbname;
	char *orbnameo;
	char *srcexpr=NULL;
	int orbin=-1, orbout=-1;
	int pktid;
	int nbytes, bufsize = 0;
	int messages=0;
	Dbptr db;
	double time;
	char *packet=NULL;
	char *packet2=NULL;
	char *p;
	char src[100];
	char string[8];
	int npackets=0;
	int i, n;
	int ret;
	int hdrsiz, pktsiz, pkttype;
	char *ptr;
	int ircnt=0;
	int orcnt=0;
	int lastpkt_age;
	int lastpkt_pktid=-1;
	double lastpkt_time;
	double lastpkt_ltime;
	int ready;
	int first=1;
	int pktstart=-1;
	int rcnt_time=600;
	int found ;
	int flush=0;

	elog_init ( argc, argv ) ; 

	if (argc < 3) {
		usage();
		exit (1);
	}
	
	orbname = NULL;
	orbnameo = NULL;
	for (argc-=1,argv+=1; argc>0; argv++,argc--) {
		if (!strcmp(*argv, "-orbin")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "orbcp: Need argument for -orbin.\n");
				usage();
				exit (1);
			}
			orbname = *argv;
		} else if (!strcmp(*argv, "-orbout")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "orbcp: Need argument for -orout.\n");
				usage();
				exit (1);
			}
			orbnameo = *argv;
		} else if (!strcmp(*argv, "-src")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "orbcp: Need argument for -src.\n");
				usage();
				exit (1);
			}
			srcexpr = *argv;
		} else if (!strcmp(*argv, "-ircnt")) {
			ircnt = 1;
		} else if (!strcmp(*argv, "-orcnt")) {
			orcnt = 1;
		} else if (!strcmp(*argv, "-flush")) {
			flush = 1;
		} else if (!strcmp(*argv, "-messages")) {
			messages = 1;
			if (setupdb ("orbcp", &db) < 0) {
				elog_clear_register(1);
				elog_complain(0, "orbcp: setupdb() error.\n");
				exit (1);
			}
			db = dblookup (db, 0, "remark", 0, 0);
		} else if (!strcmp(*argv, "-pktstart")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "orbcp: Need argument for -pktstart.\n");
				usage();
				exit (1);
			}
			pktstart = atoi(*argv);
		} else if (!strcmp(*argv, "-rcnt_time")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "orbcp: Need argument for -rcnt_time.\n");
				usage();
				exit (1);
			}
			rcnt_time = 60*atoi(*argv);
		} else if (!strcmp(*argv, "-npackets")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "orbcp: Need argument for -npackets.\n");
				usage();
				exit (1);
			}
			npackets = atoi(*argv);
		} else {
			elog_complain(0, "orbcp: Illegal argument '%s'.\n", *argv);
			usage();
			exit (1);
		}
	}
	if (orbname == NULL) {
		elog_complain(0, "orbcp: No input -in_orbname specified.\n");
		usage();
		exit (1);
	}
	if (orbnameo == NULL) {
		elog_complain(0, "orbcp: No output -out_orbname specified.\n");
		usage();
		exit (1);
	}

	if (ircnt) {
		/* orbin = orbopen (orbname, "r&"); */
		orbin = orbopen (orbname, "r");
	} else {
		orbin = orbopen (orbname, "r");
	}
	if (orbin < 0) {
		elog_clear_register(1);
		elog_complain(0, "orbcp: orbopen() error for '%s'.\n", orbname);
		exit (1);
	}
	first = 1;

	if (srcexpr) {
		if (orbselect (orbin, srcexpr) < 0) {
			elog_clear_register(1);
			elog_complain(0, "orbcp: orbselect(%s, %s) erro.\n", orbname, srcexpr);
			exit (1);
		}
	}

	if (orcnt) {
		orbout = orbopen (orbnameo, "w&");
	} else {
		orbout = orbopen (orbnameo, "w");
	}
	if (orbout < 0) {
		elog_clear_register(1);
		elog_complain(0, "orbcp: orbopen() error for '%s'.\n", orbnameo);
		exit (1);
	}

	if (pktstart >= 0) {
		pktid = orbseek (orbin, pktstart);
		if (pktid < 0) {
			pktid = orbseek (orbin, ORBNEWEST);
		}
	} else {
		pktid = orbseek (orbin, ORBNEWEST);
	}
	if (pktid < 0) {
		elog_clear_register(1);
		elog_complain(0, "orbcp: orbseek() error for '%s'.\n", orbname);
		printf ("orbcp: nothing in orb\n");
		exit (1);
	}

	n = 0;
	lastpkt_age = 0;
	if (messages) myputmsg (orbout, db, "orbcp", "Start message logging");
	while (1) {
		if (!first) {
			ret = myfdkey(orbin, 1000);
			if (ret == 0) {	/* No pending input */
				lastpkt_age += 1;
				if (ircnt && lastpkt_age > rcnt_time && lastpkt_pktid >= 0) {
					if (messages) myputmsg (orbout, db, "orbcp", "age timeout");
					goto RECONNECT;
				}
				continue;
			} else if (ret < 0) {	/* error */
				if (messages) myputmsg (orbout, db, "orbcp", "fdkey() error");
				goto RECONNECT;
			}
		}
		ret = orbreap_nd (orbin, &pktid, src, &time, &packet, &nbytes, &bufsize);
		first = 0;
		switch (ret) {
		case 0:		/* Normal return */
			ready = 1;
			lastpkt_age = 0;
			lastpkt_pktid = pktid;
			lastpkt_time = time;
			lastpkt_ltime = now ();
			break;
		case ORB_INCOMPLETE : 	/* Incomplete packet */
			ready = 0;
			break;
		default:		/* Some other error */
			if (messages) myputmsg (orbout, db, "orbcp", "orbreap_nd() error");
			ready = -1;
			break;
		}
		if (ready == 0) continue;
		if (ready < 0) {
			if (ircnt) {
				int orbs;

RECONNECT:			elog_clear_register(0);
				if (messages) myputmsg (orbout, db, "orbcp", "reconnecting");
				sleep (10);
				lastpkt_age += 10;
				/* continue; */

				/* while (1) {
					int ok;

					sleep (10);
					lastpkt_age += 10;
					if (messages) myputmsg (orbout, db, "orbcp", "orb open for stat");
					orbs = orbopen (orbname, "r");
					if (orbs < 0) {
						elog_clear_register(0);
						if (messages) myputmsg (orbout, db, "orbcp", "orb open for stat failed");
						continue;
					}
					if (srcexpr) {
						if (messages) myputmsg (orbout, db, "orbcp", "orb select for stat");
						if (orbselect (orbs, srcexpr) < 0) {
							if (messages) myputmsg (orbout, db, "orbcp", "orb select for stat failed");
							orbclose (orbs);
							elog_clear_register(0);
							continue;
						}
					}
					while (1) {
						Orbstat *ostat;
						int istat;
						double slatest_time;

						sleep (10);
						lastpkt_age += 10;
						ostat = NULL;
						if (messages) myputmsg (orbout, db, "orbcp", "orb stat");
						if (orbstat ( orbs, &ostat ) < 0) {
							if (messages) myputmsg (orbout, db, "orbcp", "orb stat failed");
							orbclose (orbs);
							elog_clear_register(0);
							ok = 0;
							break;
						}
						ok = 0;
						slatest_time = 0.0;
						for (istat=0; istat<maxtbl(ostat->srcs); istat++) {
							OrbSrc *src;

							src = (OrbSrc *) gettbl (ostat->srcs, istat);
							if (src->slatest_time > slatest_time) slatest_time = src->slatest_time;
						}
						free_orbstat (ostat);
						if (slatest_time - lastpkt_time > (double)rcnt_time) {
							ok = 1;
							if (messages) myputmsg (orbout, db, "orbcp", "orb stat OK");
							break;
						}
					}
					if (ok) break;
				} 
				orbclose (orbs); */

				if (messages) myputmsg (orbout, db, "orbcp", "orb close");
				orbclose (orbin);
				while (1) {
					sleep (10);
					lastpkt_age += 10;
					if (messages) myputmsg (orbout, db, "orbcp", "orb open");
					orbin = orbopen (orbname, "r");
					if (orbin < 0) {
						if (messages) myputmsg (orbout, db, "orbcp", "orb open failed");
						elog_clear_register(0);
						continue;
					}
					if (srcexpr) {
						sleep (20);
						if (messages) myputmsg (orbout, db, "orbcp", "orb select");
						if (orbselect (orbin, srcexpr) < 0) {
							if (messages) myputmsg (orbout, db, "orbcp", "orb select failed");
							orbclose (orbin);
							elog_clear_register(0);
							continue;
						}
					}
					break;
				}
				first = 1;
				lastpkt_age = 0;
				if (messages) myputmsg (orbout, db, "orbcp", "orb seek to last_pktid");
				elog_complain( 0, "seeking to pktid #%d\n", lastpkt_pktid ) ; 
				if ((found = orbseek (orbin, lastpkt_pktid)) != lastpkt_pktid) {
					elog_complain( 0, "result of orbseek was %d\n", found ) ; 
					if (messages) myputmsg (orbout, db, "orbcp", "orb seek to last_pktid failed");
					elog_clear_register(0);
					orbseek (orbin, ORBNEWEST);
				} else {
					if (messages) myputmsg (orbout, db, "orbcp", "orb seek to next_pktid");
					if (orbseek (orbin, ORBNEXT) < 0) {
						if (messages) myputmsg (orbout, db, "orbcp", "orb seek to next_pktid failed");
						elog_clear_register(0);
						orbseek (orbin, ORBNEWEST);
					}
				}
				continue;
			} else {
				elog_clear_register(1);
				elog_complain(0, "orbcp: orbreap_nd() error.\n");
				break;
			}
		}
		p = packet;
		if (orcnt) {
			while (orbput (orbout, src, time, p, nbytes) < 0) {
				elog_clear_register(0);
				sleep (10);
			}
			if (flush) orbflush (orbout);
		} else {
			if (orbput (orbout, src, time, p, nbytes) < 0) {
				elog_clear_register(1);
				elog_complain(0, "orbcp: orbput() error.\n");
				break;
			}
			if (flush) orbflush (orbout);
		}
		n++;
		if (npackets > 0 && n >= npackets) break;
	}

}

usage()

{
	elog_complain(0, "usage: orbcp -orbin in_orbname -orbout out_orbname [-src srcexpr]\n");
	elog_complain(0, "             [-ircnt] [-orcnt] [-pktstart pktid]\n");
	elog_complain(0, "             [-rcnt_time minutes] [-npackets npackets]\n");
	elog_complain(0, "             [-messages] [-flush]\n");
}

int
setupdb (prog, db)

char *   prog;
Dbptr *        db;

{
	FILE *f;
	char string[512];

	sprintf (string, "/tmp/%s%d", prog, getpid());
	f = fopen(string, "w");
	if (f == NULL) {
               	elog_log(1, "setupdb: fopen('%s') error.\n", string);
               	return (-1);
	}
	if (fputs ("rt1.0\n", f) == EOF) {
               	elog_log(1, "setupdb: fputs('%s') error.\n", string);
               	return (-1);
	}
	if (fputs ("\n", f) == EOF) {
               	elog_log(1, "setupdb: fputs('%s') error.\n", string);
               	return (-1);
	}
	fclose (f);
	if (dbopen (string, "r+", db) == dbINVALID) {
               	elog_log(0, "setupdb: dbopen('%s') error.\n", string);
               	return (-1);
       	}
       	unlink (string);
	return (0);
}

static int
myputmsg (int orb, Dbptr db, char *prog, char *msg)

{
	char string[512];
	char name[512];

	db = dblookup (db, 0, "remark", 0, "dbSCRATCH");
	gethostname (name, 512);
	sprintf (string, "%s(%s:%d): %s %s", prog, name, getpid(), strtime(now()), msg);
	elog_complain(0, "%s\n", string);
	dbputv (db, 0, "remark", string, 0);
	db2orbpkt (db, orb);
        orbflush (orb);

}

#include <poll.h>

#ifndef POLLRDNORM
#define POLLRDNORM 0
#endif
 
#ifndef POLLRDBAND
#define POLLRDBAND 0  
#endif
 
int
myfdkey (fd, timeout)
int fd ;
int          timeout;
{
    struct      pollfd fds2poll[1] ;
    unsigned long nfds = 1 ;
    int retcode ;
 
    fds2poll[0].fd = fd ;
    fds2poll[0].events = POLLIN | POLLRDNORM | POLLPRI | POLLRDBAND | POLLERR ;
 
    switch ( poll(fds2poll, nfds, timeout) ) {
        case 1:
            if ( fds2poll[0].revents & POLLIN
             || fds2poll[0].revents & POLLRDNORM
             || fds2poll[0].revents & POLLRDBAND
             || fds2poll[0].revents & POLLPRI ) {
                retcode = 1 ;
             } else
                retcode = -1 ;
            break ;
 
        case 0:
            retcode = 0 ;
            break ;
 
        default:
            retcode = -1 ;
            break ;
    }
 
    return retcode;
}
 
