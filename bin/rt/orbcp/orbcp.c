#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "orb.h"
#include "pkt.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

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
	int ready;
	int first=1;
	int pktstart=-1;
	int rcnt_time=600;

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
				fprintf (stderr, "orbcp: Need argument for -orbin.\n");
				usage();
				exit (1);
			}
			orbname = *argv;
		} else if (!strcmp(*argv, "-orbout")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -orout.\n");
				usage();
				exit (1);
			}
			orbnameo = *argv;
		} else if (!strcmp(*argv, "-src")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -src.\n");
				usage();
				exit (1);
			}
			srcexpr = *argv;
		} else if (!strcmp(*argv, "-ircnt")) {
			ircnt = 1;
		} else if (!strcmp(*argv, "-orcnt")) {
			orcnt = 1;
		} else if (!strcmp(*argv, "-pktstart")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -pktstart.\n");
				usage();
				exit (1);
			}
			pktstart = atoi(*argv);
		} else if (!strcmp(*argv, "-rcnt_time")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -rcnt_time.\n");
				usage();
				exit (1);
			}
			rcnt_time = 60*atoi(*argv);
		} else if (!strcmp(*argv, "-npackets")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -npackets.\n");
				usage();
				exit (1);
			}
			npackets = atoi(*argv);
		} else {
			fprintf (stderr, "orbcp: Illegal argument '%s'.\n", *argv);
			usage();
			exit (1);
		}
	}
	if (orbname == NULL) {
		fprintf (stderr, "orbcp: No input -in_orbname specified.\n");
		usage();
		exit (1);
	}
	if (orbnameo == NULL) {
		fprintf (stderr, "orbcp: No output -out_orbname specified.\n");
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
		clear_register (1);
		fprintf (stderr, "orbcp: orbopen() error for '%s'.\n", orbname);
		exit (1);
	}
	first = 1;

	if (srcexpr) {
		if (orbselect (orbin, srcexpr) < 0) {
			clear_register (1);
			fprintf (stderr, "orbcp: orbselect(%s, %s) erro.\n", orbname, srcexpr);
			exit (1);
		}
	}

	if (orcnt) {
		orbout = orbopen (orbnameo, "w&");
	} else {
		orbout = orbopen (orbnameo, "w");
	}
	if (orbout < 0) {
		clear_register (1);
		fprintf (stderr, "orbcp: orbopen() error for '%s'.\n", orbnameo);
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
		clear_register (1);
		fprintf (stderr, "orbcp: orbseek() error for '%s'.\n", orbname);
		printf ("orbcp: nothing in orb\n");
		exit (1);
	}

	n = 0;
	lastpkt_age = 0;
	while (1) {
		if (!first && fdkey(orbin) == 0) {	/* No pending input */
			sleep (1);
			lastpkt_age += 1;
			if (ircnt && lastpkt_age > rcnt_time && lastpkt_pktid >= 0) goto RECONNECT;
			continue;
		}
		ret = orbreap_nd (orbin, &pktid, src, &time, &packet, &nbytes, &bufsize);
		first = 0;
		switch (ret) {
		case 0:		/* Normal return */
			ready = 1;
			lastpkt_age = 0;
			lastpkt_pktid = pktid;
			lastpkt_time = time;
			break;
		case ORB_INCOMPLETE : 	/* Incomplete packet */
			ready = 0;
			lastpkt_age = 0;
			lastpkt_pktid = pktid;
			lastpkt_time = time;
			break;
		default:		/* Some other error */
			ready = -1;
			break;
		}
		if (ready == 0) continue;
		if (ready < 0) {
			if (ircnt) {
RECONNECT:			clear_register (0);
				sleep (10);
				lastpkt_age += 10;
				/* continue; */

				orbclose (orbin);
				while (1) {
					sleep (10);
					lastpkt_age += 10;
					orbin = orbopen (orbname, "r");
					if (orbin < 0) {
						clear_register (0);
						continue;
					}
					if (srcexpr) {
						sleep (20);
						if (orbselect (orbin, srcexpr) < 0) {
							orbclose (orbin);
							clear_register (0);
							continue;
						}
					}
					break;
				}
				first = 1;
				lastpkt_age = 0;
				if (orbseek (orbin, lastpkt_pktid) != lastpkt_pktid) {
					clear_register (0);
					orbseek (orbin, ORBNEWEST);
				} else {
					if (orbseek (orbin, ORBNEXT) < 0) {
						clear_register (0);
						orbseek (orbin, ORBNEWEST);
					}
				}
				continue;
			} else {
				clear_register (1);
				fprintf (stderr, "orbcp: orbreap_nd() error.\n");
				break;
			}
		}
		p = packet;
		if (orcnt) {
			while (orbput (orbout, src, time, p, nbytes) < 0) {
				clear_register (0);
				sleep (10);
			}
		} else {
			if (orbput (orbout, src, time, p, nbytes) < 0) {
				clear_register (1);
				fprintf (stderr, "orbcp: orbput() error.\n");
				break;
			}
		}
		n++;
		if (npackets > 0 && n >= npackets) break;
	}

}

usage()

{
	fprintf (stderr, "usage: orbcp -orbin in_orbname -orbout out_orbname [-src srcexpr]\n");
	fprintf (stderr, "             [-ircnt] [-orcnt] [-pktstart pktid]\n");
	fprintf (stderr, "             [-rcnt_time minutes] [-npackets npackets]\n");
}
