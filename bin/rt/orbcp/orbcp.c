#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "orb.h"
#include "pkt.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

double mynow();

main (argc, argv)

int argc;
char **argv;

{
	char *orbname;
	char *orbnameo;
	char *filein;
	char *fileout;
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
	FILE *fin=NULL;
	FILE *fout=NULL;
	int reverse=0;
	int npackets=0;
	int i, n;
	int ret;
	int repeat=0;
	double timeoff=0.0;
	double firsttime=0.0;
	double deltime=0.0;
	double sim_latency=-200.0;
	double sim_offset=0.0;
	double lasttime;
	int hdrsiz, pktsiz, pkttype;
	char *ptr;
	int ircnt=0;
	int orcnt=0;
	int lastpkt_age;
	int lastpkt_pktid;
	double lastpkt_time;
	int ready;
	int first=1;

	elog_init ( argc, argv ) ; 

	if (argc < 3) {
		usage();
		exit (1);
	}
	
	orbname = NULL;
	orbnameo = NULL;
	filein = NULL;
	fileout = NULL;
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
		} else if (!strcmp(*argv, "-filein")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -filein.\n");
				usage();
				exit (1);
			}
			filein = *argv;
		} else if (!strcmp(*argv, "-fileout")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -fileout.\n");
				usage();
				exit (1);
			}
			fileout = *argv;
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
		} else if (!strcmp(*argv, "-reverse")) {
			reverse = 1;
		} else if (!strcmp(*argv, "-repeat")) {
			repeat = 1;
		} else if (!strcmp(*argv, "-npackets")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -npackets.\n");
				usage();
				exit (1);
			}
			npackets = atoi(*argv);
		} else if (!strcmp(*argv, "-sim_latency")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "orbcp: Need argument for -sim_latency.\n");
				usage();
				exit (1);
			}
			sim_latency = atof(*argv);
		} else {
			fprintf (stderr, "orbcp: Illegal argument '%s'.\n", *argv);
			usage();
			exit (1);
		}
	}
	if (filein == NULL && orbname == NULL) {
		fprintf (stderr, "orbcp: No input (-orbin or -filein) specified.\n");
		usage();
		exit (1);
	}
	if (filein != NULL && orbname != NULL) {
		fprintf (stderr, "orbcp: Cannot specify two inputs (both -orbin and -filein).\n");
		usage();
		exit (1);
	}

	if (orbname) {
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
	} else {
		fin = fopen (filein, "r");
		if (fin == NULL) {
			perror ("orbcp");
			fprintf (stderr, "orbcp: open(%s) error.\n", filein);
			exit (1);
		}
		if (fread (string, 4, 1, fin) == 0) {
			perror ("orbcp");
			fprintf (stderr, "orbcp: fread(%s) error.\n", filein);
			exit (1);
		}
		if (strncmp (string, "ocp1", 4)) {
			fprintf (stderr, "orbcp: wrong input file %s.\n", filein);
			exit (1);
		}
	}

	if (orbnameo) {
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
	}
	if (fileout) {
		fout = fopen (fileout, "w");
		if (fout == NULL) {
			perror ("orbcp");
			fprintf (stderr, "orbcp: open(%s) error.\n", fileout);
			exit (1);
		}
		if (fwrite ("ocp1", 4, 1, fout) == 0) {
			perror ("orbcp");
			fprintf (stderr, "orbcp: fwrite(%s) error.\n", fileout);
			exit (1);
		}
	}

	if (orbin >= 0) {
		pktid = orbseek (orbin, ORBNEWEST);
		if (pktid < 0) {
			clear_register (1);
			fprintf (stderr, "orbcp: orbseek() error for '%s'.\n", orbname);
			printf ("orbcp: nothing in orb\n");
			exit (1);
		}
	}

	n = 0;
	lastpkt_age = 0;
	while (1) {
		if (orbin >= 0) {
			if (!first && fdkey(orbin) == 0) {	/* No pending input */
				sleep (1);
				lastpkt_age += 1;
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
					clear_register (0);
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
					if (orbseek (orbin, lastpkt_pktid) != lastpkt_pktid) {
						clear_register (0);
						orbseek (orbin, ORBNEWEST);
					} else {
						orbseek (orbin, ORBNEXT);
					}
					continue;
				} else {
					clear_register (1);
					fprintf (stderr, "orbcp: orbreap() error.\n");
					break;
				}
			}
			p = packet;
		} else {
			ret = filereap (fin, src, &time, &packet, &nbytes, &bufsize, reverse);
			if (ret < 0) {
				clear_register (1);
				fprintf (stderr, "orbcp: filereap() error for '%s'.\n", filein);
				break;
			}
			if (ret == 0) {
				if (!repeat) break;
				fseek (fin, 4, SEEK_SET);
				if (deltime == 0.0) deltime = lasttime - firsttime + 2.0;
				timeoff += deltime;
				continue;
			}
			lasttime = time;
			if (firsttime == 0.0) firsttime = time;
			time += timeoff;
			if (sim_latency > -100.0) {
				if (sim_offset == 0.0) {timeoff + sim_offset;
					sim_offset = mynow() - sim_latency - time;
				}
				time += sim_offset;
				while (mynow() - time < sim_latency) usleep (100000);
			}
			p = packet;
			hdrsiz = *((int *)p);
			if (reverse) revw (&hdrsiz, &hdrsiz, 4);
			pktsiz = *((int *)(p+4));
			if (reverse) revw (&pktsiz, &pktsiz, 4);
			pkttype = *((int *)(p+12));
			if (reverse) revw (&pkttype, &pkttype, 4);
			if (pkttype == UCSDHS) {
				ptr = p + hdrsiz + 8;
				epoch2bcd(ptr, time);
			} else {
				fprintf (stderr, "orbcp: Warning - Unrecognized pkttype %d.\n", pkttype);
			}
		}
		if (orbout >= 0) {
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
		}
		if (fout) {
			if (fileput (fout, src, time, p, nbytes, reverse) < 0) {
				clear_register (1);
				fprintf (stderr, "orbcp: fileput() error for '%s'.\n", fileout);
				break;
			}
		}
		n++;
		if (npackets > 0 && n >= npackets) break;
	}

}

usage()

{
	fprintf (stderr, "usage: orbcp {-orbin in_orbname | -filein in_filename} [-src srcexpr]\n");
	fprintf (stderr, "             [-orbout out_orbname] [-fileout out_filename] [-reverse]\n");
	fprintf (stderr, "             [-repeat] [-npackets npackets] [-sim_latency sim_latency]\n");
	fprintf (stderr, "             [-ircnt] [-orcnt]\n");
}

int
filereap (f, src, time, packet, nbytes, bufsize, reverse)

FILE *    f;
char *       src;
double *          time;
char **                 packet;
int *                           nbytes;
int *                           	bufsize;
int                                     	reverse;

{
	char srci[30];

	if (fread (srci, 30, 1, f) == 0) {
		if (feof(f)) return (0);
		register_error (1, "filereap: fread() error.\n");
		return (-1);
	}
	strcpy (src, srci);
	if (fread (time, 8, 1, f) == 0) {
		register_error (1, "filereap: fread() error.\n");
		return (-1);
	}
	if (fread (nbytes, 4, 1, f) == 0) {
		register_error (1, "filereap: fread() error.\n");
		return (-1);
	}
	if (reverse) revd (time, time, 8);
	if (reverse) revw (nbytes, nbytes, 4);
	if (*packet == NULL) {
		*bufsize = *nbytes ;
		*packet = (char *) malloc (*nbytes);
		if (*packet == NULL) {
			register_error (1, "filereap: malloc() error.\n");
			return (-1);
		}
	} else if (*nbytes > *bufsize) {
		*bufsize = *nbytes ;
		*packet = (char *) realloc (*packet, *nbytes);
		if (*packet == NULL) {
			register_error (1, "filereap: realloc() error.\n");
			return (-1);
		}
	}
	if (fread (*packet, *nbytes, 1, f) == 0) {
		register_error (1, "filereap: fread() error.\n");
		return (-1);
	}
	return (1);
}

int
fileput (f, src, time, packet, nbytes, reverse)

FILE *   f;
char *      src;
double           time;
char *                 packet;
int                            nbytes;
int                                    reverse;

{
	char srco[30];
	int nbyteso=nbytes;

	if (reverse) revd (&time, &time, 8);
	if (reverse) revw (&nbyteso, &nbyteso, 4);
	strncpy (srco, src, 30);
	if (fwrite (srco, 30, 1, f) == 0) {
		register_error (1, "fileput: fwrite() error.\n");
		return (-1);
	}
	if (fwrite (&time, 8, 1, f) == 0) {
		register_error (1, "fileput: fwrite() error.\n");
		return (-1);
	}
	if (fwrite (&nbyteso, 4, 1, f) == 0) {
		register_error (1, "fileput: fwrite() error.\n");
		return (-1);
	}
	if (fwrite (packet, nbytes, 1, f) == 0) {
		register_error (1, "fileput: fwrite() error.\n");
		return (-1);
	}
	return (0);
}
 
double
mynow()
 
{
        struct timeval tv;
        struct timezone tz;
        double epoch;
 
        gettimeofday (&tv, &tz);
        epoch = tv.tv_sec + 0.000001*tv.tv_usec;
        return (epoch);
}

int
revw (from, to, n)

unsigned char *from, *to;
int n;

{
	unsigned char word[4];
	int i;

	for (i=0; i<n/4; i++) {
		word[3] = *(from++);
		word[2] = *(from++);
		word[1] = *(from++);
		word[0] = *(from++);
		*(to++) = word[0];
		*(to++) = word[1];
		*(to++) = word[2];
		*(to++) = word[3];
	}
}

int
revd (from, to, n)

unsigned char *from, *to;
int n;

{
	unsigned char word[8];
	int i;

	for (i=0; i<n/8; i++) {
		word[7] = *(from++);
		word[6] = *(from++);
		word[5] = *(from++);
		word[4] = *(from++);
		word[3] = *(from++);
		word[2] = *(from++);
		word[1] = *(from++);
		word[0] = *(from++);
		*(to++) = word[0];
		*(to++) = word[1];
		*(to++) = word[2];
		*(to++) = word[3];
		*(to++) = word[4];
		*(to++) = word[5];
		*(to++) = word[6];
		*(to++) = word[7];
	}
}
