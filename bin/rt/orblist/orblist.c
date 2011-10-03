/* %W% %G%  */
/************************************************************************
 *
 *
 *
 ***********************************************************************/
#include "orblist.h"

/*
 #define DEBUG
 */

extern Dbptr dbinvalid();
Arr *Trg;
int llog = 0;
char *pfile = 0;

void usage() {
	char *sccs_id = "$Revision$ $Date$";

	fprintf(
			stderr,
			"usage: %s [-d] [-h] [-m srcmatch ] [-n tim_diff] [-s] orbin [start [twindow]]\n  ",
			Program_Name);
	fprintf(stderr, "orb  	=> ring buffer host name.\n");
	fprintf(stderr, "%s version %s\n", Program_Name, sccs_id);
	exit(1);

}

Detect *new_tbl(srcid, ttime)
	char *srcid;double ttime;

{
	Detect *tbl;

	allot ( Detect *, tbl, 1 );
	tbl->apipe = new_orbpipe(MaxGap);
	tbl->time = ttime;
	strcpy(tbl->srcid, srcid);
	return tbl;
}

int main(argc, argv)
	int argc;char *argv[];

{
	extern int optind;
	extern char *optarg;
	double save_time, pkttime;
	double after = 0, until = 1.0e99;
	double gap, tim_diff, eps = .001;
	int err_in = 0, id, rorb;
	int nselect, nbytes, bsize = 0;
	int diff = 1, hdr = 0, dump = 0, check = 0, sort = 1, maxgap = 30;
	short *sval, hdrsize;
	Detect *dtbl;
	char *packet = 0, srcid[ORBSRCNAME_SIZE];
	char *inorbname = "localhost";
	char *timstr, *wstr;
	char *s1, *s2, *match = 0;
	char *version = "$Revision$ $Date$";

	elog_init(argc, argv);
	elog_notify(0, "%s version %s\n", argv[0], version);
	Program_Name = argv[0];

	DTArr = newarr(0);

	while ((id = getopt(argc, argv, "dhsm:n:")) != -1)
		switch (id) {

		case 'h':
			hdr = 1;
			break;

		case 'd':
			dump = 1;
			break;

		case 'm':
			match = optarg;
			break;

		case 'n':
			diff = atoi(optarg);
			break;

		case 's':
			sort = 0;
			break;

		case '?':
			err_in++;

		}
	if (err_in || argc - optind < 1 || argc - optind > 3)
		usage();

	inorbname = argv[optind++];

	/* Set signal to handle USER interupt  */

	if ((rorb = orbopen(inorbname, "r")) < 0)
		elog_die(0, "Can't open ORB\n");

	if (match) {
		if ((nselect = orbselect(rorb, match)) < 1)
			elog_die(1, "orbselect '%s' failed\n", match);
	}

	if (argc - optind >= 1) {
		timstr = argv[optind++];
		after = str2epoch(timstr);
		if (argc - optind == 1) {
			wstr = argv[optind++];
			until = str2epoch(wstr);
		}
	}

	if (after > 0) {
		if ((id = orbafter(rorb, after - eps)) < 0) {
			elog_complain(1, "orbafter to %d failed\n", after);
			id = orbtell(rorb);
			elog_complain(0, " pktid is still #%d\n", id);
		}

	} else {
		if (orbget(rorb, ORBCURRENT, &id, srcid, &after, &packet, &nbytes,
				&bsize))
			elog_die(0, "fails to get ORBCURRENT time.\n");
	}

	if (MaxGap == 0)
		MaxGap = maxgap;

	/* Loop through RB; runnin triggering algorithm  */

	save_time = 0.0;
	while (1) {
		if (!orbreap(rorb, &id, srcid, &pkttime, &packet, &nbytes, &bsize)) {
			if ((dtbl = (Detect *) getarr(DTArr, srcid)) == 0) {
				dtbl = new_tbl(srcid, pkttime);
				setarr(DTArr, srcid, dtbl);
			}
			if (sort) {
				switch (orbsort(dtbl->apipe, &id, &pkttime, srcid, &packet,
						&nbytes, &bsize)) {

				case 0:
					check = 0;
					break;
				case 2:
					fprintf(
							stderr,
							"SRCNAME: %s TIME: %lf -- packet is out of order \n",
							srcid, pkttime);
					check = 0;
					break;
				case 3:
					fprintf(stderr,
							"SRCNAME: %s TIME: %lf -- duplicate packet \n",
							srcid, pkttime);
					check = 0;
					break;
				default:
					check = 1;
					break;
				}
			} else
				check = 1;
			if (check) {
				fprintf(stderr, "SRCNAME: %s TIME: %s (%lf) \n", srcid, s1
						= strtime(pkttime), pkttime);
				free(s1);
				if (dump)
					hexdump(stderr, packet, nbytes);
				sval = (short *) packet;
				hdrsize = *sval;
				if (hdr)
					hexdump(stderr, packet + hdrsize, nbytes - hdrsize);
				tim_diff = fabs(pkttime - dtbl->time);
				if (save_time != 0.0 && tim_diff > diff) {
					gap = tim_diff - diff;
					fprintf(
							stderr,
							"!!GAP!!  %s  prev_time: %s crnt_time: %s time gap: %lf\n ",
							srcid, s1 = strtime(dtbl->time), s2 = strtime(
									pkttime), gap);
					free(s1);
					free(s2);
					if ((int) gap % diff)
						hexdump(stderr, packet + hdrsize, nbytes - hdrsize);
				}
				dtbl->time = pkttime;
				setarr(DTArr, srcid, dtbl);

			}
			save_time = pkttime;
			if (pkttime >= until)
				break;
		} else {
			elog_complain(0, "Can't get packet after %lf.\n", save_time);

		}

	}
	/* If we get here, everything is OK */
	return 0;
}

