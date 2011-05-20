/* Copyright (c) 1997 Boulder Real Time Technologies, Inc. */
/* All rights reserved */

/* This software module is wholly owned by Boulder Real Time 
   Technologies, Inc. Any use of this software module without
   express written permission from Boulder Real Time Technologies,
   Inc. is prohibited. */

/* Following is the structure of a Quanterra ORB packet. This
   is a representation of the data returned from a Q4120 or
   Q730 digitizer. Note that contrary to popular belief, the
   data packets returned from modern Quanterra digitizers are
   definitely NOT in SEED format. The waveform sample values are 
   normally compressed using Steim 1,2,3 formats, as in SEED. 
   However, the packet header information is not even remotely an
   approximation of the SEED data blockette header. It turns
   out that the hallowed SEED data blockette, advertised as
   being the "true" and "unadulterated" representation of raw
   data as it comes out of the datalogger, is a processed
   format that is created remote from the datalogger.

   We happily follow the lead of Quanterra and munge the actual
   raw packets from the dataloggers into our own ORB packet
   structure as defined below. 

   RAW QUANTERRA DATA FORMAT:

   bytes	type			description
   --------------------------------------------------------------
   0		unsigned char		sequence_digitizer
   1		unsigned char		sequence_type
   2-5		int			header_flag
   6		unsigned char		msg_type
   7		unsigned char		unused
   8		unsigned char		unused
   9		unsigned char		soh_flags
   10-13	char[4]			station
   14-15	short int		millisec
   16-17	short int		timemark_sample_index
   18-21	int			first_sample
   22-23	short int		clock_corr
   24-25	unsigned short int	nsamp
   26		char			samprate (<0 means -1/samprate)
   27		unsigned char		blockette_count
   28		unsigned char		time_year
   29		unsigned char           time_month
   30		unsigned char           time_day
   31		unsigned char           time_hour
   32		unsigned char           time_minute
   33		unsigned char           time_second
   34-37	int			sequence_packet
   38		unsigned char           header_revision
   39		unsigned char           detection_day
   40-41	short int		sequence_detection
   42-45	int			clock_drift
   46-48	char[3]			channel
   49		char			clock_flags
   50-51	char[2]			network
   52-53	char[2]			location
   54-55	short int		ht_sp3 ??
   56-57	short int		microsec
   58		unsigned char		frame_count
   59		unsigned char		hdrsp1 ??
   60-64	unsigned char[5]	z ??
   65		unsigned char		ht_sp4 ??
   66-end	unsigned char		compressed data samples

   OLD ORB QUANTERRA DATA PACKET FORMAT:

   bytes	type			description
   --------------------------------------------------------------
   0-1		unsigned short int	header_size
   2-3		unsigned short int	packet_size
   4-5		unsigned short int	header_type
   6-7		unsigned short int	packet_type
   8		unsigned char           soh_flags
   9		char                    clock_flags
   10-11	unsigned short		level
   12-15	int			nsamp
   16-19	float			samprate
   20-23	float			calib
   24-27	float			calper
   28-31	char[4]			original station code
   32-end	unsigned char           compressed data samples	

   NEW ORB QUANTERRA DATA PACKET FORMAT:

   bytes	type			description
   --------------------------------------------------------------
   0-1		unsigned short int	header_size
   2-3		unsigned short int	packet_size
   4-5		unsigned short int	header_type
   6-7		unsigned short int	packet_type
   8		unsigned char           soh_flags
   9		char                    clock_flags
   10-11	unsigned short		level
   12-15	int			nsamp
   16-19	float			samprate
   20-23	float			calib
   24-27	float			calper
   28-31	char[4]			original station code
   32-35	segtype[4]		wfdisc segtype field
   36-end	unsigned char           compressed data samples	*/

#include <sys/types.h>
#include <netinet/in.h>
 
#include "stock.h"
#include "tr.h"
#include "pkt2.h"
#include "bns.h"

static int split_netstachan ();

int 
qdata2qorbpkt (unsigned char *qdata_pkt, int ndata,
			double timein, double calib, double calper,
			char *netsta, double *time, char *srcname,
			char **packet, int *nbytes, int *bufsize, char *segtype)

{
	int *idata, npts;
	unsigned short level;
	int i, n;
	char *ptr;
	int bsize;
	unsigned char msg_type;
	unsigned char soh_flags;
	unsigned char clock_flags;
	unsigned char *data;
	int nsamp;
	char sampratec;
	float samprate;
	char station[8];
	char channel[8];
	char network[8];
	int year, month, day, doy, hour, minute, isec, millisec, microsec;
	double second;
	unsigned short hdrsiz;
	unsigned short pktsiz;
	short int si;
	unsigned short usi;
	float flt;

	/* grok the quanterra data packet */

	msg_type = qdata_pkt[6];
	switch (msg_type) {
	case 0:
		level = 0;
		break;
	case 1:
		level = 1;
		break;
	case 2:
		level = 2;
		break;
	default:
		elog_log(0, "qdata2qorbpkt: Unknown Quanterra message or compression level (%d).\n", 
						msg_type);
		return (-1);
	}
	soh_flags = qdata_pkt[9];
	clock_flags = qdata_pkt[49];
	data = &(qdata_pkt[66]);
	N2H2(&usi, &(qdata_pkt[24]), 1);
	nsamp = usi;
	sampratec = qdata_pkt[26];
	if (nsamp == 0) return (1);
	if (sampratec == 0) return (1);
	if (sampratec > 0) {
		samprate = (float)sampratec;
	} else {
		samprate = 1.0/(float)(-sampratec);
	}
	for (i=0; i<2; i++) if (qdata_pkt[50+i] != ' ' && qdata_pkt[50+i] != '\0') break;
	n = 2-i;
	memcpy (network, &(qdata_pkt[50+i]), n);
	for (i=0; i<n; i++) if (network[i] == ' ' || network[i] == '\0') break;
	network[i] = '\0';
	for (i=0; i<4; i++) if (qdata_pkt[10+i] != ' ' && qdata_pkt[10+i] != '\0') break;
	n = 4-i;
	memcpy (station, &(qdata_pkt[10+i]), n);
	for (i=0; i<n; i++) if (station[i] == ' ' || station[i] == '\0') break;
	station[i] = '\0';
	for (i=0; i<3; i++) if (qdata_pkt[46+i] != ' ' && qdata_pkt[46+i] != '\0') break;
	n = 3-i;
	memcpy (channel, &(qdata_pkt[46+i]), n);
	for (i=0; i<n; i++) if (channel[i] == ' ' || channel[i] == '\0') break;
	channel[i] = '\0';
	sprintf (srcname, "%s_%s/QCDAT", netsta, channel);
	if (timein == 0.0) {
		year = qdata_pkt[28];
		month = qdata_pkt[29];
		day = qdata_pkt[30];
		hour = qdata_pkt[31];
		minute = qdata_pkt[32];
		isec = qdata_pkt[33];
		N2H2(&si, &(qdata_pkt[14]), 1);
		millisec = si;
		N2H2(&si, &(qdata_pkt[56]), 1);
		microsec = si;
		if (year < 50) year += 2000; else year += 1900;
		doy = mday2doy (year, month, day);
		second = (double)isec + 0.001*millisec + 0.000001*microsec;
		*time = h2e (year, doy, hour, minute, second);
	} else {
		*time = timein;
	}

	/* allocate the output packet */

	hdrsiz = 36;
	pktsiz = hdrsiz + ndata;
	*nbytes = pktsiz;
	bsize = *nbytes + 1;
	if (*packet == NULL) {
		*packet = (char *) malloc (bsize);
		if (*packet == NULL) {
			elog_log(1, "qdata2qorbpkt: malloc() error.\n");
			return (-1);
		}
		*bufsize = bsize;
	} else if (bsize > *bufsize) {
		*packet = (char *) realloc (*packet, bsize);
		if (*packet == NULL) {
			elog_log(1, "qdata2qorbpkt: realloc() error.\n");
			return (-1);
		}
		*bufsize = bsize;
	}

	/* fill the output packet */

	ptr = *packet;
	H2N2 (ptr, &hdrsiz, 1);
	ptr += 2;
	H2N2 (ptr, &pktsiz, 1);
	ptr += 2;
	usi = (unsigned short int)QUANTERRA_DATA_HDR_TYPE2;
	H2N2 (ptr, &usi, 1);
	ptr += 2;
	usi = (unsigned short int)QUANTERRA_DATA_PKT_TYPE2;
	H2N2 (ptr, &usi, 1);
	ptr += 2;
	memcpy (ptr, &soh_flags, 1);
	ptr += 1;
	memcpy (ptr, &clock_flags, 1);
	ptr += 1;
	H2N2 (ptr, &level, 1);
	ptr += 2;
	H2N4 (ptr, &nsamp, 1);
	ptr += 4;
	H2N4 (ptr, &samprate, 1);
	ptr += 4;
	flt = calib;
	H2N4 (ptr, &flt, 1);
	ptr += 4;
	flt = calper;
	H2N4 (ptr, &flt, 1);
	ptr += 4;
	memcpy (ptr, station, 4);
	ptr += 4;
	memcpy (ptr, segtype, 4);
	ptr += 4;
	memcpy (ptr, &(qdata_pkt[66]), ndata);

	/* normal exit */

	return (0);
}

int 
qcomment2qorbpkt (unsigned char *qdata_pkt,
			double *time, char *srcname,
			char **packet, int *nbytes, int *bufsize)

{
	int i, n;
	char network[32], station[32];
	char comment[512];
	char *ptr;
	unsigned char comment_length;
	int year, month, day, doy, hour, minute;
	double second;

	comment_length = qdata_pkt[14];
	memcpy (comment, &(qdata_pkt[15]), (int)comment_length);
	comment[comment_length] = '\0';

	ptr = (char *)qdata_pkt + 15 + comment_length;
	for (i=0; i<2; i++) if (ptr[4+i] != ' ' && ptr[4+i] != '\0') break;
	n = 2-i;
	memcpy (network, &(ptr[4+i]), n);
	for (i=0; i<n; i++) if (network[i] == ' ' || network[i] == '\0') break;
	network[i] = '\0';
	for (i=0; i<4; i++) if (ptr[i] != ' ' && ptr[i] != '\0') break;
	n = 4-i;
	memcpy (station, &(ptr[i]), n);
	for (i=0; i<n; i++) if (station[i] == ' ' || station[i] == '\0') break;
	station[i] = '\0';
	sprintf (srcname, "/log/%s_%s", network, station);

	year = qdata_pkt[8];
	month = qdata_pkt[9];
	day = qdata_pkt[10];
	hour = qdata_pkt[11];
	minute = qdata_pkt[12];
	second = qdata_pkt[13];
	if (year < 50) year += 2000; else year += 1900;
	doy = mday2doy (year, month, day);
	*time = h2e (year, doy, hour, minute, second);

	if (log2orbpkt (comment, packet, nbytes, bufsize) < 0) {
		elog_log(0, "qcomment2qorbpkt: log2orbpkt() error.\n");
		return (-1);
	}

	/* normal exit */

	return (0);
}

int 
log2orbpkt (char *comment, char **packet, int *nbytes, int *bufsize)

{
	int comment_length;
	unsigned short hdrsiz;
	unsigned short pktsiz;
	int bsize;
	unsigned short int usi;
	char *ptr;

	comment_length = strlen(comment);
	hdrsiz = 2 + 2 + 2 + 2;
	pktsiz = hdrsiz + comment_length + 1;
	*nbytes = pktsiz;
	bsize = *nbytes + 1;
	if (*packet == NULL) {
		*packet = (char *) malloc (bsize);
		if (*packet == NULL) {
			elog_log(1, "log2orbpkt: malloc() error.\n");
			return (-1);
		}
		*bufsize = bsize;
	} else if (bsize > *bufsize) {
		*packet = (char *) realloc (*packet, bsize);
		if (*packet == NULL) {
			elog_log(1, "log2orbpkt: realloc() error.\n");
			return (-1);
		}
		*bufsize = bsize;
	}

	ptr = *packet;
	H2N2 (ptr, &hdrsiz, 1);
	ptr += 2;
	H2N2 (ptr, &pktsiz, 1);
	ptr += 2;
	usi = (unsigned short int)QUANTERRA_LOG_HDR_TYPE;
	H2N2 (ptr, &usi, 1);
	ptr += 2;
	usi = (unsigned short int)QUANTERRA_LOG_PKT_TYPE;
	H2N2 (ptr, &usi, 1);
	ptr += 2;
	memcpy (ptr, comment, (int)comment_length + 1);

	/* normal exit */

	return (0);
}

int
setupqorbpkt ()

{
	if (register_pkt_handler (QUANTERRA_DATA_PKT_TYPE, NULL, unstuffqorbpkt) < 0) {
		elog_log(0, "setupqorbpkt: register_pkt_handler() error.\n");
		return (-1);
	}
	if (register_pkt_handler (QUANTERRA_DATA_PKT_TYPE2, NULL, unstuffqorbpkt) < 0) {
		elog_log(0, "setupqorbpkt: register_pkt_handler() error.\n");
		return (-1);
	}

	return (0);
}

int
unstuffqorbpkt (double time, 
 	char *srcname, 
	char *packet, 
	Packet **pkt, 
	void *private )

{
	PktChannel *pktch;
	int i, size;
	char *ptr;
	unsigned short hdrsiz;
	unsigned short pktsiz;
	unsigned short hdrtyp;
	unsigned short pkttyp;
	unsigned short usi;
	int level;
	int nsamp;
	float samprate, calib, calper;
	int npts;
	char station[6];
	char segtype[6];
	Steim *conf;
	char *data;
	int *idata;

	ptr = packet;
	N2H2 (&hdrsiz, ptr, 1);
	ptr += 2;
	N2H2 (&pktsiz, ptr, 1);
	ptr += 2;
	N2H2 (&hdrtyp, ptr, 1);
	ptr += 2;
	N2H2 (&pkttyp, ptr, 1);
	ptr += 2;
	if (pkttyp != (unsigned short int)QUANTERRA_DATA_PKT_TYPE &&
			pkttyp != (unsigned short int)QUANTERRA_DATA_PKT_TYPE2) {
		elog_log(0, "unstuffqorbpkt: Wrong pkttyp.\n");
		return (0);
	}
	if (hdrtyp != (unsigned short int)QUANTERRA_DATA_HDR_TYPE &&
			hdrtyp != (unsigned short int)QUANTERRA_DATA_HDR_TYPE2) {
		elog_log(0, "unstuffqorbpkt: Wrong hdrtyp.\n");
		return (0);
	}
	size = pktsiz - hdrsiz;
	ptr += 2;
	N2H2 (&usi, ptr, 1);
	level = usi;
	ptr += 2;
	N2H4 (&nsamp, ptr, 1);
	ptr += 4;
	N2H4 (&samprate, ptr, 1);
	ptr += 4;
	N2H4 (&calib, ptr, 1);
	ptr += 4;
	N2H4 (&calper, ptr, 1);
	ptr += 4;
	memcpy (station, ptr, 4);
	ptr += 4;
	if (hdrtyp == (unsigned short int)QUANTERRA_DATA_HDR_TYPE2) {
		memcpy (segtype, ptr, 4);
		ptr += 4;
	} else strcpy (segtype, "V");
	data = ptr;

	if (*pkt == NULL) {
		*pkt = newpkt();
		if (*pkt == NULL) {
			elog_log(0, "unstuffqorbpkt: newpkt() error.\n");
			return (0);
		}
		(*pkt)->chan = NULL;
	}
	(*pkt)->pkttype = pkttyp;
	(*pkt)->hdrtype = hdrtyp;
	(*pkt)->nchannels = 1;

	if ((*pkt)->chan == NULL) {
		(*pkt)->chan = newtbl (1);
		if ((*pkt)->chan == NULL) {
			elog_log(0, "unstuffqorbpkt: newtbl() error.\n");
			return (0);
		}
	}

	pktch = (PktChannel *) gettbl ((*pkt)->chan, 0);
	if (pktch == NULL) {
		pktch = (PktChannel *) malloc (sizeof(PktChannel));
		if (pktch == NULL) {
			elog_log(1, "unstuffqorbpkt: malloc() error.\n");
			return (0);
		}
		settbl ((*pkt)->chan, 0, pktch);
		pktch->data = NULL;
		pktch->nbytes = 0;
	}
	if (pktch->data) {
		if (pktch->nbytes < nsamp*4) {
			pktch->nbytes = nsamp*4;
			pktch->data = (void *) realloc (pktch->data , pktch->nbytes);
			if (pktch->data == NULL) {
				elog_log(1, "unstuffqorbpkt: realloc() error.\n");
				return (0);
			}
		}
	} else {
		pktch->nbytes = nsamp*4;
		pktch->data = (void *) malloc (pktch->nbytes);
		if (pktch->data == NULL) {
			elog_log(1, "unstuffqorbpkt: malloc() error.\n");
			return (0);
		}
	}

	split_netstachan (srcname, pktch->net, pktch->sta, pktch->chan);
	pktch->time = time;
	pktch->samprate = samprate;
	pktch->calib = calib;
	strcpy (pktch->segtype, segtype);
	pktch->datatype = trINT;
	pktch->nsamp = nsamp;

	if (level == 0) {
		N2H4 (pktch->data, data, nsamp);
	} else {
		conf = newsteim();
		conf->record = data;
		conf->record_size = 448;
		conf->sdh.data_offset = 0;
		conf->sdh.nsamp = nsamp;
		conf->level = level;
		if ( usteim (conf, &idata, &npts) ) {
			elog_log(0, "unstuffqorbpkt: usteim() error.\n");
			return (0);
		}
		memcpy (pktch->data, idata, nsamp*4);
		conf->record = NULL;
		freesteim(conf);
	}

	return (1);
}

int
orbpkt2log (char *packet, char **comment)

{
	char *ptr;
	unsigned short hdrsiz;
	unsigned short pktsiz;
	unsigned short hdrtyp;
	unsigned short pkttyp;
	int size;

	ptr = packet;
	N2H2 (&hdrsiz, ptr, 1);
	ptr += 2;
	N2H2 (&pktsiz, ptr, 1);
	ptr += 2;
	N2H2 (&hdrtyp, ptr, 1);
	ptr += 2;
	N2H2 (&pkttyp, ptr, 1);
	ptr += 2;
	if (pkttyp != (unsigned short int)QUANTERRA_LOG_PKT_TYPE) {
		elog_log(0, "orbpkt2log: Wrong pkttyp.\n");
		return (0);
	}
	if (hdrtyp != (unsigned short int)QUANTERRA_LOG_HDR_TYPE) {
		elog_log(0, "orbpkt2log: Wrong hdrtyp.\n");
		return (0);
	}
	*comment = ptr;

	return (1);
}

static int
split_netstachan (netstachan, net, sta, chan)
 
char *            netstachan;
char *                        net;
char *                             sta;
char *                                  chan;
 
{
        char *ptr;
        int i;
 
        *net = '\0';
        *sta = '\0';
        *chan = '\0';
        ptr = net;
        for (i=0; i<strlen(netstachan); i++) if (netstachan[i] == '_') break; else {*ptr = netstachan[i]; ptr++;}
        *ptr = '\0';
        ptr = sta;
        for (i++; i<strlen(netstachan); i++) if (netstachan[i] == '_') break; else {*ptr = netstachan[i]; ptr++;}
        *ptr = '\0';
        ptr = chan;
        for (i++; i<strlen(netstachan); i++) if (netstachan[i] == '/') break; else {*ptr = netstachan[i]; ptr++;}
        *ptr = '\0';
}

