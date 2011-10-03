#include <math.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <regex.h>

#include "db.h"
#include "tr.h"
#include "coords.h"
#include "stock.h"
#include "orb.h"
#include "defunctpkt.h"

#define ERRBUF_SIZE 128

typedef struct Segment {
    struct Segment *next ;
    double t0, t1 ; 
    int nsamp ; 
    Trsample *data ;
} Segment ;

typedef struct Head {
    struct Segment *next;
    struct Head *anomalous;
    int             maxsamp, init;
    char	    net[PKT_NAMESIZE], sta[PKT_NAMESIZE], chan[PKT_NAMESIZE] ;
    double 	    t0, t1 ;
    double	    samprate ;
    double 	    calib ;
    Trsample	   *data ;
}               Head;

typedef struct Orbacc_hook {
    Tbl            *chandata;
    double          req_time; 
    Arr		   *selected, *rejected ;
    char           *accselect;
    regex_t	   *acc_re;
}               Orbacc_hook;

static void
free_achan ( achan ) 
PktChannel *achan ;
{
    free(achan) ; 
}

static void 
free_chandata (void *v_achan) 
{
    PktChannel *achan ; 
    achan = (PktChannel *) v_achan ; 
    free(achan->data) ; 
    free_achan(achan) ; 
}

static void
free_orbacc (orb_hook)
Orbacc_hook        *orb_hook;
{
    if ( orb_hook->chandata != 0)
	freetbl(orb_hook->chandata, free_chandata) ;

    if ( orb_hook->acc_re != 0) {
        regfree ( orb_hook->acc_re ) ;
        free ( orb_hook->acc_re ) ;
    }

    if ( orb_hook->accselect != 0 ) 
	free (orb_hook->accselect) ;

    freearr(orb_hook->selected, free) ; 
    freearr(orb_hook->rejected, 0) ;
    free (orb_hook);
}

static Head *
new_head()
{
    Head *new ;
    allot ( Head *, new, 1) ; 
    new->next = 0 ;
    new->anomalous = 0 ;
    new->maxsamp = 0 ; 
    new->init = 0 ; 
    new->t0 = new->t1 = 0.0 ; 
    new->samprate = 0.0 ; 
    new->data = 0 ;
    return new ; 
}

static int
acc_anomalous (achan, req_time, req_twin, primary) 
PktChannel     *achan;
double req_time, req_twin ;
Head *primary ;
{
    Head *anomalous ; 
    Trsample *data ;
    int i, pt0, npts ;
    int		   *pktdata ;

    /* just append to end of anomalous list */
    while ( primary->anomalous != 0 ) 
	primary = primary->anomalous ; 

    primary->anomalous = anomalous = new_head() ;
    strcpy(anomalous->net, achan->net) ; 
    strcpy(anomalous->sta, achan->sta) ; 
    strcpy(anomalous->chan, achan->chan) ; 
    anomalous->samprate = achan->samprate ;
    anomalous->calib = achan->calib ;

    npts = trclip ( achan->time, 
    		achan->samprate, achan->nsamp,
		req_time, req_time+req_twin,
		&pt0, &npts, &anomalous->t0, &anomalous->t1) ; 

    if ( npts < 1 ) 
	return 0 ; 

    anomalous->maxsamp = npts ;
    allot (Trsample *, anomalous->data, npts);
    data = anomalous->data ;
    pktdata = (int *) achan->data ;
    for (i = 0; i < npts ; i++)
	data[i] = htonl(pktdata[i+pt0]);

    return 1 ; 
}

static int 
anomalous2db (anomalous, tr)
Head *anomalous ; 
Dbptr tr ;
{
    Trsample	   *data ;
    Head 	*last ;
    int retcode=0 ;

    while ( anomalous != 0 ) {
	retcode++ ;
	tr.record = dbaddnull (tr);
	data = (Trsample *) anomalous->data ;
	dbputv (tr, 0, "net", anomalous->net,
		"sta", anomalous->sta,
		"chan", anomalous->chan,
		"time", anomalous->t0,
		"endtime", ENDTIME(anomalous->t0, anomalous->samprate, anomalous->maxsamp),
		"nsamp", anomalous->maxsamp,
		"calib", anomalous->calib,
		"samprate", anomalous->samprate,
		"datatype", "s4",
		"data", data,
		0);
	last = anomalous ;
	anomalous = anomalous->anomalous ;
	free(last) ;
    }
    return retcode ;
}

static int
accumulate (int pktid, PktChannel *achan, regex_t *acc_re,
            double req_time, double req_twin,
            Arr *selected, Arr *rejected, int *overlaps)
{
    char            net_sta_chan[ORBSRCNAME_SIZE * 2];
    Head	   *primary;
    Segment	   *asegment, *prev, *next ;
    int             i, len, maxsamp, i0 ;
    Trsample	   *data ;
    int		   *pktdata ;
    double	   t0, t1 ; 
    int		   pt0, npts ; 
    int		   condensed ;
    void	  *regs=0 ;
    regmatch_t     matches ;


    sprintf (net_sta_chan, "%s_%s_%s", achan->net, achan->sta, achan->chan);
   
    /* find matching accumulation of data */
    primary = (Head *) getarr (selected, net_sta_chan);
    if ( primary == 0 ) {
	if ( getarr(rejected, net_sta_chan) )
	    return 0 ; 

	len = strlen (net_sta_chan);
	if ( acc_re == 0 
            || ( regexec (acc_re, net_sta_chan, 1, &matches, 0) == 0
                && matches.rm_so == 0
                && matches.rm_eo == strlen(net_sta_chan))) {

	    primary = new_head() ;
	    strcpy(primary->net, achan->net) ; 
	    strcpy(primary->sta, achan->sta) ; 
	    strcpy(primary->chan, achan->chan) ; 
	    setarr (selected, net_sta_chan, primary);
	} else {
	    setarr (rejected, net_sta_chan, (void *) -1);
	    return 0 ; 
	}
    }

    if ( ! primary->init ) {
	primary->init = 1 ;
	primary->samprate = achan->samprate ;
	primary->calib = achan->calib ;
	maxsamp = req_twin*achan->samprate + 10 ; 
	if ( maxsamp > primary->maxsamp ) {
	    if ( primary->maxsamp != 0 ) 
		free(primary->data) ; 
	    primary->maxsamp = maxsamp;
	    allot (Trsample *, primary->data, maxsamp);
	}
	primary->t0 = traligned(req_time, achan->time, achan->samprate ) ; 
	primary->t1 = traligned(req_time+req_twin, achan->time, achan->samprate ) ; 
    }

    if (! TRSAMERATE(primary->samprate, achan->samprate)){
	elog_complain( 0, "%s pktid#%d samprates don't match: %10.4f %10.4f\n", net_sta_chan, 
		pktid, primary->samprate, achan->samprate ) ;
	return acc_anomalous (achan, req_time, req_twin, primary ) ; 
    }

    if (! TRSAMETICKS(primary->t0, achan->time, primary->samprate)) {
	char *s1, *s2 ;
	elog_complain( 0, "%s pktid#%d ticks don't match: %s %s rate=%10.4f\n", net_sta_chan, 
		pktid, s1=strtime(primary->t0), s2=strtime(achan->time), primary->samprate ) ;
	free(s1) ;
	free(s2) ;
	return acc_anomalous (achan, req_time, req_twin, primary ) ; 
    }

    if (primary->calib != achan->calib ) {
	elog_complain( 0, "%s pktid#%d calibs don't match: %10.4f %10.4f\n", net_sta_chan, 
		pktid, primary->calib, achan->calib ) ;
	return acc_anomalous (achan, req_time, req_twin, primary ) ; 
    }

    /* packet has correct data rate and right registration: 
	calculate the range of times and the data points */
    npts = trclip ( achan->time, 
    		achan->samprate, achan->nsamp,
		primary->t0, primary->t1, 
		&pt0, &npts, &t0, &t1 ) ; 
    i0 = TIME2SAMP(primary->t0, primary->samprate, t0) ; 

    if ( npts < 1 ) 
	return 0 ; 

    /* now find where in the list of packets this should go */
    prev = 0 ; 
    next = primary->next ; 
    while ( next != 0 && t0 > next->t0 ) {
	prev = next ; 
	next = next->next ; 
    }

    /* check for overlaps */
    if ( (prev != 0 && prev->t1 > t0 + .01/primary->samprate)
      || (next != 0 && t1 > next->t0 + .01/primary->samprate)) {
	char *s1, *s2, *s3, *s4 ;
	if (prev != 0 && prev->t1 > t0) 
	    elog_complain( 0, "%s pktid#%d overlapping packets: \n\tprev->t0=%s prev->t1=%s\n\t      t0=%s       t1=%s\n", net_sta_chan, 
		    pktid, s1=strtime(prev->t0), s2=strtime(prev->t1), s3=strtime(t0), s4=strtime(t1)) ; 
	else
	    elog_complain( 0, "%s pktid#%d overlapping packets: \n\tt0      =%s       t0=%s\n\tnext->t0=%s next->t1=%s\n", net_sta_chan, 
		    pktid, s1=strtime(t0), s2=strtime(t1), s3=strtime(next->t0), s4=strtime(next->t1)) ; 
	free(s1) ;
	free(s2) ;
	free(s3) ;
	free(s4) ;
	acc_anomalous (achan, req_time, req_twin, primary ) ; 
	(*overlaps)++ ; 
	return 1 ;
    }

    /* data fits, so copy it into buffer */
    data = primary->data ;
    pktdata = (int *) achan->data ;
    for (i = 0; i < npts ; i++)
	data[i + i0] = htonl(pktdata[i+pt0]);

    /* attempt to condense with existing data */
    condensed = 0 ;
    if ( prev != 0
	&& TRCONTIGUOUS (prev->t0, t0, 
		primary->samprate, prev->nsamp )) {
	prev->nsamp += npts ;
	prev->t1 = t1 ;
	condensed = 1 ;
    }

    if ( next != 0 
	&& TRCONTIGUOUS (t0, next->t0,
		primary->samprate, npts )) {
	if ( condensed ) { 
	    prev->nsamp += next->nsamp ; 
	    prev->t1 = next->t1 ; 
	    prev->next = next->next ; 
	    free(next) ; 
	} else { 
	    next->nsamp += npts ;
	    next->t0 = t0 ;
	    next->data = data + i0 ; 
	    condensed = 1 ;
	}
    }

    /* if odd man out, add a new segment to linked list */
    if ( !condensed ) {
	allot(Segment *, asegment, 1 ) ; 
	asegment->t0 = t0 ; 
	asegment->t1 = t1 ; 
	asegment->nsamp = npts ;
	asegment->next = next ;
	asegment->data = data + i0 ; 
	if ( prev != 0 ) 
	    prev->next = asegment ;
	else
	    primary->next = asegment ;
    }

    return 1 ;
}

static int
_acc2tr (selected, tr)
Arr *selected ; 
Dbptr tr ;
{
    Tbl            *channels;
    Trsample	   *data ;
    int i, nchan ;
    Head 	*primary ;
    Segment	*asegment, *last ;
    char *net_sta_chan ;
    int nrecords, need2free ;

    channels = keysarr (selected);
    nchan = maxtbl (channels);
    for (i = 0; i < nchan; i++) {
	net_sta_chan = gettbl(channels, i) ; 
	need2free = 1 ;
	primary = (Head *) getarr (selected, net_sta_chan);
	asegment = primary->next ;
	while ( asegment != 0 ) {
	    tr.record = dbaddnull (tr);
	    if ( primary->data != asegment->data ) {
		allot ( Trsample *, data, asegment->nsamp ) ; 
		memcpy ( data, asegment->data, asegment->nsamp * sizeof(Trsample));
	    } else {
		need2free = 0 ;
		data = (Trsample *) asegment->data ;
		primary->maxsamp = 0 ; 
	    }
	    dbputv (tr, 0, "net", primary->net,
		    "sta", primary->sta,
		    "chan", primary->chan,
		    "time", asegment->t0,
		    "endtime", ENDTIME(asegment->t0, primary->samprate,asegment->nsamp),
		    "nsamp", asegment->nsamp,
		    "calib", primary->calib,
		    "samprate", primary->samprate,
		    "datatype", "s4",
		    "data", data,
		    0);
	    last = asegment ;
	    asegment = asegment->next ;
	    free(last) ;
	}
	primary->init = 0 ;
	primary->next = 0 ; 
	if ( need2free ) {
	    free(primary->data) ; 
	}
	primary->data = 0 ;
	primary->maxsamp = 0 ; 
	if ( primary->anomalous != 0)
	    anomalous2db ( primary->anomalous, tr ) ; 
	primary->anomalous = 0 ; 
    }
    freetbl (channels, 0);
    dbquery  ( tr, dbRECORD_COUNT, &nrecords ) ; 
    return nrecords ;
}

int 
orbacc (orb, accselect, req_twin, req_overlap, latency, trp, hookp)
int             orb;
char           *accselect;
double          req_twin,
		req_overlap,
                latency;
Dbptr         *trp;
Hook          **hookp;
{
    Orbacc_hook    *p;
    Tbl            *newchandata;
    Packet         *unstuffed=0;
    PktChannel     *achan, *schan ;
    char           *packet;
    int             nbytes, bufsize = 0;
    double          pkttime;
    int pktid ; 
    char	srcname[ORBSRCNAME_SIZE] ;
    double	    req_time, 
		    req_endtime,
		    req_nexttime,
		    min_endtime,
                    max_time,
		    endtime ;
    Dbptr	    tr ;
    int		    i, n ;
    char	   *data ;
    int		    retcode=0, unstuff ;
    int		    overlaps = 0 ; 
    int		    err ;

    if (*hookp == 0) {
	*hookp = new_hook (free_orbacc);
	allot (Orbacc_hook *, p, 1 ) ; 
	(*hookp)->p  = p ;
	p->chandata = newtbl (0);
	if (orbget (orb, ORBCURRENT,
		    &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize)) {
	    elog_log(1, "orbget fails\n");
	    return -1;
	}
	p->req_time = pkttime;
	p->acc_re = 0;
	p->accselect = 0;
	p->selected = newarr(0); 
	p->rejected = newarr(0); 
    }
    p = (*hookp)->p;

    if ((*trp).database < 0) {
	tr = trnew (0, 0);
	tr = dblookup (tr, 0, "trace", 0, 0);
	*trp = tr ;
    } else {
	tr = *trp;
	trtruncate (tr, 0);
    }

    if (accselect != 0 && *accselect != 0 
	&& (p->accselect == 0 || strcmp (p->accselect, accselect) != 0)) {
	allot (regex_t *, p->acc_re, 1);
	if ( p->accselect != 0 ) 
	    free(p->accselect) ; 
	p->accselect = strdup (accselect);
        if ( (err = regcomp(p->acc_re, accselect, REG_EXTENDED)) != 0 ) {
            char errbuf[ERRBUF_SIZE] ;
            regerror ( err, p->acc_re, errbuf, ERRBUF_SIZE) ;
            elog_log(0, "couldn't compile pattern '%s'\n", errbuf);
            return -2;
        }

	freearr(p->selected, free) ; 
	freearr(p->rejected, free) ;
	p->selected = newarr(0); 
	p->rejected = newarr(0); 
    }

    req_time = max_time = min_endtime = p->req_time;
    req_endtime = req_time + req_twin;
    req_nexttime = p->req_time = req_time + req_twin - req_overlap ;

    newchandata = newtbl (0);
    n = maxtbl (p->chandata);
    for (i = 0; i < n; i++) {
	achan = (PktChannel *) gettbl (p->chandata, i);

	endtime = SAMP2TIME (achan->time, achan->samprate, achan->nsamp);
	max_time = MAX (max_time, achan->time);
	min_endtime = MIN (min_endtime, endtime);
	accumulate (pktid, achan, p->acc_re, req_time, req_twin, 
	    p->selected, p->rejected, &overlaps);

	if (endtime > req_nexttime) {
	    pushtbl (newchandata, achan);
	} else {
	    free (achan->data);
	    free_achan (achan);
	}
    }
    freetbl (p->chandata, 0);
    p->chandata = newchandata;

    while (max_time - req_endtime < latency) {
	if (orbreap (orb, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize)) {
	    elog_log(1, "orbreap fails\n");
	    retcode = -3 ;
	    break;
	}

#ifdef DEBUG
	{
	char	   *s ;
	elog_complain( 0, "%5d %s %s\n", pktid, srcname, s=strtime(pkttime) ) ; 
	free(s) ;
	}
#endif

	switch (unstuff=unstuffpkt (pkttime, srcname, packet, &unstuffed)) {
	    case 1:
		for (i = 0; i < unstuffed->nchannels; i++) {
		    achan = (PktChannel *) gettbl (unstuffed->chan, i);
		    if ( achan->time < req_endtime ) 
			accumulate (pktid, achan, p->acc_re, req_time, req_twin, 
			    p->selected, p->rejected, &overlaps);

		    endtime = SAMP2TIME (achan->time, achan->samprate, achan->nsamp);
		    min_endtime = MIN (min_endtime, endtime);
		    max_time = MAX (max_time, achan->time);

		    if (endtime > req_nexttime) {
			allot (PktChannel *, schan, 1);
			*schan = *achan;
			allot (char *, data, achan->nbytes);
			schan->data = (void *) data ;
			memcpy (schan->data, achan->data, achan->nbytes);
			pushtbl (newchandata, schan);
		    }
		}
		break ;

	    case 2:	/* status packet */
		break; 

	    default:
		retcode = -4 ;
		elog_complain( 0, "unknown return code %d from unstuffpkt\n", 
		    unstuff ) ; 
		break ;
	}

    }

    retcode = (_acc2tr (p->selected, tr) > 0) ? 0 : -5 ;
    if ( bufsize > 0 ) 
	free(packet) ;

    if ( unstuffed != 0 )
	freePkt(unstuffed) ;

    return retcode;
}
