/*	low level i/o routines for ah format records

 *		-- witte	6 june 85
 *
 *	gethead routine was altered so it can read the ah structure
 *      on a sun4 machine  - Mahdad Parsi June 4 1992.
 */

#include <stdio.h>
#include <sys/ioctl.h>
#include <rpc/rpc.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ahhead.h"
#include "ahio.h"

void 
get_null_head (ahhed * hed);
void 
acpy (char *from, char *to, unsigned int nbytes);
void 
ah_error (char *s1, char *s2, int status);
int 
xdr_ahhead (XDR * xdrsp, ahhed * ahheadp);

/* ah error processing */

int             ah_errno = 0;
int             ah_nerr = 10;

/* ah error numbers */
#define	AE_RHED		1	       /* error reading header	 */
#define	AE_DTYPE	2	       /* bad data type	 */
#define	AE_WHED		3	       /* error writing header	 */
#define	AE_RDATA	4	       /* error reading data	 */
#define	AE_WDATA	5	       /* error writing data	 */
#define	AE_WRECORD	6	       /* error writing record	 */
#define	AE_RRECORD	7	       /* error reading record	 */
#define	AE_TTYOUT	8	       /* binary going to tty	 */
#define	AE_TTYIN	9	       /* binary coming from tty	 */

/* ah errlist */

char           *ah_errlist[] = {
    "no error",			       /* 0	no error	 */
    "read header error",	       /* 1	AE_RHED		 */
    "bad data type",		       /* 2	AE_DTYPE	 */
    "write header error",	       /* 3	AE_WHED		 */
    "read data error",		       /* 4	AE_RDATA	 */
    "write data error",		       /* 5	AE_WDATA	 */
    "write record error",	       /* 6	AE_WRECORD	 */
    "read record error",	       /* 7	AE_RRECORD	 */
    "tty can't get binary",	       /* 8	AE_TTYOUT	 */
    "tty can't send binary"	       /* 9	AE_TTYIN	 */
};

/*	gethead
 *		gets the next header from the stream pointed to by
 *		file_pt and returns this header in the structure head.
 *		file_pt is assumed to be positioned at the next header,
 *		and does not search.
 *
 *	added:  Reads the input fiule byte by byte and sets the appropriate
 *              structure fields in the ah header. June 4 1992.
 *
 *	returns:
 *			1		->	no error
 *			-1		->	not enough head to read
 *			-2		->	bad data type
 */
int 
gethead (ahhed * head, FILE * file_pt)
{
    int             ierr = 0,	       /* error indicator */
                    i;		       /* counter variable */
    int             sizefloat,	       /* sizeofloat()  */
                    sizedouble,	       /* sizeodouble()  */
                    sizeint,	       /* sizeoint()  */
                    sizeshort;	       /* sizeoshort()  */

    sizedouble = sizeof (double);
    sizeint = sizeof (int);
    sizefloat = sizeof (float);
    sizeshort = sizeof (short);

    /* station info */
    if ((ierr = fread (head->station.code, CODESIZE, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (head->station.chan, CHANSIZE, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (head->station.stype, STYPESIZE, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->station.slat, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->station.slon, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->station.elev, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->station.DS, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->station.A0, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    for (i = 0; i < NOCALPTS; i++) {
	if ((ierr = fread (&head->station.cal[i].pole.r, sizefloat, 1, file_pt)) == -1)
	    goto CH4ERROR;
	if ((ierr = fread (&head->station.cal[i].pole.i, sizefloat, 1, file_pt)) == -1)
	    goto CH4ERROR;
	if ((ierr = fread (&head->station.cal[i].zero.r, sizefloat, 1, file_pt)) == -1)
	    goto CH4ERROR;
	if ((ierr = fread (&head->station.cal[i].zero.i, sizefloat, 1, file_pt)) == -1)
	    goto CH4ERROR;
    }				       /* for */

    /* event info */
    if ((ierr = fread (&head->event.lat, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.lon, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.dep, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.ot.yr, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.ot.mo, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.ot.day, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.ot.hr, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.ot.mn, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->event.ot.sec, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (head->event.ecomment, COMSIZE, 1, file_pt)) == -1)
	goto CH4ERROR;


    /* record info */
    if ((ierr = fread (&head->record.type, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.ndata, sizeint, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.delta, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.maxamp, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.abstime.yr, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.abstime.mo, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.abstime.day, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.abstime.hr, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.abstime.mn, sizeshort, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.abstime.sec, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (&head->record.rmin, sizefloat, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (head->record.rcomment, COMSIZE, 1, file_pt)) == -1)
	goto CH4ERROR;
    if ((ierr = fread (head->record.log, LOGSIZE, 1, file_pt)) == -1)
	goto CH4ERROR;


    /* extra */
    for (i = 0; i < NEXTRAS; i++) {
	if ((ierr = fread (&head->extra[i], sizefloat, 1, file_pt)) == -1)
	    goto CH4ERROR;
    }


    if ((head->record.type < TYPEMIN) || (head->record.type > TYPEMAX)) {
	get_null_head (head);
	ierr = -2;
	ah_errno = AE_DTYPE;
    }
CH4ERROR:if (ierr == -1) {
	get_null_head (head);
	ierr = -1;
	ah_errno = AE_RHED;
    }
    return (ierr);

}



/*	puthead
 *		writes the header head onto the stream pointed to by
 *		file_pt.
 *	returns:
 *			1		->	no error
 *			-1		->	error writing header
 */
int 
puthead (ahhed * head, FILE * file_pt)
{
    int             ierr = 0;

    if ((ierr = fwrite ((char *) head, sizeof (ahhed), 1, file_pt)) != 1) {
	ah_errno = AE_WHED;
	ierr = -1;
    }
    return (ierr);
}



/*	size
 *		returns the size (in bytes) of the data type given by
 *		head->record.type.
 *	returns:
 *			size of data type	->	no error
 *			-1			->	unknown data type
 */
int 
size (ahhed * head)
{
    int             type_size = 0;

    switch (head->record.type) {
      case 1:			       /* real time series */
	type_size = sizeof (float);
	break;
      case 2:			       /* complex time series */
	type_size = sizeof (complex);
	break;
      case 3:			       /* real x,y pairs */
	type_size = sizeof (vector);
	break;
      case 4:			       /* x real, y complex, or real x,y,z */
	type_size = sizeof (tensor);
	break;
      case 5:			       /* complex x,y pairs */
	type_size = 2 * sizeof (complex);
	break;
      case 6:			       /* double */
	type_size = sizeof (double);
	break;
      default:			       /* unknown data type */
	type_size = -1;
	ah_errno = AE_DTYPE;
	break;
    }
    return (type_size);
}


/*	tohead
 *		positions the read/write head to the beginning of the
 *		n-th header in the file pointed to by file_pt.
 *	returns:
 *			n	->	no error
 *			-1	->	not enough heads
 *			-2	->	bad seek
 */
int 
tohead (int n, FILE * file_pt)
{
    ahhed           head;
    int             i,
                    ierr;

    rewind (file_pt);
    for (i = 1; i < n; ++i) {
	if (gethead (&head, file_pt) == 1) {
	    if (fseek (file_pt, (long) (head.record.ndata) * (size (&head)), 1) == -1) {
		ierr = -2;	       /* bad seek */
		ah_errno = AE_RHED;
		return (ierr);
	    }
	} else {
	    ierr = -1;		       /* not enough head */
	    ah_errno = AE_RHED;
	    return (ierr);
	}
    }
    return (i);			       /* success */
}



/*	getdata
 *		reads from the file pointed to by file_pt into
 *		the array pointed to by array.  It assumes that
 *		the read/write head is positioned correctly
 *		(i.e., right after the header), and does not
 *		search.  Works for any allowed data type.
 *	returns:
 *			number of elements read	->	OK
 *			-1			->	error
 */
int 
getdata (ahhed * head, char *array, FILE * file_pt)
{
    int             ierr = 0;

    if ((ierr = fread (array, size (head), (int) head->record.ndata, file_pt)) != (int) head->record.ndata) {
	ah_errno = AE_RDATA;
	ierr = -1;
    }
    return (ierr);
}


/*	putdata
 *		writes array to the file pointed to by
 *		file_pt.  Works for any allowed data type.
 *	returns:
 *			number of elements written	->	OK
 *			-1			->	error
 */
int 
putdata (ahhed * head, char *array, FILE * file_pt)
{
    int             ierr = 0;

    if ((ierr = fwrite (array, size (head), (int) head->record.ndata, file_pt)) != (int) head->record.ndata) {
	ah_errno = AE_WDATA;
	ierr = -1;
    }
    return (ierr);
}


/*	putrecord
 *		writes head and array to the file pointed to by
 *		file_pt.  Works for any allowed data type.
 *	returns:
 *			0	->	OK
 *			-1	->	error writing header
 *			-2	->	error writing data
 */
int 
putrecord (ahhed * head, char *array, FILE * file_pt)
{
    int             ierr = 0;

    (puthead (head, file_pt) == 1) ? ((putdata (head, array, file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if (ierr)
	ah_errno = AE_WRECORD;

    return (ierr);
}


/*	getrecord
 *		gets header and data from the file pointed to by
 *		file_pt and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search.  Obviously, calling
 *		routine must have allocated enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	error reading header
 *			-2	->	error reading data
 */
int 
getrecord (ahhed * head, char *array, FILE * file_pt)
{
    int             ierr = 0;

    (gethead (head, file_pt) == 1) ? ((getdata (head, array, file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if (ierr)
	ah_errno = AE_RRECORD;
    return (ierr);
}

/*
 *	getrecord2
 *		gets header and data from the file pointed to by
 *		file_pt and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search (although it does
 *		some error checking).  Space for array is allocated, so
 *		be sure to pass a pointer to the data pointer. Got it?
 *	returns:
 *			0	->	ok
 *			-1	->	error reading record
 *			-2	->	error allocating space for data
 */
int 
getrecord2 (ahhed * head, char **array, FILE * file_pt)
{
    int             ierr = 0;
    int             gethead (ahhed * head, FILE * file_pt);
    char           *mkdatspace (ahhed * head);

    if (gethead (head, file_pt) != 1) {
	ierr = -1;
	return (ierr);
    }
    *array = mkdatspace (head);
    if (*array == NULL) {
	ierr = -2;
	return (ierr);
    }
    if (getdata (head, *array, file_pt) < 0)
	ierr = -1;

    return (ierr);
}


/*	gogethead
 *		gets n-th header from the stream pointed to by
 *		file_pt and returns this header in the structure
 *		head.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading header
 */
int 
gogethead (int n, ahhed * head, FILE * file_pt)
{
    int             ierr = 0;

    (tohead (n, file_pt) == n) ? ((gethead (head, file_pt) < 1) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return (ierr);
}


/*	gogetrecord
 *		gets n-th record (header and data) from the stream
 *		pointed to by file_pt and places it in head and array.
 *		Calling routine must allocate enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading record
 */
int 
gogetrecord (int n, ahhed * head, char *array, FILE * file_pt)
{
    int             ierr = 0;

    (tohead (n, file_pt) == n) ? ((getrecord (head, array, file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return (ierr);
}

/* logger adds a 10 character comment to the log section of the header
 * comment should be passed as a character pointer must be terminated
 * by a ';' and a `/0`
 * returns:
 *	logger =  0  -> log info added to header structure
 *	logger = -1  -> no ';', added
 *	logger = -2  -> input string greater than LOGENT
 *                      input truncated to allowable limit
 *	logger = -3  -> attempt to make log string greater than LOGSIZE
 *                      input comment truncated to fit
 *
 *			written by Tom Boyd   6/10/85
 */

int 
logger (char *char_pt, ahhed * head_pt)
{
    int             org,
                    in,
                    err,
                    diff;

    err = 0;

/* find length of log array and input array  */

    org = strlen (head_pt->record.log);/* log array */
    in = strlen (char_pt);	       /* input array */

/* check for a terminating ':' in the input array */

    if (*(char_pt + in - 1) != ';') {  /* no semicolon----add it */
	err = (-1);
	*(char_pt + in) = ';';
	*(char_pt + in + 1) = '\0';
	in += 1;
    }
/* check the length of the input array */

    if (in > LOGENT) {		       /* entry length too long-----truncate it */
	err = (-2);
	*(char_pt + LOGENT - 1) = ';';
	*(char_pt + LOGENT) = '\0';
	in = LOGENT;
    }
/* check combined length of array and new input and add it */

    diff = LOGSIZE - (org + in);
    if (diff == -in)
	return (-3);		       /* no room left in log array */
    if (diff < 0)
	diff *= (-1), err = (-3);      /* partial room left----use it */
    strncat (head_pt->record.log, char_pt, diff);	/* cat two strings */

    return (err);
}



/*	out_is_tty
 *		determines whether stdout is being sent to screen.
 *	returns:
 *			0	->	stdout is not tty
 *			1	->	stdout is tty
 */
int 
out_is_tty (void)
{

    if (isatty (1)) {		       /* sun specific --- stdout */
	ah_errno = AE_TTYOUT;
	return (1);
    }
    return (0);
}


/*	in_is_tty
 *		determines whether stdin is tty
 *	returns:
 *			0	->	stdin is not tty
 *			1	->	stdin is tty
 */
int 
in_is_tty (void)
{

    if (isatty (0)) {		       /* sun specific --- stdin */
	ah_errno = AE_TTYIN;
	return (1);
    }
    return (0);
}


/*	mkdatspace
 *		allocates enough space for the data array, and
 *		returns a pointer to the memory location, or
 *		NULL if failure.
 *	returns:
 *			character pointer	->	success
 *			NULL			->	failure
 */
char           *
mkdatspace (ahhed * head)
{
    return (calloc ((unsigned) head->record.ndata, (unsigned) size (head)));
}



void
get_null_head (ahhed * hed)
{
    int             i;

    strcpy (hed->station.code, "null");
    strcpy (hed->station.chan, "null");
    strcpy (hed->station.stype, "null");
    hed->station.slat = 0.0;
    hed->station.slon = 0.0;
    hed->station.elev = 0.0;
    hed->station.DS = 0.0;
    hed->station.A0 = 0.0;
    for (i = 0; i < NOCALPTS; ++i) {
	hed->station.cal[i].pole.r = 0.0;
	hed->station.cal[i].pole.i = 0.0;
	hed->station.cal[i].zero.r = 0.0;
	hed->station.cal[i].zero.i = 0.0;
    }

    hed->event.lat = 0.0;
    hed->event.lon = 0.0;
    hed->event.dep = 0.0;
    hed->event.ot.yr = (short) 0;
    hed->event.ot.mo = (short) 0;
    hed->event.ot.day = (short) 0;
    hed->event.ot.hr = (short) 0;
    hed->event.ot.mn = (short) 0;
    hed->event.ot.sec = 0.0;
    strcpy (hed->event.ecomment, "null");

    hed->record.type = (short) 0;
    hed->record.ndata = 0L;
    hed->record.delta = 0.0;
    hed->record.maxamp = 0.0;
    hed->record.abstime.yr = (short) 0;
    hed->record.abstime.mo = (short) 0;
    hed->record.abstime.day = (short) 0;
    hed->record.abstime.hr = (short) 0;
    hed->record.abstime.mn = (short) 0;
    hed->record.abstime.sec = 0.0;
    hed->record.rmin = 0.0;
    strcpy (hed->record.rcomment, "null");
    strcpy (hed->record.log, "null");

    for (i = 0; i < NEXTRAS; ++i)
	hed->extra[i] = 0.0;

    return;
}

/* acpy(from,to,nbytes) copies nbytes from the array "from" to the
 *	array "to".
 */
void
acpy (char *from, char *to, unsigned int nbytes)
{
    while (nbytes--)
	*from++ = *to++;
    return;
}


void
ah_error (char *s1, char *s2, int status)
{				       /* print ah format error message and die */
    extern char    *progname;

    if (progname)
	fprintf (stderr, "%s: ", progname);
    fprintf (stderr, s1, s2);
    if (ah_errno > 0 && ah_errno < ah_nerr)
	fprintf (stderr, " (%s)", ah_errlist[ah_errno]);
    fprintf (stderr, "\n");
    exit (status);
}

#define	MAX(a,b)	(((a) > (b)) ? (a) : (b))
#define	MIN(a,b)	(((a) < (b)) ? (a) : (b))

/*
 *	maxamp
 *		determines the maximum absolute amplitude of the data array, and
 *		places that number in head.record.maxamp.
 *	returns:
 *			0	->	ok
 *			-1	->	error
 */
int 
maxamp (ahhed * head, char *data)
{
    float          *fpt;
    double         *dpt,
                    dmin,
                    dmax;
    float           max,
                    min;
    int             n_data_pts;

    switch (head->record.type) {
      case FLOAT:
	n_data_pts = head->record.ndata;
	break;
      case COMPLEX:
      case VECTOR:
	n_data_pts = 2 * head->record.ndata;
	break;
      case TENSOR:
	n_data_pts = 3 * head->record.ndata;
	break;
      case 5:
	n_data_pts = 4 * head->record.ndata;
	break;
      case DOUBLE:
	n_data_pts = head->record.ndata;
	break;
      default:
	ah_errno = AE_DTYPE;
	return (-1);
	break;
    }

    if (head->record.type == DOUBLE) {
	dpt = (double *) data;
	dmax = dmin = *dpt;
	while (n_data_pts--) {
	    dmax = MAX (dmax, *dpt);
	    dmin = MIN (dmin, *dpt);
	    ++dpt;
	}
	((fabs (dmax) > fabs (dmin)) ? (head->record.maxamp = (float) dmax) : (head->record.maxamp = (float) -dmin));
    } else {
	fpt = (float *) data;
	max = min = *fpt;
	while (n_data_pts--) {
	    max = MAX (max, *fpt);
	    min = MIN (min, *fpt);
	    ++fpt;
	}
	((fabs ((double) max) > fabs ((double) min)) ? (head->record.maxamp = max) : (head->record.maxamp = -min));
    }

    return (0);
}

/*	xdr_gethead
 *		gets the next header from the xdr stream pointed to by
 *		xdrs and returns this header in the structure head.
 *		xdrs is assumed to be positioned at the next header,
 *		and does not search.
 *	returns:
 *			1		->	no error
 *			-1		->	not enough head to read
 *			-2		->	bad data type
 */
int 
xdr_gethead (ahhed * head, XDR * xdrs)
{
    int             ierr = 0;

    if ((ierr = xdr_ahhead (xdrs, head)) == 1) {
	if ((head->record.type < TYPEMIN) || (head->record.type > TYPEMAX)) {
	    get_null_head (head);
	    ierr = -2;		       /* bad data type */
	    ah_errno = AE_DTYPE;
	}
    } else {			       /* not enough head */
	get_null_head (head);
	ierr = -1;
	ah_errno = AE_RHED;
    }
    return (ierr);
}



/*	xdr_puthead
 *		writes the header head onto the xdr stream pointed to by
 *		xdrs.
 *	returns:
 *			1		->	no error
 *			-1		->	error writing header
 */
int 
xdr_puthead (ahhed * head, XDR * xdrs)
{
    int             ierr = 0;

    if ((ierr = xdr_ahhead (xdrs, head)) != 1) {
	ah_errno = AE_WHED;
	ierr = -1;
    }
    return (ierr);
}



/*	xdr_tohead
 *		positions the read/write head to the beginning of the
 *		n-th header in the xdr stream pointed to by xdrs.
 *	returns:
 *			n	->	no error
 *			-1	->	not enough heads
 *			-2	->	bad seek
 */
int 
xdr_tohead (int n, XDR * xdrs)
{
    ahhed           head;
    int             i,
                    ierr,
                    j;
    float           float_dum;
    double          double_dum;
    complex         complex_dum;
    tensor          tensor_dum;

/* be warned: the following xdr_setpos call may not work at all 	*/
/* depending on the stream.  The use of 0 to get to the beginning 	*/
/* works empirically, but is not documented  ... sigh	- dws		*/
    xdr_setpos (xdrs, (u_int) 0);

    for (i = 1; i < n; ++i) {
	if (xdr_gethead (&head, xdrs) == 1) {
	    switch (head.record.type) {
	      case FLOAT:
		for (j = 0; j < head.record.ndata; j++) {
		    if (!xdr_float (xdrs, &float_dum)) {
			ierr = -2;     /* bad seek */
			ah_errno = AE_RHED;
			return (ierr);
		    }
		}

		break;
	      case COMPLEX:
	      case VECTOR:
		for (j = 0; j < head.record.ndata; j++) {
		    if (!xdr_float (xdrs, &complex_dum.i) ||
			    !xdr_float (xdrs, &complex_dum.r)) {
			ierr = -2;     /* bad seek */
			ah_errno = AE_RHED;
			return (ierr);
		    }
		}

		break;
	      case TENSOR:
		for (j = 0; j < head.record.ndata; j++) {
		    if (!xdr_float (xdrs, &tensor_dum.xx) ||
			    !xdr_float (xdrs, &tensor_dum.yy) ||
			    !xdr_float (xdrs, &tensor_dum.xy)) {
			ierr = -2;     /* bad seek */
			ah_errno = AE_RHED;
			return (ierr);
		    }
		}
		break;
	      case 5:
		for (j = 0; j < 4 * head.record.ndata; j++) {
		    if (!xdr_float (xdrs, &float_dum)) {
			ierr = -2;     /* bad seek */
			ah_errno = AE_RHED;
			return (ierr);
		    }
		}
		break;
	      case DOUBLE:
		for (j = 0; j < head.record.ndata; j++) {
		    if (!xdr_double (xdrs, &double_dum)) {
			ierr = -2;     /* bad seek */
			ah_errno = AE_RHED;
			return (ierr);
		    }
		}
		break;
	      default:
		ierr = -2;	       /* bad seek */
		ah_errno = AE_DTYPE;
		return (ierr);
	    }
	} else {
	    ierr = -1;		       /* not enough head */
	    ah_errno = AE_RHED;
	    return (ierr);
	}
    }
    return (i);			       /* success */
}



/*	xdr_getdata
 *		reads from the xdr stream pointed to by xdrs into
 *		the array pointed to by array.  It assumes that
 *		the read/write head is positioned correctly
 *		(i.e., right after the header), and does not
 *		search.  Works for any allowed data type.
 *	returns:
 *			number of elements read	->	OK
 *			-1			->	error
 */
int 
xdr_getdata (ahhed * head, char *array, XDR * xdrs)
{
    int             ierr = 0;
    float          *pfloat;
    double         *pdouble;
    complex        *pcomplex;
    tensor         *ptensor;
    int             i;

    switch (head->record.type) {
      case FLOAT:
	pfloat = (float *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_float (xdrs, pfloat++)) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case COMPLEX:
      case VECTOR:
	pcomplex = (complex *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_float (xdrs, &(pcomplex->r)) ||
		    !xdr_float (xdrs, &(pcomplex++->i))) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case TENSOR:
	ptensor = (tensor *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_float (xdrs, &(ptensor->xx)) ||
		    !xdr_float (xdrs, &(ptensor->yy)) ||
		    !xdr_float (xdrs, &(ptensor++->xy))) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case 5:
	pfloat = (float *) array;
	for (i = 0; i < 4 * head->record.ndata; i++) {
	    if (!xdr_float (xdrs, pfloat++)) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case DOUBLE:
	pdouble = (double *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_double (xdrs, pdouble++)) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      default:
	ierr = -1;
	ah_errno = AE_DTYPE;
	return (ierr);
    }
    return (ierr);
}


/*	xdr_putdata
 *		writes array to the xdr stream pointed to by xdrs.
 *		Works for any allowed data type.
 *	returns:
 *			number of elements written	->	OK
 *			-1			->	error
 */
int 
xdr_putdata (ahhed * head, char *array, XDR * xdrs)
{
    int             ierr = 0;
    float          *pfloat;
    double         *pdouble;
    complex        *pcomplex;
    tensor         *ptensor;
    int             i;

    switch (head->record.type) {
      case FLOAT:
	pfloat = (float *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_float (xdrs, pfloat++)) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case COMPLEX:
      case VECTOR:
	pcomplex = (complex *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_float (xdrs, &(pcomplex->r)) ||
		    !xdr_float (xdrs, &(pcomplex++->i))) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case TENSOR:
	ptensor = (tensor *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_float (xdrs, &(ptensor->xx)) ||
		    !xdr_float (xdrs, &(ptensor->yy)) ||
		    !xdr_float (xdrs, &(ptensor++->xy))) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case 5:
	pfloat = (float *) array;
	for (i = 0; i < 4 * head->record.ndata; i++) {
	    if (!xdr_float (xdrs, pfloat++)) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;
      case DOUBLE:
	pdouble = (double *) array;
	for (i = 0; i < head->record.ndata; i++) {
	    if (!xdr_double (xdrs, pdouble++)) {
		ah_errno = AE_RDATA;
		ierr = -1;
		return (ierr);
	    }
	    ++ierr;
	}
	break;

      default:
	ierr = -1;
	ah_errno = AE_DTYPE;
	return (ierr);
    }
    return (ierr);
}


/*	xdr_putrecord
 *		writes head and array to the xdr stream pointed to by xdrs.
 *		Works for any allowed data type.
 *	returns:
 *			0	->	OK
 *			-1	->	error writing header
 *			-2	->	error writing data
 */
int 
xdr_putrecord (ahhed * head, char *array, XDR * xdrs)
{
    int             ierr = 0;

    (xdr_puthead (head, xdrs) == 1) ? ((xdr_putdata (head, array, xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if (ierr)
	ah_errno = AE_WRECORD;

    return (ierr);
}


/*	xdr_getrecord
 *		gets header and data from the xdr stream pointed to by
 *		xdrs and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search.  Obviously, calling
 *		routine must have allocated enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	error reading header
 *			-2	->	error reading data
 */
int 
xdr_getrecord (ahhed * head, char *array, XDR * xdrs)
{
    int             ierr = 0;

    (xdr_gethead (head, xdrs) == 1) ? ((xdr_getdata (head, array, xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if (ierr)
	ah_errno = AE_RRECORD;
    return (ierr);
}

/*
 *	xdr_getrecord2
 *		gets header and data from the xdr stream pointed to by
 *		xdrs and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search (although it does
 *		some error checking).  Space for array is allocated, so
 *		be sure to pass a pointer to the data pointer. Got it?
 *	returns:
 *			0	->	ok
 *			-1	->	error reading record
 *			-2	->	error allocating space for data
 */
int 
xdr_getrecord2 (ahhed * head, char **array, XDR * xdrs)
{
    int             ierr = 0;
    int             xdr_gethead (ahhed * head, XDR * xdrs);
    char           *mkdatspace (ahhed * head);

    if (xdr_gethead (head, xdrs) != 1) {
	ierr = -1;
	return (ierr);
    }
    *array = mkdatspace (head);
    if (*array == NULL) {
	ierr = -2;
	return (ierr);
    }
    if (xdr_getdata (head, *array, xdrs) < 0)
	ierr = -1;

    return (ierr);
}


/*	xdr_gogethead
 *		gets n-th header from the xdr stream pointed to by
 *		xdrs and returns this header in the structure
 *		head.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading header
 */
int 
xdr_gogethead (int n, ahhed * head, XDR * xdrs)
{
    int             ierr = 0;

    (xdr_tohead (n, xdrs) == n) ? ((xdr_gethead (head, xdrs) < 1) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return (ierr);
}


/*	xdr_gogetrecord
 *		gets n-th record (header and data) from the xdr stream
 *		pointed to by xdrs and places it in head and array.
 *		Calling routine must allocate enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading record
 */
int 
xdr_gogetrecord (int n, ahhed * head, char *array, XDR * xdrs)
{
    int             ierr = 0;

    (xdr_tohead (n, xdrs) == n) ? ((xdr_getrecord (head, array, xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return (ierr);
}






int
xdr_ahhead (XDR * xdrsp, ahhed * ahheadp)
{
    u_int           l;
    char          **pp,
                   *p;
    float         **ppf,
                   *pf;

    l = CODESIZE;
    p = ahheadp->station.code;
    pp = &p;
    if (!xdr_bytes (xdrsp, pp, &l, (u_int) CODESIZE))
	return (0);
    l = CHANSIZE;
    p = ahheadp->station.chan;
    pp = &p;
    if (!xdr_bytes (xdrsp, pp, &l, CHANSIZE))
	return (0);
    l = STYPESIZE;
    p = ahheadp->station.stype;
    pp = &p;
    if (!xdr_bytes (xdrsp, pp, &l, STYPESIZE))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->station.slat))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->station.slon))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->station.elev))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->station.DS))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->station.A0))
	return (0);
    for (l = 0; l < NOCALPTS; l++) {
	if (!xdr_float (xdrsp, &ahheadp->station.cal[l].pole.r))
	    return (0);
	if (!xdr_float (xdrsp, &ahheadp->station.cal[l].pole.i))
	    return (0);
	if (!xdr_float (xdrsp, &ahheadp->station.cal[l].zero.r))
	    return (0);
	if (!xdr_float (xdrsp, &ahheadp->station.cal[l].zero.i))
	    return (0);
    }
    if (!xdr_float (xdrsp, &ahheadp->event.lat))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->event.lon))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->event.dep))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->event.ot.yr))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->event.ot.mo))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->event.ot.day))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->event.ot.hr))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->event.ot.mn))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->event.ot.sec))
	return (0);
    l = COMSIZE;
    p = ahheadp->event.ecomment;
    pp = &p;
    if (!xdr_bytes (xdrsp, pp, &l, COMSIZE))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->record.type))
	return (0);
    /* replaced xdr_long with xdr_int here, so that ndata stays at 32 bits */
    if (!xdr_int (xdrsp, &ahheadp->record.ndata))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->record.delta))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->record.maxamp))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->record.abstime.yr))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->record.abstime.mo))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->record.abstime.day))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->record.abstime.hr))
	return (0);
    if (!xdr_short (xdrsp, &ahheadp->record.abstime.mn))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->record.abstime.sec))
	return (0);
    if (!xdr_float (xdrsp, &ahheadp->record.rmin))
	return (0);
    l = COMSIZE;
    p = ahheadp->record.rcomment;
    pp = &p;
    if (!xdr_bytes (xdrsp, pp, &l, COMSIZE))
	return (0);
    l = LOGSIZE;
    p = ahheadp->record.log;
    pp = &p;
    if (!xdr_bytes (xdrsp, pp, &l, LOGSIZE))
	return (0);
    l = NEXTRAS;
    pf = ahheadp->extra;
    ppf = &pf;
    if (!xdr_array (xdrsp, (char **) ppf, &l, NEXTRAS, sizeof (float),
		    (xdrproc_t) xdr_float))
	return (0);


    return (1);
}
