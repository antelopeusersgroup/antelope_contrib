/* Routine to read Helmberger Format Seismograms and to return */
/* information about number of traces, number of time points,  */
/* dt, and the data vector                                     */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <fcntl.h>

#define TRUE 1
#define FALSE 0

char form[32]     = "(6e12.5)";
FILE *inf,*of;

/*
 * readhelm: 'in' is the file to read, in ASCII helmberger format
 *            *N is the number of trace sectors in the file
 *            *NT is an array of the number of points in each of the
 *        N sectors.
 *            *DT is an array of the time steps in each sector.
 *            *vec1 is an array of the trace values, the *NT[0] values
 *        first, then the *NT[1] values, up to *NT[*N-1] values.
 * On input, if *N is non-zero, then it is set to the number of
 *   sectors already allocated in NT, DT, and vec1. In that case,
 *   *NT must contain the expected trace lenghts (all the same) and
 *   *vec1 must be allocated of size *N * *NT[0] * sizeof(float).
 * readhelm exits if any errors are encountered.
 */

readhelm(in,N,NT,DT,vec1)
     char *in;
     int *N, **NT;
     float **DT;
     float **vec1;
{
    int nt, orignt, nx,nxx, perline, i, j, cnt, left, f_width, ss;
    int nsect, vsize, *pNT;
    float dt, *pv, dd, tt, *pDT;
    char c_form[512],c_form1[128],c_form2[128];
    char line[128];

    if ( (inf=fopen(in,"r")) == NULL) {
	fprintf(stderr, "Error opening %s; quiting\n", in);
	exit(1);
    }
    nsect = *N;

    /* read initial header */
    fgets(line,100,inf);
    sscanf(line,"%d",&nxx);
    *N=nxx; /* number of traces in the file */
    if (nsect == 0) {
	*NT = (int *)calloc( nxx, sizeof(int));
	*DT = (float *)calloc( nxx, sizeof(float));
    }
    else if (nxx > nsect) {
	fprintf(stderr, "%s holds %d sectors; %d expected; quiting\n",
		in, nxx, nsect);
	exit(1);
    }
    pNT = *NT;
    pDT = *DT;
    
    fgets(form,100,inf);
    perline = chkform(form,c_form1,c_form2,&f_width);
    /*fprintf(stderr,"perline=%d  form=%s\n",perline,form);*/

    nx=nxx;
    while (nx) {
	fgets(line,100,inf);
	sscanf(line,"%f %f",&dd,&tt);
	fgets(line,100,inf);
	sscanf(line,"%d %f",&nt,&dt);
		
	if (nsect != 0) {
	    vsize = *pNT;
	    if (nt > vsize) {
		fprintf(stderr, "trace in %s is %d points, expected %d\n",
			in, nt, vsize);
		exit(1);
	    }
	}
	*pNT++ = nt;
	*pDT++ = dt;
	/*   fprintf(stderr,"nt=%d dt=%f\n",nt,dt);*/

	if(nx == nxx) {   /* first time through */
	    orignt = nt;
	    if (nsect == 0)
		*vec1 = (float *) malloc(nt * nxx * sizeof(float));
	    pv = *vec1;
	}
	else {
	    if (nt > orignt) {
		fprintf (stderr, "first sector of %s is not largest\n", in);
		exit(1);
	    }
	}

	cnt  = nt / perline;
	left = nt % perline;
	for (i=0;i<cnt;i++) {
	    for (j=0;j<perline;j++)
		ss=fscanf(inf,c_form2,pv++);

	    fgets(line,100,inf);
	}

	if ( left != 0) {
	    for (j=0;j<left;j++)
		fscanf(inf,c_form2,pv++);
	    fgets(line,100,inf);
	}

	nx--;

    } /*end while */
    fclose(inf);
    /*   fprintf(stderr,"Finished Read\n");*/
    return;
}


/*Subroutines*/
char x_form[64];

int
chkform(f_form,c_form1,c_form2,f_width)
     char *f_form, *c_form1, *c_form2;
     int *f_width;
{
    int i, cnt, not_done, found_num, num, con1, con2;
    char *px_form, *pf_form, *pn;

    px_form = x_form;
    *f_width = 0;

    for(i=0;i<32;i++) if (f_form[i] == '(') break;
    if (i == 32) {
	fprintf(stderr,"readhelm:  Bad format, no '(', continuing...\n");
	*px_form++ = '(';
	i = 0;
    }
    else {
	*px_form++ = f_form[i];
	i++;
    }
    pf_form = &(f_form[i]);
    cnt = 32 - i;
    not_done = TRUE;
    for (i=0;i<cnt && not_done;i++) {
      
	if (isspace(*pf_form)) {
	    pf_form++;
	    fprintf(stderr,"readhelm:  Avoid blanks in format\n");
	}

	else if (isdigit(*pf_form)) {
	    pn = px_form;
	    *px_form++ = *pf_form++;
	    while(isdigit(*pf_form)) {
		*px_form++ = *pf_form++;
		i++;
	    }
	    *px_form = '\0';
	    if ((found_num = sscanf(pn,"%d",&num)) != 1) {
		fprintf(stderr,"readhelm:  unknown format error\n");
		exit(-1);
	    }
	}

	else switch (*pf_form) {
	case 'e':
	case 'E':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
		fprintf(stderr,"readhelm:  e-format error\n");
		exit(-1);
            }
            if (con1 < con2 + 6)
            {
		fprintf(stderr,"readhelm:  e-format error\n");
		exit(-1);
            }
            if (con1 == con2 + 6)
            {
		fprintf(stderr,"readhelm:  e-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%de",con1,con2);
            sprintf(c_form2,"%%%df",con1);
            sprintf(px_form,"%d.%d)",con1,con2);
            not_done = FALSE;
            break;
         
	case 'f':
	case 'F':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
		fprintf(stderr,"readhelm:  f-format error\n");
		exit(-1);
            }
            if (con1 < con2 + 2)
            {
		fprintf(stderr,"readhelm:  f-format error\n");
		exit(-1);
            }
            if (con1 == con2 + 2)
            {
		fprintf(stderr,"readhelm:  f-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%df",con1,con2);
            sprintf(c_form2,"%%%df",con1);
            sprintf(px_form,"%d.%d)",con1,con2);
	    *f_width = con1;
            not_done = FALSE;
            break;

	case 'g':
	case 'G':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
		fprintf(stderr,"readhelm:  g-format error\n");
		exit(-1);
            }
            if (con1 < con2 + 2)
            {
		fprintf(stderr,"readhelm:  g-format error\n");
		exit(-1);
            }
            if (con1 == con2 + 2)
            {
		fprintf(stderr,"readhelm:  g-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%dg",con1,con2);
            sprintf(c_form2,"%%%df",con1);
            sprintf(px_form,"%d.%d)",con1,con2);
            not_done = FALSE;
            break;
         
	default:
            if (found_num) {
		fprintf(stderr,"readhelm:  unknown format error\n");
		exit(-1);
            }
            fprintf(stderr,"readhelm:  Bad characters in format continuing...\n");
            pf_form++;
            i++;
            break;
	}
    }
    not_done = FALSE;
    for(i=0;i<32;i++) {
	if (f_form[i] == ')') break;
	if (f_form[i] == '\0') not_done = TRUE;
    }
    if (i == 32 || not_done)
	fprintf(stderr,"readhelm:  Bad format, no ')', continuing...\n");
    for (i=0;i<32;i++) f_form[i] = x_form[i];
    if (found_num) return(num);
    return(1);
}

/* read binary-format Green's function file, written by gfa2b */
readbin(in, N,NT,DT,vec1)
     char *in;
     int *N, **NT;
     float **DT;
     float **vec1;
{
    int fd;
    int ns, *nt, nread, toread, ntotal, NTOTAL;
    int nsect, vsize, *pNT;
    float *pv, *pDT;
    struct {
	float dt;
	int nsect;
    } head;
	    
    if ( (fd = open(in, O_RDONLY)) < 0) {
	fprintf(stderr,"Error opening %s\n", in);
	exit(1);
    }
    nsect = *N;

    /* Read the number of sectors; allocate if needed */
    if ( (nread = read(fd, &head, sizeof(head))) < sizeof(head)) {
	fprintf(stderr, "Error reading %s\n", in);
	close(fd);
	exit( 1 );
    }
    *N = head.nsect;
    nt = (int *)calloc( head.nsect, sizeof(int));
    if (nsect == 0) {
	*NT = (int *)calloc( head.nsect, sizeof(int));
	*DT = (float *)calloc( head.nsect, sizeof(float));
    }
    else if (head.nsect > nsect) {
	fprintf(stderr, "%s holds %d sectors; %d expected; quiting\n",
		in, head.nsect, nsect);
	exit(1);
    }
    pNT = *NT;
    pDT = *DT;
    
    /* Read the size of each sector */
    if ( (nread = read(fd, nt, head.nsect * sizeof(int))) < 
	 head.nsect * sizeof(int)) {
	fprintf(stderr, "Error reading %s\n", in);
	close(fd);
	return( -1 );
    }
    
    ntotal = 0;
    NTOTAL = 0;
    for (ns = 0; ns < head.nsect; ns++) {
	*pDT++ = head.dt;
	ntotal += nt[ns];
	NTOTAL += (*NT)[ns];
	pNT[ns] = nt[ns];
    }
    if (nsect == 0) {
	*vec1 = (float *) malloc(ntotal * sizeof(float));
    }
    else if (ntotal > NTOTAL) {
	fprintf(stderr, "GF file too large (%d total points), expected %d\n",
		ntotal, NTOTAL);
	close(fd);
	return( -1 );
    }
    

    pv = *vec1;
    for( ns = 0; ns < head.nsect; ns++) {
	toread = nt[ns] * sizeof(float);
	if ( (nread = read(fd, pv, toread)) < toread) {
	    fprintf(stderr, "Error reading %s\n", in);
	    close(fd);
	    exit( 1 );
	}
	pv += nt[ns];
    }
    
    close(fd);
    free(nt);
    
    return;
}
