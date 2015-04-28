/*
program to window portion of 3-D data file

window len= [in= out= planes= vecs= esize= p0= v0= e0= np= nv= ne= dp= dv= de=]

arguments:
	in=stdin	input file, a series of vectors
	out=stdout	windowed result
	len=(nt=)	length of input file vector
	vecs=(nx=)	number of vectors in a plane
	planes=1	number of planes in dataset
	esize=4		size of input file element in bytes
	p0=0		the first plane to be copied
	v0=0		the first vector in each plane to be copied
	e0=0		the first element of each vector to be copied
	np=(planes-p0)/dp	the number of planes to be copied
	nv=(vecs-v0)/dv	the number of vectors to be copied
	ne=(len-e0)/de	the number of elements from each vector to be copied
	dp=1		increment between input planes
	dv=1		increment between input vectors
	de=1		increment between input elements
#
to compile: cc window -o window -lget
*/
#include <stdio.h>
#include <fcntl.h>

char in[40], out[40];
main (argc,argv)
int argc; char **argv;
	{
	int rfile,wfile;
	int len,esize=4;
	int v0=0,e0=0,nv=0,ne=0,dv=1,de=1;
	int nseek,de1,i,j,k,planes=1,vecs=0,np=0,dp=1,nseek1,m,p0=0;
	char *x;

	/* fetch parameters */
	setpar(argc,argv);
	if (getpar("in","s",in)) {
		if ((rfile = open(in,O_RDONLY,0644)) < 2) {
			fprintf(stderr,"cannot open %s\n",in);
			exit(-1);
		}
	}
	else rfile=0;
	if (getpar("out","s",out)) {
		if ((wfile = open(out,O_WRONLY | O_CREAT | O_TRUNC,0644)) < 2) {
			fprintf(stderr,"cannot open %s\n",out);
			exit(-1);
		}
	}
	else wfile=1;
	if (getpar("len","d",&len)==0)
		if (getpar("nt","d",&len)==0) fprintf(stderr,"len= missing\n");
	getpar("planes","d",&planes);
	getpar("esize","d",&esize);
	if (getpar("vecs","d",&vecs)==0) mstpar("nx","d",&vecs);
	getpar("p0","d",&p0);
	getpar("v0","d",&v0);
	getpar("e0","d",&e0);
	getpar("dp","d",&dp);
	getpar("dv","d",&dv);
	getpar("de","d",&de);
	if (getpar("np","d",&np)==0) np = (planes - p0) / dp;
	if (getpar("nv","d",&nv)==0) nv = (vecs - v0) / dv;
	if (getpar("ne","d",&ne)==0) ne = (len - e0) / de;
	endpar();
	fprintf(stderr,"  in=%s out=%s\n",in,out);
	fprintf(stderr,"  planes=%d vecs=%d len=%d esize=%d\n",planes,vecs,len,esize);
	fprintf(stderr,"  np=%d p0=%d dp=%d\n",np,p0,dp);
	fprintf(stderr,"  nv=%d v0=%d dv=%d\n",nv,v0,dv);
	fprintf(stderr,"  ne=%d e0=%d de=%d\n",ne,e0,de);
	
	x = (char *) malloc(ne*de*esize);
	lseek (rfile,((p0*vecs+v0)*len+e0)*esize,0);
	nseek = ((dv - 1) * len + (len - ne * de)) * esize;
	nseek1 = ((dp - 1) * vecs + (vecs - nv * dv)) * len * esize;
	ne *= esize;
	de1 = (de - 1) * esize;
	while (np-->0) {
		for (m=0; m<nv; m++) {
			read (rfile,x,ne*de);
			if (de>1) for (i=j=0; i<ne; j+=de1)
				for (k=i+esize; i<k;) x[i++] = x[j++];
			dowrite (wfile,x,ne);
			lseek (rfile,nseek,1);
		}
		lseek (rfile,nseek1,1);
	}
}

int
dowrite(fd,buf,bytes)
int fd, bytes;
char *buf;
{
	int nwrite, total;

	if ((total = write(fd,buf,bytes)) <= 0) return(total);
	buf += total;
	while (bytes > total) {
		if ((nwrite = write(fd,buf,bytes-total)) <= 0) return(total);
		total += nwrite;
		buf += nwrite;
	}
	return(total);
}
