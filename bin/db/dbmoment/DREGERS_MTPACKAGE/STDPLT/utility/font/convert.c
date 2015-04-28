#include	<stdio.h>
struct fonthead
   {
	short	magic;
	unsigned short size;
	short	maxx;
	short	maxy;
	short	xtnd;
   } fh;

struct charhead
   {
	unsigned short addr;
	short nbytes;
	char up;
	char dn;
	char lt;
	char rt;
	short width;
   }	ch[256];

struct scanline
   {
	char v;
	char h1;
	char h2;
   }	vecs[10000];
struct output
   {
	unsigned char hpos;
	unsigned char cnt;
   }	out[200];
#define VINC	0x80

char buf[64000];
char bits[8] = { 0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 0x2, 0x1 };

main()
   {
	int i;
	int ncol, nrow, ncolb, nrowb, ir, ic;
	char *base, *p;
	char cpr;
	int nvec, lookfor;
	int bitsize, vecsize;

	bitsize= vecsize= 0;
	if(read(0,&fh,10) != 10)
	   {
		fprintf(stderr,"cannot read input\n");
		exit(-1);
	   }
	if(fh.magic != 0436)
	   {
		fprintf(stderr,"bad magic\n");
		exit(-1);
	   }
	if(read(0,ch,256*10) != 256*10)
	   {
		fprintf(stderr,"bad read in char heads\n");
		exit(-1);
	   }
	if(read(0,buf,fh.size) != fh.size)
	   {
		fprintf(stderr,"bad read in char bits\n");
		exit(-1);
	   }
	fprintf(stderr,"size= %d\n",fh.size);
	for(i=0; i<256; i++)
	   {
		if(ch[i].nbytes == 0) continue;
		ncol= (ch[i].lt + ch[i].rt);
		nrow= (ch[i].up + ch[i].dn);
		ncolb= (ncol + 7)/8;
		nrowb= (nrow + 7)/8;
		if(nrow*ncolb != ch[i].nbytes)
		   {
			fprintf(stderr,"size mismatch want=%d got=%d\n",ncolb*nrow,ch[i].nbytes);
			exit(-1);
		    }
		bitsize += ch[i].nbytes;
		base= buf + ch[i].addr;
		nvec= 0;
		for(ir=0; ir<nrow; ir++)
		   {
			p= base + ir*ncolb;
			lookfor= 1;
			for(ic=0; ic<ncol; ic++)
			   {
				if(p[ic/8] & bits[ic%8])
				   {
					if(lookfor == 0) continue;
					vecs[nvec].v= ir;
					vecs[nvec].h1= ic;
					lookfor= 0;
				   }
				 else
				   {
					if(lookfor == 1) continue;
					vecs[nvec].h2= ic-1;
					nvec++;
					lookfor= 1;
				   }
				/*
				if(p[ic/8] & bits[ic%8]) putc('X',stderr);
				 else putc(' ',stderr);
				 */
			   }
			if(lookfor == 0)
			   {
				vecs[nvec].h2= ncol-1;
				nvec++;
			   }
			/*putc('\n',stderr);*/
		   }
		if(nvec == 0)
		   {
			out[0].hpos= 0;
		vecsize += nvec;
		cpr= (i>= 040 && i< 0177 ? i : '?');
		/*
		fprintf(stderr,"i=%3d (%c): nb=%3d nvs=%4d up=%3d dn=%3d lt=%3d rt=%3d wid=%3d\n",i,cpr,
			ch[i].nbytes,
			nvec*3,
			ch[i].up,
			ch[i].dn,
			ch[i].lt,
			ch[i].rt,
			ch[i].width);
		*/
	   }
	fprintf(stdout,"bitsize=%5d  vecsize1= %5d  vecsize2= %5d\n",
		bitsize,vecsize*2,vecsize*3);
	fprintf(stdout,"%%1=%4.1f   %%2=%4.1f\n",
		100.0* (float)(2*vecsize)/(float)(bitsize),
		100.0* (float)(3*vecsize)/(float)(bitsize));
   }
