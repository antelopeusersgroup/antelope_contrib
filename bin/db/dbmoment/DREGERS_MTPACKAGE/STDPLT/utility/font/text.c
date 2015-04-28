#define MAXFONTS	10

struct fontheader
   {
	short magic;
	unsigned short size;
	short fontsize;
	short fontribs;
	short fontname;
   }; 

struct charhead
   {
	unsigned short addr;
	short nvec;
	char up,down,left,right;
	short width;
   };

struct charhead
   {
	char vpos;
	char hpos1;
	char hpos2;
   };

struct loadedfonts
   {
	int fontkey;
	int fontuse;
	struct charhead *ch;
	struct chardata *cd;
   }	loadedfonts[MAXFONTS];

gethgtwid(hgtup,hgtdn,wid,fmt,a1,a2,a3,a4,a5,a6)
int *hgtup, *hgtdn, *wid, a1,a2,a3,a4,a5,a6;
char *fmt;
   {
	int tempsize, tempcolor, temptype;
	int xpt, ypt, arg;
	struct fontinfo *pfont, *loadfont();
	struct dispatch *disp;
	char *cptr, *charbits, c;

	*wid= *hgtup= *hgtdn= 0;
	tempsize=  txt.textsize;
	tempcolor= txt.textcolor;
	temptype=  txt.texttype;
	pfont= loadfont(tempsize,temptype);
	sprintf(linebuf,fmt,a1,a2,a3,a4,a5,a6);
	cptr= linebuf;
	while( (c= *cptr++) != '\0' )
	   {
		if(c == MAC)
		   {
			if( (c= *cptr++) == '\0' ) break;
			if(c == MAC) goto bypass;
			if(cptr[0] != '(')
			   {
				fprintf(stderr,"qtext: '('-error mac=%c\n",c);
				break;
			   }
			arg= atol(&cptr[1]);
			while(*cptr && *cptr != ')') cptr++;
			if( *cptr != ')')
			   {
				fprintf(stderr,"qtext: ')'-error mac=%c\n",c);
				break;
			   }
			cptr++;
			switch(c)
			   {
				case 'f': temptype= arg; break;
				case 's': tempsize= arg; break;
				case 'c': tempcolor=arg; break;
				default:
					fprintf(stderr,"bad macro mac=%c\n",c);
					goto bypass;
					break;
			   }
			if(cptr[1] == MAC) continue;
			pfont= loadfont(tempsize,temptype);
		   }
		 else
		   {
		
		bypass:

			disp= & pfont->fontdisp[(int)(c)&0x7f];
			*wid += disp->width;
			if(disp->up   > *hgtup) *hgtup= disp->up;
			if(disp->down > *hgtdn) *hgtdn= disp->down;
			pfont->fontuse++;
		   }
	   }
   }


do_char(xp,yp,ch,cd)
int *xp, *yp;
struct charhead *ch;
struct chardata *cd;
   {
	float fxbase, fybase, fxpt1, fypt1, fxpt2, fypt2, S, C;

	C= txt.Cosangle;
	S= txt.Sinangle;
	fxbase= *xp - C*disp->left -S*disp->up;
	fybase= *yp - S*disp->left +C*disp->up;
	*xp += C*disp->width;
	*yp += S*disp->width;
	for(i=0; i < ch->nvec; i++)
	   {
		fxpt1= fxbase + C* cd[i].hpos1 -S* cd[i].vpos;
		fypt1= fybase + S* cd[i].hpos1 +C* cd[i].vpos;
		fxpt2= fxbase + C* cd[i].hpos2 -S* cd[i].vpos;
		fypt2= fybase + S* cd[i].hpos2 +C* cd[i].vpos;
		do_vector((int)(fxpt1),(int)(fypt1),(int)(fxpt2),(int)(fypt2));
	   }
   }

char fdir[]	="/usr/lib/vfont";

struct fontinfo *loadfont(size,type)
int size, type;
   {
	int fontdes, i, imin, minuse, iuse;
	int fd;
	char fname[40];
	struct fontinfo *ptr;
	static oldfontdes = -1;
	static struct fontinfo *oldfontptr=0;
	struct header header;

	fontdes= size | (type <<8);
	if(fontdes == oldfontdes) return(oldfontptr);
	for(i= 0; i<txt.nfont; i++)
	   {
		ptr= fonts +i;
		if(ptr->fontdes == fontdes)
		   {
			oldfontptr= ptr;
			oldfontdes= fontdes;
			return(ptr);
		   }
	   }
	if(txt.nfont >= NFONTMAX)
	   {
		minuse= fonts[0].fontuse;
		imin= 0;
		for(i=1; i<txt.nfont; i++)
		   {
			ptr= fonts +i;
			if(ptr->fontuse < minuse)
			   {
				minuse= ptr->fontuse;
				imin= i;
			   }
		   }
		fprintf(stderr,"font overload fontuse=%d\n",minuse);
		iuse= imin;
		free(fonts[iuse].fontdisp);
	   }
	 else
	   {
		iuse= txt.nfont;
		txt.nfont++;
	   }
	ptr= &fonts[iuse];
	ptr->fontdes= fontdes;
	ptr->fontuse= 0;
	switch(type)
	   {
		case 1:
			sprintf(fname,"%s/R.%d",fdir,size);
			break;
		case 2:
			sprintf(fname,"%s/I.%d",fdir,size);
			break;
		case 3:
			sprintf(fname,"%s/B.%d",fdir,size);
			break;
		case 4:
			sprintf(fname,"%s/S.%d",fdir,size);
			break;
	   }
	if( (fd= open(fname,0)) < 0)
	   {
		fprintf(stderr,"cannot open %s\n",fname);
		exit();
	   }
	read(fd,&header,10);
	if( header.magic != 0436)
	   {
		fprintf(stderr,"bad header in %s\n",fname);
		exit();
	   }
	ptr->fontdisp= (struct dispatch *)malloc(128*10 + header.size);
	if(ptr->fontdisp == NULL)
	   {
		fprintf(stderr,"cannot allocate memory in load\n");
		exit();
	   }
	read(fd,ptr->fontdisp,128*10);
	ptr->fontbits= (char *)(ptr->fontdisp + 128);
	lseek(fd,10 + 256*10,0);
	read(fd,ptr->fontbits,header.size);
	ptr->fontdisp[(int)(' ')].width= ptr->fontdisp[(int)('i')].width;
	close(fd);
	return(ptr);
   }
