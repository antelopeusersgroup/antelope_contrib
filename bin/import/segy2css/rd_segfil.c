/* @(#)rd_segfil.c	1.1 03/12/96  */
/*===========================================================================
 *
 *
 *    sgycss/rd_segfil.c
 *
 *    Read file in SEGY format , make data file with the name YYDDDHHMMSS.w,
 *    as well as wfdisc file: YYDDDHHMMSS.wfdisc.
 *    Parse the header of each SEGY data file (240 bytes) to get information 
 *    about data type, number of the samples, instrument characteristics, etc.
 *
 =========================================================================*/
#include "segcss.h"


int rd_segfil(names,name, param, parnum)
struct name *names;
char *name;
struct conver *param;
int parnum;

  {
        struct stat buf;
	struct data dates;
        int year;
	FILE *fd;
        int bytes, i, j;
        short buffer[BUF_SIZE+1]; 
        char fpath[132];
	static char wfdir_nam[132];
        char fdname[12];
        int snumber;

/*  Open SEGY file, read file header, get parameters  */
    
    if(stat(name, &buf) != 0) {
       fprintf(stderr,"rd_segfil(): 'stat'");
       perror(name);
    }
      /*  Open file  */

    if ((fd = fopen(name,"r")) == NULL) {
        printf("sgycss/rd_segfil(): %s - ", name);
        perror(name);
        return 0;
    }

    if( (bytes = fread(buffer, sizeof(short), BUF_SIZE, fd) ) <= 0)  {
      fprintf(stderr,"sgycss/rd_segfil(): read %d bytes instead of %d\n",bytes,BUF_SIZE);
      return 0;
    }
    
    fclose(fd);

/* Get time information from the header and fill 'datas' structure  */

    dates.yr   = (int) *(buffer + YR_OFF     );
    dates.day  = (int) *(buffer + YR_OFF + 1 );
    dates.hour = (int) *(buffer + YR_OFF + 2 );
    dates.min  = (int) *(buffer + YR_OFF + 3 );
    dates.sec  = (int) *(buffer + YR_OFF + 4 );
    dates.msec = (short) *(buffer + MSEC_OFF);

/* Get instrument information from the header  */

    Channel    = (int) *(buffer + CH_OFF);
    snumber  =   (int) *(buffer + SN_OFF);
    Dformat = (int) *(buffer + DFORMAT);
    if(Dformat == 0) Dformat = 2;
    else if(Dformat == 1) Dformat = 4;
    else {
         printf("segy2css/rd_segfil():Can't determine data format.");
         printf("                    Will consider data is 2 bytes long.");
         Dformat = 2;
    }

/* Get number of data samples and data bytes  */

    Data_bytes = (long) (buf.st_size - 240);
    Sample = (long) (buf.st_size - 240)/Dformat;
    
        
/* Convert serial instrument number to the station name  */

    decode(snumber, &dates, fdname, param, parnum);

/* Put  station name and channel name to the 'names' structure  */

    sprintf(names->sta,"%s\0",fdname);
    sprintf(names->chan,"%d\0",Channel);

    year = dates.yr;

/* Make css3.0 wave form file name and wfdisc file name  */
    
    if(Byevent)
       pathfrname(name, fpath);
    else 
       sprintf(fpath,"%4d%3d%2d%2d%2d.w\0",year,dates.day, dates.hour, dates.min,dates.sec);
    
    for(i = 0; i < strlen(fpath); i++)
      if (fpath[i] == ' ')  fpath[i] = '0'; 

    if (strcmp(wfdir_nam,fpath) != 0 )  {
        sprintf(names->dataf ,"%4d%3d%2d%2d%2d.w\0", year, dates.day, dates.hour, dates.min, dates.sec); 
        sprintf(names->fwd, "%4d%3d%2d%2d%2d.wfdisc\0", year, dates.day, dates.hour, dates.min, dates.sec);
        
        for(i = 0; i < strlen(names->dataf); i++)
          if(names->dataf[i] == ' ') names->dataf[i] = '0';

        for(i = 0; i < strlen(names->fwd); i++)
          if(names->fwd[i] == ' ') names->fwd[i] = '0';

/* Open data file and wfdisc file for output  */

        if(Fp_wfd != NULL) fclose(Fp_wfd);
        if ((Fp_wfd = fopen(names->fwd, "w")) == NULL) {
             fprintf(stderr,"segy2css: fopen ");
             perror(names->fwd);
             return 0;
        }

        if(Fp_out > 0) close(Fp_out);
        if( (Fp_out = open(names->dataf, O_CREAT | O_WRONLY, MODE)) <= 0 )  {
           fprintf(stderr,"segy2css/rd_segfile(): Can't open %s\n", names->dataf);
           perror(names->dataf);
           return 0;
        }     
       DOFFSET = 0;
    }


    strcpy(wfdir_nam, fpath);

/*  Write wave form and wfdisc files   */

    if(!wrt_segwfd(buffer, &dates, names, name) ) return 0;

    return 1;
}
