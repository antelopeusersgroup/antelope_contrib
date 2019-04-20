#include <stdio.h>
#include <string.h>
#include "db.h"
void usage()
{
    fprintf(stderr,"dbimportQCmetrics db < csvfile\n");
    fprintf(stderr,"create or append output of SetQCmetrics to db\n");
    exit(-1);
}
void parse_error(int linecount,char *inbuf)
{
  fprintf(stderr,"parsing error on line %d with the following content\n",
                   linecount);
  fprintf(stderr,"%s\n",inbuf);
  exit(-1);
}
int main(int argc, char **argv)
{
    if(argc!=2) usage();
    char *dbname=argv[1];
    Dbptr db;
    if(dbopen(dbname,"r+",&db))
    {
        fprintf(stderr,"dbopen failed for dbname=%s\n",dbname);
        usage();
    }
    db=dblookup(db,0,"metrics",0,0);
    if(db.record==dbINVALID)
    {
        fprintf(stderr,"dblookup failed for extension table metrics\n");
        fprintf(stderr,"Check to see if it has been installed in css3.0.ext\n");
        usage();
    }
    char *inbuf=malloc(128);
    int linecount=0;
    size_t bufsize=128;
    while(getline(&inbuf,&bufsize,stdin)!=EOF)
    {
        long evid;
        char sta[10],phase[10],filterdesc[50];
        double compamp,rmssnr,peaksnr;
        /* Always skip line 1 - header label for csv data file */
        if(linecount==0) 
        {
            ++linecount;
            continue;
        }
        char *sptr;  // this is the moving pointer - keep inbuf to free
        sptr=strdup(inbuf);
        char *token;
        token=strsep(&sptr,",");
        if(token==NULL) parse_error(linecount,inbuf);
        sscanf(token,"%ld",&evid);
        token=strsep(&sptr,",");
        if(token==NULL) parse_error(linecount,inbuf);
        strcpy(sta,token);
        token=strsep(&sptr,",");
        if(token==NULL) parse_error(linecount,inbuf);
        strcpy(phase,token);
        token=strsep(&sptr,",");
        if(token==NULL) parse_error(linecount,inbuf);
        strcpy(filterdesc,token);
        token=strsep(&sptr,",");
        if(token==NULL) parse_error(linecount,inbuf);
        sscanf(token,"%lf",&compamp);
        token=strsep(&sptr,",");
        if(token==NULL) parse_error(linecount,inbuf);
        sscanf(token,"%lf",&rmssnr);
        token=strsep(&sptr,",");
        if(token==NULL) parse_error(linecount,inbuf);
        sscanf(token,"%lf",&peaksnr);
        if(dbaddv(db,0,"evid",evid,
                "sta",sta,
                "phase",phase,
                "filter",filterdesc,
                "compratio",compamp,
                "snr",rmssnr,
                "peaksnr",peaksnr,NULL)==dbINVALID)
        {
            fprintf(stderr,"dbaddv error trying to save line=%d\n",linecount);
            exit(-1);
        }
        free(sptr);
        ++linecount;
    }

}
