/* @(#)wrt_segdata.c	1.1 03/12/96  */
/*===========================================================================
 *
 *
 *    segy2css/wrt_segdata.c
 *
 *    Write data file in CSS format to the disk.                             
 *                                                  
 *
 =========================================================================*/
#include "segcss.h"

int wrt_segdata(oldname, names)
char *oldname;                  /* Name of the file with data in SEGY format  */
struct name *names;             /* Names structure  */

{

        char *buffer;
        int fd_in;
        long bytes;


    if( (buffer = (char *) malloc(Data_bytes) ) == NULL )  {
       perror("segy2css/wrt_segdata(): malloc");
       return 0;
    }

    if(Fp_out <= 0) 
       if( (Fp_out = open(names->dataf, O_CREAT | O_WRONLY, MODE)) <= 0 )  {
           fprintf(stderr,"segy2css/wrt_segdata(): Can't open %s\n", names->dataf);
           perror(names->dataf);
           return 0;
       }     
    if( (fd_in = open(oldname, O_RDONLY)) <= 0 )  {
         fprintf(stderr,"segy2css/wrt_segdata(): Can't open %s\n", oldname);
         perror(oldname);
         return 0;
    }     

/* Skip header data  */

    if(lseek(fd_in, DATA_OFF, SEEK_SET) < 0)  {
       fprintf(stderr,"segy2css/wrt_segdata(): lseek error\n");
       free(oldname);
       return 0;
    }

/*  Copy data in a properly place  */

    bytes = read(fd_in, buffer, Data_bytes);  
 
    if (bytes !=  Data_bytes) {
        fprintf(stderr,"wrt_segdata: read %d ",bytes);
        fprintf(stderr,"instead of %d.  ", Data_bytes);
        perror("read:");
        close(fd_in);
        close(Fp_out);
        free(buffer);
        return 0;
    }
    close(fd_in);
    bytes = write(Fp_out, buffer, Data_bytes);  
    if (bytes !=  Data_bytes) {
        fprintf(stderr,"rmtrace/write_trace: write %d ",bytes);
        fprintf(stderr,"instead of %d.  ", Data_bytes);
        perror("write:");
        free(buffer); close(Fp_out);
        return 0;
    }
   
    free(buffer); 
    return 1;
}
