/* @(#)main.c	1.2 03/20/96  */
/*========================================================================
 *
 * 
 *  segy2css/main.c 
 *
 *  Program read file(s) with  data in SEGY format and convert them
 *  to data file(s) in CSS format.
 *
 *
 ======================================================================*/
#include "segcss.h"
#define LFILE	"List"


main(argc, argv)
int argc;
char *argv[];
{

 FILE *fd;
 struct name names;      /* Structure with the data,wfdusc,station and channel names  */
 struct conver *param;   /* Structure with the station name conversion table  */
 char *sccs_id = "1.2 (03/20/96)";
 char *fname;           /* Name of the file with data in SEGY format  */
 char *parname;         /* Name of the file with station name conversion table  */
 char *str;
 int i,cont;
 int parnum, num;

    cont = 1; Wfid = 0; parnum = -1; Fp_out = -1; 
    
/* Allocate space  */

    if( (names.dataf = (char *) malloc(strlen(DATAF)) ) == NULL )  {
       perror("seg2css/main(): malloc");
       return 0;
    }
    if( (names.fwd = (char *) malloc(strlen(WD_FNAME))) == NULL )  {
       perror("seg2css/main(): malloc");
       return 0;
    }
    if( (fname = (char *) malloc(132) ) == NULL )  {
       perror("seg2css/main(): malloc");
       exit(1);
    }
    if( (parname = (char *) malloc(132) ) == NULL )  {
       perror("seg2css/main(): malloc");
       exit(1);
    }
    if( (str = (char *) malloc(132) ) == NULL )  {
       perror("seg2css/main(): malloc");
       exit(1);
    }
    fprintf(stdout,"%s version %s\n", argv[0], sccs_id);
    
/*  Get parameters from command line  */

    if(argc == 1)  {
       usage(argv[0]);
    }  
    for(num = 1; num < argc-1; num++)  {
       if (strncmp(argv[num], "-event", strlen("-event") ) == 0) 
           Byevent = 1;
        else if(strncmp(argv[num], "-f", strlen("-f")) == 0)  {
           strcpy(parname, argv[num]+strlen("-f"));
           if(strlen(parname) <= 0)   usage(argv[0]);
        } else  {
           fprintf(stderr, "Unknown parameter in a comand line\n");
           usage(argv[0]); 
        } 
    }
    if(strlen(parname) <= 0) parname = NULL; 

/* Put names of all SEGY data files from specified directory to the file 
   'List'  */

    sprintf(str,"ls %s > List\0", argv[argc-1]);
    system(str); 
   

/*  Get file name from list file  */

       if ((fd = fopen(LFILE,"r") ) == NULL)  {
            fprintf(stderr,"error in main():Can't open file List \n");
            perror(LFILE);
            exit(1);
       }  

/* Read  station conversion table file  */

       parnum = init(&param, parname);


/*  Process data file in SEGY format  */

       while(cont)  {
               if( (fname = fgets(fname,132,fd) ) != NULL)  {  
                   for(i = strlen(fname); i >= 0; i--) if( fname[i] == '\n' ) break;
                   fname[i] = '\0';
                   if(!rd_segfil(&names, fname, param, parnum))  
                      fprintf(stderr, "Can't convert %s to css\n", fname);
                }  else cont = 0;
       }
      sprintf(str,"rm List\0");
      system(str);
}
void usage(name)
char *name;
{

        fprintf(stderr,"\nusage:  %s [-event] [-ftbl] \"fname\" ( quotes are required ! )\n\n", name);
        fprintf(stderr,"  Where:\n");
        fprintf(stderr,"    -event    => option, tells program build wfdisc by event. ");
        fprintf(stderr,"\n                 Build by time is default. \n");
        fprintf(stderr,"    tbl       => full name of the file with instrument code/station name table \n\n");
        fprintf(stderr,"    fname     => full data file name in SEGY format\n\n");

        exit(1);
}
