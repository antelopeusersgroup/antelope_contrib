/* $Name $Revision$ $Date$    */
/************************************************************
 *
 * 
 *   segy2css/init.c()                        
 *
 *  Read file of settings '.segy2css' or specified in the command
 *  line ASCII file, to convert instrument
 *  serial number to the station name.
 *  Files should be in format:
 *  snum    start_time         end_time           sta_name                   
 *  NNN     1993356:12:10:10.0 1993356:18:20:20.0 NNNNNN 
 *
 * If file '.segy2css' doesn't exist in the home directory
 * program will take settings from include file 'segcss.h'.
 *
 **********************************************************/
#include "segcss.h"
#include "util.h"
#include "trim.h"


int init(output, parname)
struct conver **output;
char *parname;

{
FILE    *fp;
char *home;
struct stat buf;
char    fname[132];
char    name[132];
int i, nrec = 0;

struct list {
    struct conver conver;
    struct list *next;
} head, *crnt, *new, *next;
 
struct conver conver, *array;




/* try to read parameter file for settings */
 
/* Does conversion file specified in the command line  */

    if(parname != NULL) {
        strcpy(fname, parname);
        fname[strlen(parname)] = '\0';
    } else  {

/* We have not specific conversion file. Try to find .segy2css in HOME directory */

       home = (char *) getenv("HOME");
       if(home)  {
           strcpy (fname, home);
           strcat (fname, "/");
           strcat (fname, ".segy2css");
       }  else strcpy (fname, ".segy2css");
    }
    if(stat(fname, &buf) != 0)  {
       if(ENOENT) return 0;
       else {
           printf("segy2css/init()::can't stat");
            perror(fname);
            exit(1);
       }
    }  else  {

/* Open conversion file and fill structure 'conver' with parameters  */

        if((fp=fopen(fname,"r"))!=NULL){
            head.next = NULL;
            crnt      = &head;
            while(fscanf(fp, CONVER_SCS, CONVER_RVL(&conver)) != EOF) {
              CONVER_TRM(&conver);
              if ((new = (struct list *) malloc(sizeof(struct list))) == NULL) {
                   perror("segy2css/init(): malloc");
                   exit(1);
              }
              new->conver = conver;
              new->next   = NULL;
              crnt->next  = new;
              crnt        = crnt->next;
              ++nrec;
           }
 
/*  Transfer from linked list into array  */
 
           if ((array = (struct conver *)malloc(nrec*sizeof(struct conver))) == NULL) {
               perror("segy2css/init(): malloc");
               exit(1);
           }
 
           i = 0; crnt = head.next;
           while (crnt != NULL) {
               next = crnt->next;
               array[i++] = crnt->conver;
               free(crnt);
               crnt = next;
           }
           assert(i == nrec);
        
/*  Assign array to user provided pointer and return number of elements  */
 
           *output = array;
           fclose(fp);
           return nrec;
      }  else {
         printf("segy2css/init(): Can't open file %s\n", fname);
         exit(1);
      }

    }
   return TRUE;
} 

/* $Id$ */
