/*
 * kom.c : Simple positional command parcer.
 *
 *$ 91May07 CEJ Version 1.0
 *$ 93Oct03 CEJ Added k_put routine.
 *$ 95Oct18 LDD Created kom.h to house function prototypes.
 *              Explicitly declared return types for all functions.
 */
/*********************C O P Y R I G H T   N O T I C E ***********************/
/* Copyright 1991 by Carl Johnson.  All rights are reserved. Permission     */
/* is hereby granted for the use of this product for nonprofit, commercial, */
/* or noncommercial publications that contain appropriate acknowledgement   */
/* of the author. Modification of this code is permitted as long as this    */
/* notice is included in each resulting source module.                      */
/****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kom.h"

#define MAXCRD 256
static struct k_buf {
        FILE *fid;
        int ncrd;               /* bytes in command             */
        int istr;               /* index of current string      */
        int icrd;               /* scan pointer                 */
        int ierr;               /* error if non-zero            */
        char crd[MAXCRD];       /* command buffer               */
        char csav[MAXCRD];      /* last command read            */
} ;

#define MAXBUF 4
struct k_buf com = {0L, 0, 0, 0, 0, "Virgin", "Virgin"};
struct k_buf Kbuf[MAXBUF];
int Nbuf = 0;


/*
 **** k_open : open new file for k-system input.  For now only one file
 *              can be open at a time, but it should be straight foward to add
 *              recursion capabitity using a stack in the com area.
 */
int k_open( char *name )
{
        if(Nbuf < MAXBUF && com.fid) {
                Kbuf[Nbuf++] = com;
                com.fid = 0;
        }
        if(com.fid)
                return(0);
/*      if(!strcmp(name, "term"))
                com.fid = stdin;
        else                                                    */
                com.fid = fopen(name, "r");
        if(com.fid)
                return(Nbuf+1);
        return(0);

}
/*
 **** k_close : close current file
 */
int k_close( void )
{
        if(com.fid){
                if(com.fid != stdin)
                        fclose(com.fid);
                com.fid = 0L;
        }
        if(Nbuf > 0) {
                com = Kbuf[--Nbuf];
                return Nbuf+1;
        }
        return(0);

}

/*
 **** k_get : Return pointer to current card.
 */
char *k_get( void )
{
        return com.crd;
}

/*
 **** k_dump : Print last card read.
 */
void k_dump( void )
{
        printf("%s\n", com.crd);
}

/*
 **** k_err : return last error code and clear
 */
int k_err( void )
{
        int jerr;

        jerr = com.ierr;
        com.ierr = 0;
        return(jerr);
}

/*
 * k_int : Parce next token as integer.
 */
int k_int( void )
{
        int ival;
        char *s;

        s = k_str();
        if(!s) {
                com.ierr = -1;
                return(0);
        }
        ival = atoi(s);
        if(ival == 0 && *s != '0') {
                com.ierr = -2;
                return(0);
        }
        return(ival);
}

/*
 * k_its : Compare string from last token to given command.
 */
int k_its(char *c)
{
        char *s;

        s = &com.crd[com.istr];
        while(*c == *s) {
                if(*s == '\0')
                        return(1);
                c++;
                s++;
        }
        return(0);
}

/*
 * k_long : Return next token as a long integer.
 */
long k_long( void )
{
        long lval;
        char *s;

        s = k_str();
        if(!s) {
                com.ierr = -1;
                return(0L);
        }
        lval = atol(s);
        if(lval == 0 && *s != '0') {
                com.ierr = -2;
               return(0L);
        }
        return(lval);
}

/*
 **** k_put : insert command line into buffer
 */
int k_put(char *crd)
 {
        int i, n;

        strcpy(com.crd, crd);
        com.ncrd = strlen(crd);
        if(com.ncrd && com.crd[com.ncrd-1] == '\n')
                com.crd[--com.ncrd] = 0;
        if(!com.ncrd) {
                com.ncrd = 1;
                com.crd[0] = ' ';
                com.crd[1] = 0;
        }
        com.istr = 0;
        com.icrd = 0;
        com.ierr = 0;
        n = 1;
        for(i=0; i<com.ncrd; i++) {
                if(com.crd[i] == '\t')
                        com.crd[i] = ' ';
                if(com.crd[i] != ' ')
                        n = i + 1;
        }
        com.ncrd = n;
        com.crd[n] = 0;
        strcpy(com.csav, com.crd);
        return(com.ncrd);
}

/*
 **** k_rd : read command line into buffer
 */
int k_rd( void )
{
        FILE *fid;
        extern struct k_buf com;
        int n;
        int i;

        fid = com.fid;
        if(com.fid) {
                if(!fgets(com.crd, MAXCRD-1, fid))
                        if(feof(fid))
                                return(0);
        } /*else {
                if(!gets(com.crd))
                        return(0);
        }                                 */
        com.ncrd = strlen(com.crd);
        if(com.ncrd && com.crd[com.ncrd-1] == '\n')
                com.crd[--com.ncrd] = 0;
        if(!com.ncrd) {
                com.ncrd = 1;
                com.crd[0] = ' ';
                com.crd[1] = 0;
        }
        com.istr = 0;
        com.icrd = 0;
        com.ierr = 0;
        n = 1;
        for(i=0; i<com.ncrd; i++) {
                if(com.crd[i] == '\t')
                        com.crd[i] = ' ';
                if(com.crd[i] != ' ')
                        n = i + 1;
        }
        com.ncrd = n;
        com.crd[n] = 0;
        strcpy(com.csav, com.crd);
        return(com.ncrd);
}

/*
 **** k_com : returns last command line read
 */
char *k_com( void )
{
        return com.csav;
}

/*
 **** k_str() : Return next token as a pointer to string.
 */
char *k_str( void )
{
        int state;
        int i;

        state = 1;
        for(i=com.icrd; i<com.ncrd; i++) {
                switch(state) {
                case 1: /* Looking for first non-blank          */
                        if(com.crd[i] == ' ' || com.crd[i] == '\t')
                                break;
                        if(com.crd[i] == '"') {
                                state = 3;
                                com.istr = i + 1;
                                break;
                        }
                        state = 2;
                        com.istr = i;
                        break;
                case 2: /* Looking for end of normal string */
                        if(com.crd[i] == ' ' || com.crd[i] == '\t') {
                                com.crd[i] = 0;
                                com.icrd = i + 1;
                                return(&com.crd[com.istr]);
                        }
                        break;
                case 3: /* Quoted string */
                        if(com.crd[i] == '"') {
                                com.crd[i] = 0;
                                com.icrd = i + 1;
                                return(&com.crd[com.istr]);
                        }
                        break;
                }
        }
        if(state == 2) {
                com.crd[com.ncrd] = 0;
                com.icrd = com.ncrd;
                return(&com.crd[com.istr]);
        }
        com.ierr = -17;
        return( (char *) 0 );
}

/*
 **** k_val() Return next token as a double real
 */
double k_val( void )
{
        double val;
        char *s;

        s = k_str();
        if(!s) {
                com.ierr = -1;
                return(0.0);
        }
        val = atof(s);
        return(val);
}
