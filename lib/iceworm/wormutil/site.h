/*
 * site.h : Network parameter definitions.
 *
 *$ 95Aug31 LDD Added net & comp to SITE structure definition
 *$ 95Sep19 KL  Added staname & chanid to SITE structure
 *$ 95Oct19 LDD Added prototypes for functions in site.c
 *
 */
#ifndef SITE_H
#define SITE_H

/* Define the structure that will hold the site table
 ****************************************************/
int nSite;
typedef struct {
        char    name[6];    /* shorted from 8 to 6 for "universal" names */
        char    net[3];     /* added for "universal" naming convention   */
        char    comp[4];    /* 950831:ldd                                */
        char    staname[50];
        int     chanid;
        double  lat;
        double  lon;
        double  elev;
} SITE;
SITE *Site;

/* Prototypes for functions in site.c
 ************************************/
int  site_com  ( void );                   /* process recognized commands     */
void site_read ( char * );                 /* read in a HYPOINV site file     */
int  site_load ( char * );                 /* process a kom.c command file    */
int  site_index( char *, char *, char * ); /* return index in the Site table  */
                                           /*   of the given site code        */
#endif
