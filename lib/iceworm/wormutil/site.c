/*
 * site.c : Station parameter routines.
 *
 *$ 95Aug31 LDD Added "site_file" command to site_com()
 *$ 95Sep01 LDD Added 2nd & 3rd args to site_index()
 *$ 95Oct19 LDD Explicitly declared return types for all functions.
 *              Added function prototypes to site.h
 */
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "kom.h"
#include "site.h"

/* Initialization constants
 **************************/
static int initSite = 0;

/* Changed from 1000 to 1800 by WMK 2/12/96 */
static int maxSite  = 1800;     /* Change to alter size of mem allocation */


/**************************************************************************
 * site_init()  Allocate the site table                                   *
 **************************************************************************/
void site_init(void)
{
        if(initSite)
                return;
        initSite = 1;
        nSite = 0;
        Site = (SITE *)calloc(maxSite, sizeof(SITE));
        if(!Site) {
                fprintf(stderr, "site_init:  Could not allocate site table; exitting!\n");
                exit(0);
        }
        return;
}


/**************************************************************************
 * site_load(name)  Process a kom.c-style command file that contains only *
 *                  commands recognized by site_com                       *
 **************************************************************************/

int site_load(char *name)
{
        char *com;

        if(!k_open(name)) {
                fprintf(stderr, "site_load:  Cannot open site file <%s>\n", name);
                return 0;
        }
        while(k_rd()) {
                com = k_str();
                if ( !com )       continue;
                if ( site_com() ) continue;
                fprintf(stderr, "site_load:  <%s> Unknown command\n", com);
        }
        k_close();
/*      fprintf(stderr, "Site file <%s> loaded, nSite = %d\n", name, nSite); */
        return 1;
}


/**************************************************************************
 *  site_read(name)  Read in a HYPOINVERSE format, universal station      *
 *                   code file                                            *
 **************************************************************************/

void site_read(char *name)
{
        FILE  *stafile;
        char   line[256];
        int    dlat, dlon, elev;
        float  mlat, mlon;
        char   comp, ns, ew;
        int    n;

/* initialize site table
   *********************/
        site_init();

/* open station file
   *****************/
        if( (stafile = fopen( name, "r" )) == (FILE *) NULL ) {
                fprintf(stderr,
                       "site_read: Cannot open site file <%s>; exitting!\n", name);
                exit(0);
        }

/* read in one line of the site file at a time
   *******************************************/
        while( fgets( line, sizeof(line), stafile ) != (char *) NULL )
        {

        /* see if internal site table has room left */
                if( nSite >= maxSite ) {
                   fprintf( stderr,
                        "site_read: Site table full; cannot load entire file <%s>\n", name );
                   fprintf( stderr,
                        "site_read: Use <maxsite> command to increase table size; exitting!\n" );
                   exit(0);
                }

        /* decode each line of the file */
                strncpy( Site[nSite].name, &line[0],  5);
                strncpy( Site[nSite].net,  &line[6],  2);
                strncpy( Site[nSite].comp, &line[10], 3);
                comp = line[9];

                line[42] = '\n';
                n = sscanf( &line[15], "%d %f%c%d %f%c%d",
                            &dlat, &mlat, &ns, &dlon, &mlon, &ew, &elev );
                if ( n < 7 ) {
                        fprintf( stderr,
                               "site_read: Error decoding line in station file\n%s\n",
                                line );
                        continue;
                }
        /*      printf( "%-5s %-2s %-3s %d %.4f%c%d %.4f%c%4d\n",
                         Site[nSite].name, Site[nSite].net, Site[nSite].comp,
                         dlat, mlat, ns,
                         dlon, mlon, ew, elev ); */ /*DEBUG*/

        /* use one-letter component if there is no 3-letter component given */
                if ( !strcmp(Site[nSite].comp, "   ") ) sprintf( Site[nSite].comp, "%c  ", comp );

        /* convert to decimal degrees */
                if ( dlat < 0 ) dlat = -dlat;
                if ( dlon < 0 ) dlon = -dlon;
                Site[nSite].lat = (double) dlat + (mlat/60.0);
                Site[nSite].lon = (double) dlon + (mlon/60.0);

        /* make south-latitudes and west-longitudes negative */
                if ( ns=='s' || ns=='S' )
                        Site[nSite].lat = -Site[nSite].lat;
                if ( ew=='w' || ew=='W' || ew==' ' )
                        Site[nSite].lon = -Site[nSite].lon;
                Site[nSite].elev = (double) elev/1000.;

        /*      printf("%-5s %-2s %-3s %.4f %.4f %.0f\n\n",
                       Site[nSite].name, Site[nSite].net, Site[nSite].comp,
                       Site[nSite].lat, Site[nSite].lon, Site[nSite].elev ); */ /*DEBUG*/

       /* update the total number of stations loaded */
                if(nSite < maxSite) ++nSite;

        } /*end while*/

        fclose( stafile );
        return;
}


  /**********************************************************************
   * site_com(): Process all recognized commands.                       *
   *             Return 1 on success, 0 if command was not recognized   *
   **********************************************************************/

int site_com( void )
{
        char *name;

        if(k_its("site")) {
                site_init();
                if(nSite >= maxSite)
                        return 1;
                name = k_str();
                if(!name) return 1;
                strcpy(Site[nSite].name, name);
                strcpy(Site[nSite].net, "");    /*added 950901:ldd*/
                strcpy(Site[nSite].comp, "");   /*added 950901:ldd*/
                Site[nSite].lat = k_val();
                Site[nSite].lon = k_val();
                Site[nSite].elev = 0.0;
                Site[nSite].elev = k_val();
                if(nSite < maxSite)
                        nSite++;
                return 1;
        }

        if(k_its("maxsite")) {
                if(initSite) {
                     fprintf( stderr, "site_com:  Error: site table already allocated.\n" );
                     fprintf( stderr,
                           "site_com:  Use <maxsite> before any <site> or <site_file> commands" );
                     fprintf( stderr, "; exitting!\n" );
                     exit( 0 );
                }
                maxSite = k_int();
                return 1;
        }

        if(k_its("site_file")) {    /* added command to read in a HYPOINVERSE format */
                name = k_str();     /* "universal code" station file.     950831:ldd */
                if(!name) return 1;
                site_read(name);
                return 1;
        }

        return 0;
}


/***************************************************************************
 * site_index(site, net, comp) : Returns index of site, or -1 if not found *
 * 950901:ldd  added 2nd & 3rd arguments to handle "universal" station     *
 *             naming convention                                           *
 ***************************************************************************/

int site_index(char *site, char *net, char *comp)
{
        int i;

        site_init();
        for(i=0; i<nSite; i++) {
                if( !strcmp(Site[i].name, site ) &&
                    !strcmp(Site[i].net,  net  ) &&
                    !strcmp(Site[i].comp, comp )    )  return i;
        }
        return -1;
}

