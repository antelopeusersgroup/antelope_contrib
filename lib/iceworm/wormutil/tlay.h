/*
 * tlay.h : Data descriptions used for regional travel time phases
 *      0 = P, 1 = S, 2 = Pn, 3 = Sn, 4 = Pg, 5 = Sg
 *
 *$ 95Oct19 LDD Added prototypes for functions in tlay.c
 */
#ifndef TLAY_H
#define TLAY_H

static char *Phs[] = {"P", "S", "Pn", "Sn", "Pg", "Sg"};
typedef struct {
        int     phase;
        double  t;
        double  dtdr;
        double  dtdz;
} TPHASE;


/* Prototypes for functions in tlay.c that might used in other source files
 **************************************************************************/
int    t_com( void );     /* Process all recognized commands */

/* Calculate regional phase travel times: P, Pg, S, and Sg*/
int    t_region( double, double, TPHASE * );

/* Calculate travel time for a given phase */
double t_phase( int, double, double, double *, double * );

/* Calculate travel times */
double t_lay( double, double, double *, double * );


/* Prototypes for functions used internally by tlay.c functions
 **************************************************************/
int    t_set( void );             /* Set up travel time calculations            */
float  t_dis( float );            /* Calc direct ray distance from takeoff angle*/
float  t_fun( float );            /* Calc direct travel time from takeoff angle */
int    t_model ( double, double );               /* Add layer to velocity model */
double t_direct( double, double, double *, double * ); /* direct P travel time  */
double t_pmp   ( double, double, double *, double * ); /* P mantle reflection t */

#endif
