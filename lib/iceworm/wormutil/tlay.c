/*
 * tlay.c : Travel-time in layered half space (ala Eaton)
 *
 *$ 95Oct19 LDD Explicitly declared return types for all functions.
 *              Added function prototypes to tlay.h
 */
/*********************C O P Y R I G H T   N O T I C E ***********************/
/* Copyright 1991 by Carl Johnson.  All rights are reserved. Permission     */
/* is hereby granted for the use of this product for nonprofit, commercial, */
/* or noncommercial publications that contain appropriate acknowledgement   */
/* of the author. Modification of this code is permitted as long as this    */
/* notice is included in each resulting source module.                      */
/****************************************************************************/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "kom.h"
#include "tlay.h"

#define Panic(x) (fprintf(stderr,"Panic point %d in tlay.c\n",(x)),exit(-1))

#define MAXLAY 20
static int nLay = 0;
int iRef;

/* Function prototypes from other source files*/
void mnbrak(float *, float *, float *, float *, float *, float *, float(*)(float) );
float brent(float, float, float, float (*)(float), float, float *);

/* Global variables is used by t_fun and t_dis function                 */
static int    iSrc;     /* Source layer index, for direct ray           */
static double zSrc;     /* Source depth, for direct ray calc            */
static double xSrc;     /* Source distance, for direct ray calc         */

/* Global variables set by t_set and used by t_lay for fast calculations*/
static double zTop[2*MAXLAY];
static double vLay[2*MAXLAY];

/* Travel time command processing       */
int initMod   = 0;
double PoverS = 1.72;

/***********************************************************************
 *   t_com():  Process all recognized commands.                        *
 *             Return 1 on success, 0 if command was not recognized    *
 ***********************************************************************/
int t_com( void )
{
        double z, v, r, t, dtdr, dtdz;
        double tz, tr;
        double rmax, rdel;
        TPHASE treg[4];
        int np;
        int i;

        if(k_its("lay")) {
                z = k_val();
                v = k_val();
                t_model(z, v);
                return 1;
        }

        if(k_its("psratio")) {
                PoverS = k_val();
                return 1;
        }

        if(k_its("ray")) {
                z = k_val();
                r = k_val();
                t = t_lay(r, z, &dtdr, &dtdz);
                fprintf(stdout, "%6.2f : %6.2f %6.2f %6.2f %6.2f\n",
                        t, r, z, dtdr, dtdz);
                return 1;
        }

        if(k_its("raytst")) {
                t_set();
                z = k_val();
                rmax = k_val();
                rdel = k_val();
                for(r=rdel; r<rmax; r+=rdel) {
                        tr = t_lay(r+0.1, z, &dtdr, &dtdz);
                        tz = t_lay(r, z+0.1, &dtdr, &dtdz);
                        t = t_lay(r, z, &dtdr, &dtdz);
                        fprintf(stdout,
                                "%d %6.1f %6.1f %6.2f %6.2f %6.2f %6.2f\n",
                                iRef, z, r, dtdr, dtdz,
                                10.0*(tr-t), 10.0*(tz-t));
                }
                return 1;
        }

        if(k_its("region")) {
                t_set();
                z = k_val();
                rmax = k_val();
                rdel = k_val();
                for(r=rdel; r<rmax; r+=rdel) {
                        np = t_region(r, z, treg);
                        fprintf(stdout, "%6.1f : ", r);
                        for(i=0; i<np; i++)
                                fprintf(stdout, "%6.2f ", treg[i].t);
                        fprintf(stdout, "\n");
                }
                return 1;
        }

        return 0;
}

/**************************************************************************
 * t_model()  Add layer to velocity model.                                *
 **************************************************************************/
int t_model(double z, double v)
{
        int i;

        if(nLay < MAXLAY) {
                zTop[nLay] = z;
                vLay[nLay] = v;
                nLay++;
        }

        for(i=1; i<nLay; i++) {
                zTop[2*nLay-1-i] = 2*zTop[nLay-1] - zTop[i];
                vLay[2*nLay-1-i] = vLay[i-1];
        }
        zTop[nLay] = zTop[nLay-1] + 0.01;

/*      printf("nLay = %d\n", nLay);
        for(i=0; i<2*nLay-1; i++)
                printf("%d : %6.1f %6.1f\n", i, zTop[i], vLay[i]); */ /*DEBUG*/
        return nLay;
}

/**************************************************************************
 *   t_set()  Set up travel time calculations                             *
 **************************************************************************/
int t_set( void )
{
        if(initMod)
                return nLay;
        initMod = 1;
        return nLay;
}

/**************************************************************************
 *   t_fun()  Calculate direct travel time from takeoff angle             *
 **************************************************************************/
float t_fun(float r)
{
        int i;
        double q;
        double p;
        float t;
        float td;

        p = atan(r/zSrc);
        t = 0.0;
        for(i=0; i<=iSrc; i++) {
                if(i == iSrc) {
                        td = (float) (fabs(zSrc - zTop[i]) / cos(p) / vLay[i]);
                } else {
                        q = asin(vLay[i]*sin(p)/vLay[iSrc]);
                        td = (float)((zTop[i+1] - zTop[i]) / cos(q) / vLay[i]);
                }
                t += td;
        }
        return t;
}

/**************************************************************************
 *   t_dis()  Calculate direct ray distance from takeoff angle            *
 **************************************************************************/
float t_dis( float r )
{
        int i;
        double q;
        double p;
        float x;
        float xd;

        p = atan(r/zSrc);
        x = (float)0.0;
        for(i=0; i<=iSrc; i++) {
                if(i == iSrc) {
                        xd = (float) ((zSrc - zTop[i]) * tan(p));
                } else {
                        q = asin(vLay[i]*sin(p)/vLay[iSrc]);
                        xd = (float)((zTop[i+1] - zTop[i]) * tan(q));
                }
                x += xd;
        }
        return (float) ((x-xSrc)*(x-xSrc));
}

/**************************************************************************
 *  t_lay()  Calculate travel times                                       *
 **************************************************************************/
double t_lay( double  r,        /* Epicentral distance          */
              double  z,        /* Hypocentral depth            */
              double *dtdr,     /* dTdR                         */
              double *dtdz )    /* dTdZ                         */
{
        double tmin;            /* Minimum trav time            */
        double t;               /* Travel time                  */
        double x;               /* Critical dist                */
        double p;               /* Takeoff angle                */
        double q;               /* Layer incidence              */
        double td;              /* Travel time inc              */
        double xd;              /* Crit. dist inc               */
        double tdir;
        double toa;
        double sgn;
        float ax, bx, cx;
        float fa, fb, fc;
        float xmin;
        int isrc;
        int i, j;

        t_set();
        tmin = 10000.0;
        sgn = 1.0;
        if(z < 0.0) {
                z = -z;
                sgn = -sgn;
        }
/* Calculate source layer                       */
        isrc = 0;
        for(i=1; i<nLay; i++) {
                if(z > zTop[i])
                        isrc = i;
        }
        iSrc = isrc;
        zSrc = z;
        if ( zSrc < .01 ) zSrc = .01;              /* Added by WMK 2/12/96 */
        xSrc = r;

/* Calculate minimum refraction         */
        iRef = 0;
        for(i=isrc+1; i<nLay; i++) {
                t = 0.0;
                x = 0.0;
                p = 1.0/vLay[i];
                for(j=0; j<i; j++) {
                        q = asin(p * vLay[j]);
                        td = (zTop[j+1]-zTop[j]) / cos(q) / vLay[j];
                        xd = (zTop[j+1]-zTop[j]) * tan(q);
                        t += td;                /* Upgoing ray          */
                        x += xd;
                        if(j == isrc) { /* Source in layer      */
                                t += td * (zTop[j+1] - z)
                                        / (zTop[j+1] - zTop[j]);
                                x += xd * (zTop[j+1] - z)
                                        / (zTop[j+1] - zTop[j]);
                        }
                        if(j > isrc) {  /* Downgoing ray        */
                                t += td;
                                x += xd;
                        }
                }
                if(x < r) {
                        t += (r-x) / vLay[i];
                        if(t < tmin) {
                                iRef = i;
                                tmin = t;
                                toa = 3.1415927 - asin(p * vLay[isrc]);
                        }
                }
        }

/* Travel time of direct ray */
        ax = (float)(0.8 * xSrc);
        bx = (float)(1.5 * xSrc);
        mnbrak(&ax, &bx, &cx, &fa, &fb, &fc, t_dis);
        brent(ax, bx, cx, t_dis, (float)0.001, &xmin);
        tdir = t_fun(xmin);
        if(tdir < tmin) {
                iRef = 0;
                tmin = tdir;
                toa = atan(xmin/zSrc);     /* changed from z to zSrc by WMK */
                if(toa < 0.0)
                        toa += 3.141592654;
        }

        *dtdz = sgn * cos(toa)/vLay[isrc];
        *dtdr = sin(toa)/vLay[isrc];
        return tmin;
}

/**************************************************************************
 *  t_direct()  Calculate travel time of direct P                         *
 **************************************************************************/
double t_direct( double  r,
                 double  z,
                 double *dtdr,
                 double *dtdz )
{
        double toa;
        double tp;
        float ax, bx, cx;
        float fa, fb, fc;
        float xmin;
        int i;

/* source layer */
        iSrc = 0;
        for(i=1; i<nLay; i++)
                if(z > zTop[i])
                        iSrc = i;
        zSrc = z;
        xSrc = r;

/* Calculate travel time, need to take care of Z > 0 by direct calcuation */
        ax = (float)(0.8 * xSrc);
        bx = (float)(1.5 * xSrc);
        mnbrak(&ax, &bx, &cx, &fa, &fb, &fc, t_dis);
        brent(ax, bx, cx, t_dis, (float) 0.001, &xmin);
        toa = atan(xmin/z);
        if(toa < 0.0)
                toa += 3.141592654;
        tp = t_fun(xmin);
        *dtdz = cos(toa) / vLay[iSrc];
        *dtdr = sin(toa) / vLay[iSrc];
        return tp;
}

/**************************************************************************
 *  t_pmp()  Calculate travel time of mantle reflection                   *
 **************************************************************************/
double t_pmp( double  r,
              double  z,
              double *dtdr,
              double *dtdz )
{
        double toa;
        double tpmp;
        float ax, bx, cx;
        float fa, fb, fc;
        float xmin;
        int nlay;
        int i;

/* source layer */
        nlay = nLay;
        nLay = 2*nlay - 1;
        iSrc = 0;
        for(i=1; i<nLay; i++)
                if(z > zTop[i])
                        iSrc = i;
        zSrc = z;
        xSrc = r;

/* Calcuate travel time of direct ray from image source         */
        ax = (float)(0.8 * xSrc);
        bx = (float)(1.5 * xSrc);
        mnbrak(&ax, &bx, &cx, &fa, &fb, &fc, t_dis);
        brent(ax, bx, cx, t_dis, (float)0.001, &xmin);
        toa = atan(xmin/z);
        if(toa < 0.0)
                toa += 3.141592654;
        tpmp = t_fun(xmin);
        *dtdz = -cos(toa) / vLay[iSrc];
        *dtdr = sin(toa) / vLay[iSrc];
        nLay = nlay;
        return tpmp;
}

/**************************************************************************
 *  t_phase()  Calculate travel time for a given phase                    *
 **************************************************************************/
double t_phase( int     ph,             /* Phase index (0-5)    */
                double  r,              /* Epicentral distance  */
                double  z,              /* Hypocentral depth    */
                double *dtdr,           /* dTdR                 */
                double *dtdz )          /* dTdZ                 */
{
        double t;

        t_set();
        switch(ph) {
        case 0: /* P    */
        case 2: /* Pn   */
                t = t_lay(r, z, dtdr, dtdz);
                break;
        case 1: /* S    */
        case 3: /* Sn   */
                t = PoverS * t_lay(r, z, dtdr, dtdz);
                *dtdr *= PoverS;
                *dtdz *= PoverS;
                break;
        case 4: /* Pg   */
                nLay--;
                t = t_lay(r, z, dtdr, dtdz);
                nLay++;
                break;
        case 5: /* Sg   */
                nLay--;
                t = PoverS * t_lay(r, z, dtdr, dtdz);
                nLay++;
                *dtdr *= PoverS;
                *dtdz *= PoverS;
                break;
        case 6: /* P*, Direct P */
                t = t_direct(r, z, dtdr, dtdz);
                break;
        case 7: /* PmP */
                t = t_pmp(r, z, dtdr, dtdz);
                break;
        }
        return t;
}

/**************************************************************************
 *  t_region()  Calculate regional phase travel times: P, Pg, S, and Sg   *
 **************************************************************************/
int t_region( double  r,        /* Epicentral distance                  */
              double  z,        /* Hypocentral depth                    */
              TPHASE *treg)     /* Travtime and derive for each phase   */
{
        double t;
        double dtdr;
        double dtdz;

        t = t_lay(r, z, &dtdr, &dtdz);
        treg[0].phase = 0;
        treg[0].t = t;
        treg[0].dtdr = dtdr;
        treg[0].dtdz = dtdz;
        treg[1].phase = 1;
        treg[1].t = PoverS * t;
        treg[1].dtdr = PoverS * dtdr;
        treg[1].dtdz = PoverS * dtdz;
        if(nLay < 2)
                return 2;
        nLay--;
        t = t_lay(r, z, &dtdr, &dtdz);
        nLay++;
        if(t < treg[0].t + 0.1)
                return 2;
        treg[0].phase = 2;
        treg[1].phase = 3;
        treg[2].phase = 4;
        treg[2].t = t;
        treg[2].dtdr = dtdr;
        treg[2].dtdz = dtdz;
        treg[3].phase = 5;
        treg[3].t = PoverS * t;
        treg[3].dtdr = PoverS * dtdr;
        treg[3].dtdz = PoverS * dtdz;
        return 4;
}
