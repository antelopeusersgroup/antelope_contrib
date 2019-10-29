#include <math.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <errno.h>

#define PI 3.1415927
#define MT_INFILE "mt_inv.in"
#define MT_OUTFILE "mt_inv.out"

struct MOMENT
 {
 float mxx;
 float myy;
 float mxy;
 float mxz;
 float myz;
 float mzz;
 int   isoflag;
 };

struct GREEN
 {
 int zz,nn;
 int np;
 float dt;
 float *u1;  /* Tangential Strikeslip */
 float *u2;  /* Tangential Dipslip    */
 float *u3;  /* Radial     Strikeslip */
 float *u4;  /* Radial     Dipslip    */
 float *u5;  /* Radial     45_slip    */
 float *u6;  /* Vertical   Strikeslip */
 float *u7;  /* Vertical   Dipslip    */
 float *u8;  /* Vertical   45_slip    */
 float *u9;  /* radial     explosion  */
 float *u10; /* Vertical   explosion   */
 };

struct DATA
 {
 int zz,nn;
 int np;
 float dt;
 float *t;  /* Tangential */
 float *r;  /* Radial     */
 float *z;  /* Vertical   */
 float dist;
 float azi;
 float vr;
 char name[250];
 };

void correl();
void realft();
void twofft();
void four1();
void nrerror();
float *vector();
int *ivector();
double *dvector();
float **matrix();
double **dmatrix();
int **imatrix();
float **submatrix();
void free_vector();
void free_ivector();
void free_dvector();
void free_matrix();
void free_dmatrix();
void free_imatrix();
void free_submatrix();
float **convert_matrix();
void free_convert_matrix();
