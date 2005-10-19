#ifndef _INTERPOLATOR1D_H_
#define _INTERPOLATOR1D_H_
#include "dmatrix.h"
namespace INTERPOLATOR1D
{
double linear_scalar ( double x, double x1, double y1,
                                        double x2, double y2 );
void linear_vector (double x, double x1, double *y1,
	double x2, double *y2, int nv, double *y);
int regular_lookup(double xp, double min_x, double dx);
int irregular_lookup(double xp, double *x,int nx);
double linear_scalar_regular ( double xp, double x0, double dx,
		double *y, int nx);
double linear_scalar_irregular(double xp,double *x, double *y, int nx);
void linear_scalar_regular_to_regular(int nin, double x0in, double dxin, double *yin,
		int nout, double x0out, double dxout, double *yout);
void linear_scalar_irregular_to_regular(int nin, double *xin, double *yin,
		int nout, double x0out, double dxout, double *yout);
void linear_scalar_regular_to_irregular(int nin, double x0in, double dxin, double *yin,
		int nout, double *xout, double *yout);
void linear_scalar_irregular_to_irregular(int nin, double *xin, double *yin,
		int nout, double *xout, double *yout);
double *linear_vector_regular ( double	xp, double x0, double dx,dmatrix& y);
double *linear_vector_irregular(double xp,double *x, dmatrix& y);
void linear_vector_regular_to_regular(double x0in, double dxin, dmatrix& yin,
		double x0out, double dxout, dmatrix& yout);
void linear_vector_irregular_to_regular(double *xin, dmatrix& yin,
		double x0out, double dxout, dmatrix& yout);
void linear_vector_regular_to_irregular(double x0in, double dxin, dmatrix& yin,
		double *xout, dmatrix& yout);
void linear_vector_irregular_to_irregular(double *xin, dmatrix& yin,
		double *xout, dmatrix& yout);
}
#endif
