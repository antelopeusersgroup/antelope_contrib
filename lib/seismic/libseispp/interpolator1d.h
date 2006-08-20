#ifndef _INTERPOLATOR1D_H_
#define _INTERPOLATOR1D_H_
#include "dmatrix.h"
/*! \brief A family of interpolators between regular and irregular 1d grids.
*
* Interpolation is a common theme for a variety of processing.  This family of 
* functions can be used to interpolate between regular and irregular grids 
* on a one-dimensional domain.  The names of the functions are verbose enough
* that their use should be obvious, but complete documentation will clarify.
*  A common feature of all procedures is that if the domain of the output is
*  larger than the input the last point to the left or right (as appropriate)
* will be extended as a constant outside the original range.  
*/
namespace INTERPOLATOR1D
{
/*! \brief Linear interpolator for a single point for a scalar valued function.
* This procedure is called to interpolate a single point that is 
* assumed to be located between two bracketing endpoints.  The
* results is a simple linear interpolation of y(x).
*
* \return y(x) 
* \param x independent variable of position interpolation is desired.
* \param x1 independent variable to the left of x 
* \param y1 y(x1)
* \param x2 independent variable to the right of x.
* \param y2 y(x2)
*/
double linear_scalar ( double x, double x1, double y1,
                                        double x2, double y2 );
/*! \brief Linear interpolator for a single point of a vector valued function.
* This procedure is called to interpolate a single point that is 
* assumed to be located between two bracketing endpoints.  The
* results is a simple linear interpolation of y(x) where in this case
* y is a vector of length nv.
*
* \param x independent variable of position interpolation is desired.
* \param x1 independent variable to the left of x 
* \param y1 y(x1)
* \param x2 independent variable to the right of x.
* \param y2 y(x2)
* \param nv length of vector function y(x).
* \param y array of length nv to hold result. Note this is assumed to 
*   be an existing region of storage.  As usual unpredictable results
*   will follow if y is not an existing block of doubles of length nv.
*/
void linear_vector (double x, double x1, double *y1,
	double x2, double *y2, int nv, double *y);
/*! Get an index position into an array with a regular spacing.
*  This method implements a fairly trivial computation, but 
* standardizes the computation for the ease of the user. 
* \param xp x position of point for which the index is to be computed.
* \param min_x x position of y[0].
* \param dx sample interval along x axis.
*
* \return index position point immediately to the left of xp.
*/
int regular_lookup(double xp, double min_x, double dx);
/*! Get an index position into an irregularly spaced grid.
* In an irregular grid finding a the position of a point is
* not trivial and, in general, can only be done by a linear 
* search forward from the first point.  
* 
* \param xp x position of point for which the index is to be computed.
* \param x array of x values that define the grid.  These are 
*	implicitly assumed to be arranged order of increasing size.
* \param nx length of x vector.
*
* \return index position point immediately to the left of xp.
*/
int irregular_lookup(double xp, double *x,int nx);
/*! Lookup and return a y value for a requested point in a regular grid.
*	This procedure combines lookup with linear interpolation for
* scalar points in a regular grid.  Result is y(xp).
*
* \param xp x value of point to be interpolated.
* \param x0 x value of sample 0.
* \param dx sample interval on x axis for regular grid.
* \param y array of length nx of samples values of y at grid points.
* \param nx length of y array.
*
* \return y(xp) computed by linear interpolation.
*/
double linear_scalar_regular ( double xp, double x0, double dx,
		double *y, int nx);
/*! Lookup and return a y value for a requested point in an irregular grid.
*	This procedure combines lookup with linear interpolation for
* scalar points defined on an irregular grid.  Result is y(xp).
*
* \param xp x value of point to be interpolated.
* \param x array of length nx of x values that define the grid point positions.
* \param y array of length nx of samples values of y at grid points.
* \param nx length of y array.
*
* \return y(xp) computed by linear interpolation.
*/
double linear_scalar_irregular(double xp,double *x, double *y, int nx);
/*! \brief Map from one regular grid defining a scalar function to another.
* This procedure maps all samples from one regular grid to another.  This
* is the same as the process usually called resampling in time series analysis.
* Be warned though that this does nothing to avoid aliasing but simply does a 
* linear interpolation between adjacent points.  To avoid aliasing in downsampling 
* antialias filters should normally be used.
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input.  
* \param nin number of samples in input grid.
* \param x0in x value of sample 0 of input grid.
* \param dxin sample interval of input grid.
* \param yin y values that define the functional values for y on the input grid.
* \param nout number of samples in output grid.
* \param x0out x value of sample 0 of output grid.
* \param dxout sample interval of output grid.
* \param yout array of length nout to hold result of interpolation.  This array is 
*	not bounds checked. i.e. for efficiency the routine assumes this is a valid
*	pointer to an appropriately sized block of memory.  
*/
void linear_scalar_regular_to_regular(int nin, double x0in, double dxin, double *yin,
		int nout, double x0out, double dxout, double *yout);
/*! \brief Map from scalar samples on an irregular grid to a regular one.
* This procedure maps all samples from an irregular to regular grid.  Sampling
* is by a simple interpolation between input grid points.
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input.  
* \param nin number of samples in input grid.
* \param xin array of x values defining irregular input grid positions.,
* \param yin y values that define the functional values for y on the input grid.
* \param nout number of samples in output grid.
* \param x0out x value of sample 0 of output grid.
* \param dxout sample interval of output grid.
* \param yout array of length nout to hold result of interpolation.  This array is 
*	not bounds checked. i.e. for efficiency the routine assumes this is a valid
*	pointer to an appropriately sized block of memory.  
*/
void linear_scalar_irregular_to_regular(int nin, double *xin, double *yin,
		int nout, double x0out, double dxout, double *yout);
/*! \brief Map from a regular grid defining a scalar function 
* an irregular grid.
* This procedure maps all samples from a regular to irregular grid.  
* A simple linear interpolation is used to define an output array of 
* scalars that are estimates of the value of y at the output grid points.
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input.  
* \param nin number of samples in input grid.
* \param x0in x value of sample 0 of input grid.
* \param dxin sample interval of input grid.
* \param yin y values that define the functional values for y on the input grid.
* \param nout number of samples in output grid.
* \param xout array of length nout containing grid positions for desired output.
* \param yout array of length nout to hold result of interpolation.  This array is 
*	not bounds checked. i.e. for efficiency the routine assumes this is a valid
*	pointer to an appropriately sized block of memory.  
*/
void linear_scalar_regular_to_irregular(int nin, double x0in, double dxin, double *yin,
		int nout, double *xout, double *yout);
/*! \brief Map from scalar samples on an irregular grid to a regular one.
* This procedure maps all samples from an irregular to regular grid.  Sampling
* is by a simple interpolation between input grid points.
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input.  
* \param nin number of samples in input grid.
* \param xin array of x values defining irregular input grid positions.,
* \param yin y values that define the functional values for y on the input grid.
* \param nout number of samples in output grid.
* \param xout array of length nout containing grid positions for desired output.
* \param yout array of length nout to hold result of interpolation.  This array is 
*	not bounds checked. i.e. for efficiency the routine assumes this is a valid
*	pointer to an appropriately sized block of memory.  
*/
void linear_scalar_irregular_to_irregular(int nin, double *xin, double *yin,
		int nout, double *xout, double *yout);
/*! Lookup and return a vector-valued y value for a requested point in a regular grid of vectors.
* This procedure combines lookup with linear interpolation for
* vector points defined on a regular grid.  Result is y(xp).
*
* \param xp x value of point to be interpolated.
* \param x0 x value of sample 0.
* \param dx sample interval on x axis for regular grid.
* \param y nv by nx matrix of samples of y at grid points.
*
* \return y(xp) computed by linear interpolation. This vector of length nv is 
*	allocated with new and should be released with delete [] after use.
*/
double *linear_vector_regular ( double	xp, double x0, double dx,dmatrix& y);
/*! Lookup and return a vector-valued y value for a requested point 
* in an irregular grid points.
* This procedure combines lookup with linear interpolation for
* vector points defined on an irregular grid.  Result is y(xp).
*
* \param xp x value of point to be interpolated.
* \param x array of length nx of x values that define the grid point positions.
* \param y nv by nx matrix of samples of y at grid points.
*
* \return y(xp) computed by linear interpolation. This vector of length nv is 
*	allocated with new and should be released with delete [] after use.
*/
double *linear_vector_irregular(double xp,double *x, dmatrix& y);
/*! \brief Map from one regular grid defining a vector-valued function to another.
* This procedure maps all samples from one regular grid to another.  This
* is the same as the process usually called resampling in time series analysis.
* Be warned though that this does nothing to avoid aliasing but simply does a 
* linear interpolation between adjacent points.  To avoid aliasing in downsampling 
* antialias filters should normally be used.  We use a lightweight matrix object to 
* hold y values with allows us to drop the need to pass the number of samples in
* the input and output grids.  
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input.  
* \param x0in x value of sample 0 of input grid.
* \param dxin sample interval of input grid.
* \param yin nv by nxin matrix of y values that define the functional values for y on the input grid.
* \param x0out x value of sample 0 of output grid.
* \param dxout sample interval of output grid.
* \param yout nv by nxout matrix of y values to that will contain the interpolated output
* 	on completion.
*/
void linear_vector_regular_to_regular(double x0in, double dxin, dmatrix& yin,
		double x0out, double dxout, dmatrix& yout);
/*! \brief Map from a vector-valued function defined on an irregular grid to 
* a regular, output grid.
* This procedure maps all samples from an irregular to regular grid.  
* We use a lightweight matrix object to 
* hold y values with allows us to drop the need to pass the number of samples in
* the input and output grids.  
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input.  
* \param xin array of length nin (derived from yin) input grid positions.
* \param yin nv by nxin matrix of y values that define the functional values for y on the input grid.
* \param x0out x value of sample 0 of output grid.
* \param dxout sample interval of output grid.
* \param yout nv by nxout matrix of y values to that will contain the interpolated output
* 	on completion.
*
*/
void linear_vector_irregular_to_regular(double *xin, dmatrix& yin,
		double x0out, double dxout, dmatrix& yout);
/*! \brief Map from one regular grid defining a vector-valued function 
* an irregular output grid.
* This procedure maps all samples from a regular grid to the irregular
* output grid.  We use a lightweight matrix object to 
* hold y values with allows us to drop the need to pass the number of samples in
* the input and output grids.  
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input. 
* 
* \param x0in x value of sample 0 of input grid.
* \param dxin sample interval of input grid.
* \param yin nv by nxin matrix of y values that define the functional values for y on the input grid.
* \param xout array of length nout (derived from yout) output grid positions.
* \param yout nv by nxin matrix of y values that will contain the interpolated output on return.
*/
void linear_vector_regular_to_irregular(double x0in, double dxin, dmatrix& yin,
		double *xout, dmatrix& yout);
/*! \brief Map a vector valued function from one irregular grid to another.  
* 
* This procedure maps all samples using a linear interpolator fuction.
* We use a lightweight matrix object to 
* hold y values with allows us to drop the need to pass the number of samples in
* the input and output grids. 
*
*\par
* Note that if the output grid extents are larger than that of the input the y value 
*	to the left or right (as appropriate) are extended as an estimate of samples
*	outside the bounds of the input. 
* 
* \param xin array of length nin (derived from yin) input grid positions.
* \param yin nv by nxin matrix of y values that define the functional values for y on the input grid.
* \param xout array of length nout (derived from yout) output grid positions.
* \param yout nv by nxin matrix of y values that will contain the interpolated output on return.
*/
void linear_vector_irregular_to_irregular(double *xin, dmatrix& yin,
		double *xout, dmatrix& yout);
}
#endif
