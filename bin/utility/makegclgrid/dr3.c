/*   This is a collection of double vector routines taken from
    datascope coords library from Danny Harvey.  When this
    program was under construction these functions were not 
   properly set up in the coords library.  I patched them together
    here and added dr3tran by hand translation of a fortran code.
    G Pavlis, December 2000

===========
  	Title	:  dr3cros.c
  	Author	:  Jim Lewis (adapted from r3cros_.f)
  	Date	:  19 April, 1991
  	Synopsis:  Cross product of two 3-vectors
  	Keywords:  ees, matvec, dr3cros, vector
  	Revisions:
  	mm/dd/yy   name		description
  	Usage	:  void dr3cros(a, b, c)
  		   double a[3], b[3], c[3];
  	Input	:  a:  3-vector
  		   b:  3-vector
  	Output	:  c:  3-vector cross product of a times b
  	Cross product of two 3-vectors
  
*/

/*
void dr3cros(a,b,c)
double a[3],b[3],c[3];
*/
void dr3cros(double *a, double *b, double *c)
{
   double d[3];

   d[0] = a[1]*b[2] - a[2]*b[1];
   d[1] = a[2]*b[0] - a[0]*b[2];
   d[2] = a[0]*b[1] - a[1]*b[0];

   c[0] = d[0];
   c[1] = d[1];
   c[2] = d[2];

   return;
}

/*
  	Title	:  dr3mxv.c
  	Author	:  George Kaplan, adapted from MIT matvec library
  	Date	:  01 Mar 1990
  	Synopsis:  Multiply 3x3 matrix by a 3-vector
  	Keywords:  ees, matvec, r3mxv, matrix, vector
  	Revisions:
  	mm/dd/yy   name		description
  	Usage	:  void dr3mxv(a, b, c)
  		   double a[9], b[3], c[3];
  	Input	:  a:  3x3 matrix
  		   b:  3 vector
  	Output	:  c:  product 3-vector a times b

   ************************************************************ */

/*
void dr3mxv(a, b, c)
double  a[9], b[3], c[3];
*/
void dr3mxv(double *a, double *b, double *c)
{
	double d[3];
	int i;

	d[0] = a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
	d[1] = a[3] * b[0] + a[4] * b[1] + a[5] * b[2];
	d[2] = a[6] * b[0] + a[7] * b[1] + a[8] * b[2];
	for (i = 0; i < 3; i++)  c[i] = d[i];
}

/*	Title	:  dr3sub.c
 *	Author	:  George Kaplan, from MIT matvec library
 *	Date	:  02 Mar 1990
 *	Synopsis:  Subtract a vector from another
 *	Keywords:  ees, matvec, r3sub, vector
 *	@(#)r3sub_.f	1.2 3/2/90  UCB SSL
 *	Revisions:
 *	mm/dd/yy   name		description
 *       7/19/91  danq  double precision version
 */


/*
void            dr3sub(v1, v2, v3)
double          v1[3], v2[3], v3[3];
*/
void dr3sub(double *v1, double *v2, double *v3)
{
    int             i;

    for (i = 0; i < 3; i++)
	v3[i] = v1[i] - v2[i];
}

/*
  	Title	:  r3sxv.c
  	Author	:  Jim Lewis (adapted from r3sxv.c)
  	Date	:  19 April, 1991
  	Synopsis:  Multiply scalar times 3-vector
  	Keywords:  ees, matvec, r3sxv, scalar, vector
  	Revisions:
  	mm/dd/yy   name		description
  	Usage	:  void r3sxv(a, b, c)
  		   double a, b[3], c[3];
  	Input	:  a:  scalar
  		   b:  3-vector
  	Output	:  c:  3-vector product of a times b

   ************************************************************ */

/*
void  dr3sxv(a, b, c)
double a, b[3];
double c[3];
*/
void dr3sxv(double a, double *b, double *c)
{
	c[0] = a * b[0];
	c[1] = a * b[1];
	c[2] = a * b[2];
}

/* double version of r3tran in coords.h */
void dr3tran(double *a, double *b)
{
	b[0]=a[0];
	b[1]=a[3];
	b[2]=a[6];
	b[3]=a[1];
	b[4]=a[4];
	b[5]=a[7];
	b[6]=a[2];
	b[7]=a[5];
	b[8]=a[8];
}
