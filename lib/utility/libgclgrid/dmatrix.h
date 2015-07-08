#ifndef _DMATRIX_H_
#define _DMATRIX_H_
#include <string>
#include <iostream>
#include <sstream>
#include <vector>
#include <boost/serialization/vector.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
using namespace std;
//==================================================================
//@{
// Generic data object thrown by dmatrix object for nondescript errors.
// dmatrix_error is a base class for more specific errors.
//
// @author Gary L. Pavlis
//@}
//==================================================================
class dmatrix_error : public exception 
{
public:
    /*! Holds message string posted. */
        string message;
        /*! SEISPP style error print. */
        virtual void log_error(){cerr<<"Pf error: %s"<< message<<endl;}
        /*! std::exception standard interface. */
        virtual const char* what() const throw()
        {
            return message.c_str();
        };
        /* necessary baggage for some compilers - empty destructor */
        ~dmatrix_error() throw(){};
};
//==================================================================
/*!
 Thrown by a dmatrix if a requested index is outside the bounds
 of the matrix dimension.

\author Gary L. Pavlis
//==================================================================
*/
class dmatrix_index_error : public dmatrix_error
{
public:
//@{
/*!
Basic constructor for this error object.
\param nrmax number of rows in matrix
\param ncmax number of columns in matrix
\param ir row index requested
\param ic column index requested
*/
	dmatrix_index_error(int nrmax, int ncmax, int ir, int ic)
		{row = ir; column=ic; nrr=nrmax; ncc=ncmax;};
/*! Writes the error message to standard error.
*/
        virtual void log_error()
	{ cerr << "Matrix index (" << row << "," << column 
		<< ")is outside range = " << nrr << "," << ncc << endl;};
        /*! std::exception standard interface. */
        virtual const char* what() const throw()
        {
            stringstream ss(message);
            ss << "dmatrix object:  indexing error"<<endl
                << "Matrix index (" << row << "," << column
                << ")is outside range = " << nrr << "," << ncc << endl;
            string result(ss.str());
            return result.c_str();
        };
        /* necessary baggage for some compilers - empty destructor */
        ~dmatrix_index_error() throw(){};
private:
	int row,column;
	int nrr, ncc;
};
/*!
Thrown by a dmatrix when two matrices have a size mismatch.

\author Gary L. Pavlis
*/
class dmatrix_size_error : public dmatrix_error
{
public:
/*!
Basic constructor for this error object.
\param nr1 number of rows in matrix 1
\param nc1 number of columns in matrix 1
\param nr1 number of rows in matrix 2
\param nc1 number of columns in matrix 2
*/
	dmatrix_size_error (int nr1, int nc1, int nr2, int nc2)
		{nrow1=nr1; ncol1=nc1;nrow2=nr2;ncol2=nc2;};
/*! Writes the error message to standard error.*/
	virtual void log_error()
	{
            cerr << "dmatrix class:   size mismatch error in binary operator"<<endl
                << "matrix on left is "<< nrow1 << "X" << ncol1
		<< "while matrix on right is "
		<< nrow2 << "X" << ncol2 << endl;
	};
        /*! std::exception standard interface. */
        virtual const char* what() const throw()
        {
            stringstream ss(message);
            ss << "dmatrix class:   size mismatch error in binary operator"<<endl
                << "matrix on left is "<< nrow1 << "X" << ncol1
		<< "while matrix on right is "
		<< nrow2 << "X" << ncol2 << endl;
            string result(ss.str());
            return result.c_str();
        };
        /* necessary baggage for some compilers - empty destructor */
        ~dmatrix_size_error() throw(){};
private:
	int nrow1, ncol1, nrow2, ncol2;
};
//==================================================================
//@{
// Lightweight, simple double precision matrix object. 
// Provides basic matrix functionality. Note that elements of the
// matrix are stored internally in FORTRAN order but using
// C style indexing.  That is, all indices begin at 0, not 1 and 
// run to size - 1.  Further, FORTRAN order means the elements are
// actually ordered in columns as in FORTRAN in a continuous,
// logical block of memory.  This allow one to use the BLAS functions
// to access the elements of the matrix.  As usual be warned this
// is useful for efficiency and speed, but completely circumvents the
// bounds checking used by methods in the object.  
//
// @author Robert R and Gary L. Pavlis
//@}
//==================================================================
class dmatrix
{
public:
//@{
// Default constructor.  Produces a 1x1 matrix as a place holder.
//@}
  dmatrix();
//@{
// Basic constructor.  Allocates space for nr x nc array.
// @param nr number of rows to allocate for this matrix.
// @param nc number of columns to allocate for this matrix.
//@}
  dmatrix(int nr, int nc);
//@{
// Standard copy constructor
// @param other matrix to be copied/
//@}
  dmatrix(const dmatrix& other);
//@{
// Destructor.  Nothing special.
//@}
  ~dmatrix();
//@{
// Indexing operator for a matrix object.  Gets value of 
// matrix at (rowindex,colindex).  
// @param rowindex row to fetch
// @param colindex column to fetch.  
// @returns Reference to matrix element at position (rowindex,colindex)
// @throws dmatrix_index_error is thrown if request is out of range
//@}
  double &operator()(int rowindex, int colindex);
//@{
// Standard assignment operator
//@}
  dmatrix& operator=(const dmatrix& other);
//@{
// Adds one matrix to another.  X->X+A where A is right hand side.
// @throws dmatrix_size_error is thrown if two matrices are not of the same size
//@}
  void operator+=(const dmatrix& other);
//@{
// Subtracts one matrix from another.  X->X-A where A is right hand side.
// @throws dmatrix_size_error is thrown if two matrices are not of the same size
//@}
  void operator-=(const dmatrix& other);
//@{
// Add two matrices.  X=A+B.
// @throws dmatrix_size_error is thrown if two matrices are not of the same size
//@}
  friend class dvector;
  friend dmatrix operator+(const dmatrix&, const dmatrix&);
//@{
// Take the difference of two matrices.  X=A-B.
// @throws dmatrix_size_error is thrown if two matrices are not of the same size
//@}
  friend dmatrix operator-(const dmatrix&, const dmatrix&);
//@{
// Multiply two matrices.  X=A*B.
// @throws dmatrix_size_error is thrown if columns in A != rows of B.
//@}
  friend dmatrix operator*(const dmatrix&, const dmatrix&);
//@{
// Scale a matrix by a constant.  X=c*A where c is a constant.
//@}
  friend dmatrix operator*(const double&, const dmatrix&);
//@{
// Divide each element of a matrix by a constant.  X=A/c where c is a scalar. 
//@}
  friend dmatrix operator/(const dmatrix&, const double&);
//@{
// Transpose a matrix.  Given X, returns X^T.  
//@}
  friend dmatrix tr(const dmatrix&);
//@{
// Return a pointer to an element (r,c) of matrix.  This is most
// useful in constructs using the BLAS that require a pointer reference
// to an element of a matrix.  
// @param r is row index requested.
// @param c is column index requested.
// @throws dmatrix_index_error is thrown if request is out of range
//@}
  double* get_address(int r, int c);
//@{
// Output stream operator for a matrix.
//@}
  friend ostream& operator<<(ostream&, dmatrix&);
//@{
// Input stream operator for a matrix. 
//@}
  friend istream& operator>>(istream&, dmatrix&);
//@{
// Get the size of this matrix.  User must delete [] the result.
// User should use rows() and columns() method in preference to this.
// @returns Two element int vector with row=[0] and column=[1].  
//@}
  int *size();
//@{
// Return the number of rows in this matrix.
//@}
  int rows();
//@{
//  Return the number of columns in this matrix.  
//@}
  int columns();
//@{
//  Initialize a matrix to all zeros.
//@}
  void zero();
protected:
   //double *ary;
   vector<double> ary;   // initial size of container 0
   int length;
   int nrr, ncc;
private:
   friend class boost::serialization::access;
   template<class Archive>void serialize(Archive & ar, 
                           const unsigned int version)
   {
       ar & nrr & ncc & length;
       ar & ary;
   }
};
//@{
// Vector derived from a dmatrix.
// A vector is matrix with only one column.  We can derive it
// easily by using inheritance from dmatrix but forcing number of
// columns to be one.  
//@}
class dvector : public dmatrix
{
public:
	dvector():dmatrix(){};
	dvector& operator=(const dvector& other);
	double &operator()(int rowindex);
	dvector(int nrv) : dmatrix(nrv,1){};
	dvector(const dvector& other);
//@{
// Multiply matrix with a vector A*x
// @throws dmatrix_size_error is thrown if columns in A != rows of B.
//@}
        friend dvector operator*(const dmatrix&, const dvector&);
};
	
#endif
