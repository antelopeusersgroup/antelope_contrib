#ifndef _DMATRIX_H_
#define _DMATRIX_H_
#include <string>
#include <iostream>
using namespace std;
class dmatrix_error
{
public:
        string message;
        virtual void log_error(){cerr<<"Pf error: %s"<< message<<endl;}
};
class dmatrix_index_error : public dmatrix_error
{
public:
	int row,column;
	int nrr, ncc;
	
        dmatrix_index_error(int nrmax, int ncmax, int ir, int ic)
		{row = ir; column=ic; nrr=nrmax; ncc=ncmax;};
        virtual void log_error()
	{ cerr << "Matrix index (" << row << "," << column 
		<< ")is outside range = " << nrr << "," << ncc << endl;};
};
// exception thrown for incompatible sizes
class dmatrix_size_error : public dmatrix_error
{
public:
	int nrow1, ncol1, nrow2, ncol2;
	dmatrix_size_error (int nr1, int nc1, int nr2, int nc2)
		{nrow1=nr1; ncol1=nc1;nrow2=nr2;ncol2=nc2;};
	virtual void log_error()
	{
		cerr << "Matrix size mismatch:  matrix one is "
			<< nrow1 << "X" << ncol1 
			<< "while matrix two is "
			<< nrow2 << "X" << ncol2 << endl;
	}
};

class dmatrix
{
public:
  dmatrix(int nr, int nc);
  dmatrix(const dmatrix& other);
  ~dmatrix();
  double &operator()(int rowindex, int colindex);
  void operator=(const dmatrix& other);
  void operator+=(const dmatrix& other);
  void operator-=(const dmatrix& other);
  friend dmatrix operator+(const dmatrix&, const dmatrix&);
  friend dmatrix operator-(const dmatrix&, const dmatrix&);
  friend dmatrix operator*(const dmatrix&, const dmatrix&);
  friend dmatrix operator*(const double&, const dmatrix&);
  friend dmatrix operator/(const dmatrix&, const double&);
  friend dmatrix tr(const dmatrix&);
  double* get_address(int r, int c);
  friend ostream& operator<<(ostream&, dmatrix&);
  friend istream& operator>>(istream&, dmatrix&);
  int *size();
  void zero();
private:
   double *ary;
   int length;
   int nrr, ncc;
};
#endif

