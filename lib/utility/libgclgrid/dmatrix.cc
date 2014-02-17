#include <iostream>
#include <math.h>
#include "dmatrix.h"

using namespace std;
dmatrix::dmatrix()
{
  nrr=0;
  ncc=0;
  length=0;
  ary=NULL;
}
dmatrix::dmatrix(int nr, int nc)
{
  nrr=nr;
  ncc=nc;
  length=nr*nc;
  if(length<1)
      {
      length=1;
      nrr=ncc=0;
      }
  ary=new double[length];
}

dmatrix::dmatrix(const dmatrix& other)
  {
  nrr=other.nrr;
  ncc=other.ncc;
  length=other.length;
  ary=new double[length];
  memcpy(ary,other.ary, length*sizeof(double));
  }

dmatrix::~dmatrix()
{
if(ary!=NULL) delete [] ary;
}

double &dmatrix::operator()(int rowindex, int colindex)
{
  int out_of_range=0;
  if (rowindex>=nrr) out_of_range=1;
  if (rowindex<0) out_of_range=1;
  if (colindex>=ncc) out_of_range=1;
  if (colindex<0) out_of_range=1;
  if (out_of_range)
	throw dmatrix_index_error(nrr,ncc,rowindex,colindex);
  return (ary[rowindex+(nrr)*(colindex)]);
}
//
// subtle difference here.  This one returns a pointer to the 
// requested element
//
double* dmatrix::get_address(int rowindex, int colindex)
{
  double *ptr;
  int out_of_range=0;
  if (rowindex>=nrr) out_of_range=1;
  if (rowindex<0) out_of_range=1;
  if (colindex>=ncc) out_of_range=1;
  if (colindex<0) out_of_range=1;
  if (out_of_range)
        throw dmatrix_index_error(nrr,ncc,rowindex,colindex);
  ptr = ary + rowindex+(nrr)*(colindex);
  return(ptr);
}

dmatrix& dmatrix::operator=(const dmatrix& other)
{
if(&other!=this) 
{
	ncc=other.ncc;
	nrr=other.nrr;
	length=other.length;
	if(ary!=NULL) delete [] ary;
	ary= new double[length];
	memcpy(ary,other.ary, length*sizeof(double));
} 
return *this;
}

void dmatrix::operator+=(const dmatrix& other)
 {
int i;
  if ((nrr!=other.nrr)||(length!=other.length))
	throw dmatrix_size_error(nrr, ncc, other.nrr, other.length);
for(i=0;i<length;i++)
  ary[i]+=other.ary[i];
 }

void dmatrix::operator-=(const dmatrix& other)
 {
int i;
  if ((nrr!=other.nrr)||(length!=other.length))
	throw dmatrix_size_error(nrr, ncc, other.nrr, other.length);
for(i=0;i<length;i++)
  ary[i]-=other.ary[i];
 }

dmatrix operator+(const dmatrix &x1, const dmatrix &x2)
  {
int i;
  if ((x1.nrr!=x2.nrr)||(x1.length!=x2.length))
	throw dmatrix_size_error(x1.nrr, x1.ncc, x2.nrr, x2.length);
 dmatrix tempmat(x1.nrr,x1.ncc);
  for(i=0;i<x1.length;i++) tempmat.ary[i]=x1.ary[i]+x2.ary[i];
return tempmat;
}

dmatrix operator-(const dmatrix &x1, const dmatrix &x2)
  {
int i;
  if ((x1.nrr!=x2.nrr)||(x1.length!=x2.length))
	throw dmatrix_size_error(x1.nrr, x1.ncc, x2.nrr, x2.length);
  dmatrix tempmat(x1.nrr,x1.ncc);
  for(i=0;i<x1.length;i++) tempmat.ary[i]=x1.ary[i]-x2.ary[i];
return tempmat;
}

dmatrix operator*(const dmatrix& x1,const dmatrix& b)
{
	int i,j,k;
	double xval,bval;
	if(x1.ncc!=b.nrr)
		throw dmatrix_size_error(x1.nrr, x1.ncc, b.nrr, b.length);
	dmatrix prod(x1.nrr,b.ncc);
	for(i=0;i<x1.nrr;i++)
	  for(j=0;j<b.ncc;j++)
	  {
		prod(i,j)=ddot(x1.ncc,&(x1.ary[i]),x1.nrr,
			&(b.ary[j*(b.nrr)]),1);
	  }
	return prod;
}

dmatrix operator*(const double& x, const dmatrix &zx)
  {
int i;
  dmatrix tempmat(zx.nrr,zx.ncc);
  for(i=0;i<zx.length;i++) tempmat.ary[i]=x*zx.ary[i];
return tempmat;
  }

dmatrix operator/(const dmatrix &zx, const double& x)
  {
int i;
  dmatrix tempmat(zx.nrr,zx.ncc);
  for(i=0;i<zx.length;i++) tempmat.ary[i]=zx.ary[i]/x;
return tempmat;
  }  


dmatrix tr(const dmatrix& x1)
{
int i,j;
dmatrix temp(x1.ncc,x1.nrr);
for(i=0; i<x1.nrr; i++)
   for(j=0; j<x1.ncc;j++)
   {
	temp(j,i)=const_cast<dmatrix&>(x1)(i,j);
   }
return temp;
}


ostream& operator<<(ostream& os, dmatrix& x1)
  {
  int i,j;
  for(i=0;i<x1.nrr;i++)
  {
  for(j=0;j<x1.ncc;j++) os << " "<< x1(i,j);
  os<<"\n";
  }
  return os;
  }

istream& operator>>(istream& is, dmatrix& x1)
  {
  int i,j;
  for(i=0;i<x1.nrr;i++)
  {
  for(j=0;j<x1.ncc;j++) is >> x1(i,j);
  }
  return is;
  }

void dmatrix::zero()
{
	for(int i=0;i<length;++i) ary[i]=0.0;
}
int *dmatrix::size()
{
	int *sz;
	sz=new int[2];
	sz[0]=nrr;
	sz[1]=ncc;
	return(sz);
}
// simpler versions of same

int dmatrix::rows()
{
	return(nrr);
}
int dmatrix::columns()
{
	return(ncc);
}

// vector methods
dvector& dvector::operator=(const dvector& other)
{
	if(this != &other)
	{
		ncc=1;
		nrr=other.nrr;
		length=other.length;
		if(ary!=NULL) delete [] ary;
		ary= new double[length];
		memcpy(ary,other.ary, length*sizeof(double));
	} 
	return *this;
}
dvector::dvector(const dvector& other)
{
	ncc=1;
	nrr=other.nrr;
	length=other.length;
	if(ary!=NULL) delete [] ary;
	ary= new double[length];
	memcpy(ary,other.ary, length*sizeof(double));
}
double &dvector::operator()(int rowindex)
{
  if (rowindex>=nrr)
	throw dmatrix_index_error(nrr,1,rowindex,1);
  return (ary[rowindex]);
}		
dvector operator*(const dmatrix& x1,const dvector& b)
{
	int i,j,k;
	double xval,bval;
        int ncx1=const_cast<dmatrix&>(x1).columns();
        int nrx1=const_cast<dmatrix&>(x1).rows();
        int nrb=const_cast<dvector&>(b).rows();
	if(ncx1!=nrb)
		throw dmatrix_size_error(nrx1, ncx1, nrb, 1);
	dvector prod(nrx1);
	for(i=0;i<nrx1;i++)
		prod(i)=ddot(nrb,
			const_cast<dmatrix&>(x1).get_address(i,0),ncx1,
			const_cast<dvector&>(b).get_address(0,0),1);
	return prod;
}
