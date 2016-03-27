/*modified from original numerical recipes program for double
precision */

#include   <stdio.h>
#include   <stdlib.h> 
#include   <math.h> 
#include   "C_CODE/c_util.h" 

#define SWAP(a,b) {double temp=(a);(a)=(b);(b)=temp;}
void gaussj();
double **dsubmatrix();

minvdbl(x,y,n,m)
double **x,**y;
int n,m;
  {
  int i,j;
  double **p,**pp;

  p=dsubmatrix(x,0,n,0,n,1,1);
  pp=dsubmatrix(y,0,n,0,m,1,1);

  gaussj(p,n,pp,m);
  }

void gaussj(a,n,b,m)
double **a,**b;
int n,m;
{
	int *indxc,*indxr,*ipiv;
	int i,icol,irow,j,k,l,ll,*ivector();
	double big,dum,pivinv;
    /*free_ivector();*/

	indxc=ivector(1,n);
	indxr=ivector(1,n);
	ipiv=ivector(1,n);
	for (j=1;j<=n;j++) ipiv[j]=0;
	for (i=1;i<=n;i++) {
		big=0.0;
		for (j=1;j<=n;j++)
			if (ipiv[j] != 1)
				for (k=1;k<=n;k++) {
					if (ipiv[k] == 0) {
						if (fabs(a[j][k]) >= big) {
							big=fabs(a[j][k]);
							irow=j;
							icol=k;
						}
					} else if (ipiv[k] > 1) nrerror("GAUSSJ: Singular Matrix-1");
				}
		++(ipiv[icol]);
		if (irow != icol) {
			for (l=1;l<=n;l++) SWAP(a[irow][l],a[icol][l])
			for (l=1;l<=m;l++) SWAP(b[irow][l],b[icol][l])
		}
		indxr[i]=irow;
		indxc[i]=icol;
		if (a[icol][icol] == 0.0) nrerror("GAUSSJ: Singular Matrix-2");
		pivinv=1.0/a[icol][icol];
		a[icol][icol]=1.0;
		for (l=1;l<=n;l++) a[icol][l] *= pivinv;
		for (l=1;l<=m;l++) b[icol][l] *= pivinv;
		for (ll=1;ll<=n;ll++)
			if (ll != icol) {
				dum=a[ll][icol];
				a[ll][icol]=0.0;
				for (l=1;l<=n;l++) a[ll][l] -= a[icol][l]*dum;
				for (l=1;l<=m;l++) b[ll][l] -= b[icol][l]*dum;
			}
	}
	for (l=n;l>=1;l--) {
		if (indxr[l] != indxc[l])
			for (k=1;k<=n;k++)
				SWAP(a[k][indxr[l]],a[k][indxc[l]]);
	}
	free_ivector(ipiv,1,n);
	free_ivector(indxr,1,n);
	free_ivector(indxc,1,n);
}

double **dsubmatrix(a,oldrl,oldrh,oldcl,oldch,newrl,newcl)
double **a;
int oldrl,oldrh,oldcl,oldch,newrl,newcl;
{
	int i,j;
	double **m;

	m=(double **) malloc((unsigned) (oldrh-oldrl+1)*sizeof(double*));
	if (!m) nrerror("allocation failure in submatrix()");
	m -= newrl;

	for(i=oldrl,j=newrl;i<=oldrh;i++,j++) m[j]=a[i]+oldcl-newcl;

	return m;
}

void free_dsubmatrix(b,nrl,nrh,ncl,nch)
double **b;
int nrl,nrh,ncl,nch;
{
	free((char*) (b+nrl));
}
