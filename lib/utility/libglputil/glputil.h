#include "stock.h"
#include "pf.h"
#include "db.h"
int check_required_pf(Pf *);
Dbptr dbform_working_view(Dbptr, Pf *, char *);
int dpinv_solver(int, int, double *,int, double *,double *,int,
	double *, double *,double);
int model_space_null_project(double *, int, int, int,double *, double *);
int model_space_range_project(double *, int, int, int,double *, double *);
int data_space_null_project(double *, int, int, int, double *, double *);
int ftest_subset(double, int, double, int, double);
int ftest1(double, int, double, int, double);
int ftest2(double, int, double, int, double);
