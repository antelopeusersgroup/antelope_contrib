/* Scans the arrival time list set in previous routine to return the
maximum and minimum times found.  Needed to define processing time
windows.

Arguments:
	a - associative array keyed by station name set by get_arrivals 
		function above.
	tmin, tmax - minimum and maximum arrival times in list 

Author:  Gary Pavlis
*/
void get_arrival_time_range(Arr *a, double *tmin, double *tmax)
{
	Tbl *t;  /* holds keys */
	char *key;  
	int i;
	double *value;
	
	t = keysarr(a);
	key = gettbl(t,0);
	tmin = (double *)getarr(a,key);
	*tmax = *tmin;
	for(i=1;i<maxtbl(t);++i)
	{
		key = gettbl(t,i);
		value = (double *)getarr(a,key);
		*tmin = MIN(*tmin,*value);
		*tmax = MAX(*tmax,*value);
	}
	freetbl(t,0);
}
