/*  This pair of get and put functions are required because the 
current version of antelope truncates doubles on a put to 6 significant
figures.  As a workaround times can be treated as date strings
and cracked with Dan Quinlan's time string functions.

Author:  Gary Pavlis
Written:  October 2002
*/
double pfget_time(Pf *pf,char *name)
{
	char *s;
	double time;
	s=pfget_string(pf,name);
	time=str2epoch(s);
	return(time);
}
void pfput_time(Pf *pf,char *name,double time)
{
	pfput_string(pf,name,strtime(time));
}
