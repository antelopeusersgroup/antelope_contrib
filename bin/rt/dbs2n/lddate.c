#include <stdio.h>

char * 
lddate ( )

{
	double epoch;
	int iyear, imonth, iday, ihour, iminute;
	double sec;
	static char str[128];

	epoch = time(NULL) + 1.0;
	e2h (epoch, &iyear, &iday, &ihour, &iminute, &sec);
	doy2mday (iday, iyear, &imonth, &iday);
	sprintf (str, "%2.2d/%2.2d/%2.2d", imonth, iday, iyear-1900);
	return (str);
}
