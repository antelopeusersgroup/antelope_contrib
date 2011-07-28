/*  for writing wf-data --> sets o/p to FLOATS
	access is from FORTRAN  */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/param.h>
#include "db.h"

int
writemywf_(db, sta, chan, time, samprate, nsamp, calib, instype, dir, dfile, data)
Dbptr	*db;
char	*sta, *chan;
double	*time, *samprate;
int	*nsamp ;
double	*calib;
char	*instype, *dir, *dfile;
float 	*data;
{
	int added;
	double endtime;
	int foff = 0;
	char datatype[4];

	strcpy(datatype, "t4");
	endtime = *time + (double)(*nsamp - 1)/(*samprate);

	added = dbaddv(*db, 0, "sta", sta, "chan", chan,
	"time", *time, "samprate", *samprate,
	"nsamp", *nsamp, "calib", *calib, "foff", foff,
	"endtime", endtime,
	"instype", instype, "datatype", datatype,
	"dir", dir, "dfile", dfile,
	0);
	if (added<0) elog_complain(0, "dbaddv wf error");
	db->record = added;

	if (trputwf(*db, data)<0) elog_complain(0, "dbputwf wf error");

}
