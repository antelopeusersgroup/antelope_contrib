#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "mwp.h"

MODULE = atws_mwp		PACKAGE = atws_mwp		
PROTOTYPES: ENABLE

BOOT:
	//printf("xs: %i\n", __LINE__);

void
mwp_prepare(db, table, field, record, orid, sta, stime, etime)
	int db
	int table
	int field	
	int record
	int orid
	char* sta
	double stime
	double etime
	PREINIT:
	Dbptr pDb;
	int		nrecords;
	int 	i;
	float 	*dat;	
	double 	*mag;
	double 	*mom;
	PPCODE:
	
	pDb.database = db;
	pDb.table = table;
	pDb.field = field;
	pDb.record = record;
	nrecords= mwp_prepare(pDb, orid, sta, stime, etime, &dat, &mag, &mom);
	
	printf("Num Records Found: %i\n", nrecords);
	if(!nrecords)
		{
		EXTEND(sp,3);
		PUSHs(sv_2mortal(newSVpv("0", 0)));
		PUSHs(sv_2mortal(newSVpv("0", 0)));
		PUSHs(sv_2mortal(newSVpv("0", 0)));
		return;
		}
	EXTEND(sp,nrecords * 3);
	
	for (i= 0; i < nrecords; i++) 
		{
		PUSHs(sv_2mortal(newSVnv(dat[i])));
		PUSHs(sv_2mortal(newSVnv(mag[i])));
		PUSHs(sv_2mortal(newSVnv(mom[i])));
		}
	free(dat);
	free(mag);
	free(mom);
	
	
