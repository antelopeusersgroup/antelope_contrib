/* Copyright (c) 1997 Boulder Real Time Technologies, Inc. */
/* All rights reserved */

/* This software module is wholly owned by Boulder Real Time 
   Technologies, Inc. Any use of this software module without
   express written permission from Boulder Real Time Technologies,
   Inc. is prohibited. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "db.h"

int
maketmpdb (char *schema, Dbptr *db, char *dbname)

{
	FILE *f;

	sprintf (dbname, "/tmp/db%d", getpid());
	f = fopen(dbname, "w");
	if (f == NULL) {
		elog_log(1, "maketmpdb: fopen(%s) error.\n", dbname);
		return (-1);
	}
	if (fwrite (schema, strlen(schema), 1, f) != 1) {
		elog_log(1, "maketmpdb: fwrite(%s) error.\n", dbname);
		fclose (f);
		unlink (dbname);
		return (-1);
	}
	if (fwrite ("\n\n", 2, 1, f) != 1) {
		elog_log(1, "maketmpdb: fwrite(%s) error.\n", dbname);
		fclose (f);
		unlink (dbname);
		return (-1);
	}
	fclose (f);
	if (dbopen (dbname, "r+", db) == dbINVALID) {
		elog_log(1, "maketmpdb: dbopen(%s) error.\n", dbname);
		unlink (dbname);
		return (-1);
	}

	return (0);
}
