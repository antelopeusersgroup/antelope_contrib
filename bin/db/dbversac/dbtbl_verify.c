
int
dbtbl_verify (Dbptr db, int check_range)
{
    Tbl            *primary,
                   *alternate,
                   *foreign;

    dbquery (db, dbRECORD_COUNT, &nrecords);
    dbquery (db, dbPRIMARY_KEY, &primary);
    dbquery (db, dbALTERNATE_KEY, &alternate);
    dbquery (db, dbFOREIGN_KEY, &foreign);
    nforeign = maxtbl (foreign);

    /* compute index on primary key, verify unique */
    /* compute index on alternate key, verify unique */
    /* for "lookup tables" (done last) verify all records are referenced */

    for (db.record = 0; db.record < nrecords; db.record++) {

	for (i = 0; i < nforeign; i++) {
	    fkey = gettbl (foreign, i);
	    dbgetv (db, 0, fkey, &value, NULL);
	    /* check for existence in foreign table, mark referenced */
	}

	for (i = 0; i < nprimary; i++) {
	    pkey = gettbl (primary, i);
	    dbgetv (db, 0, pkey, &value, NULL);
	    /* check for non-null */
	}

	for (i = 0; i < nalternate; i++) {
	    pkey = gettbl (alternate, i);
	    dbgetv (db, 0, pkey, &value, NULL);
	    /* check for non-null */
	}

	if (check_range) {
	    for (i = 0; i < nfields; i++) {
		field = gettbl (fields, i);
		dbgetv (db, 0, field, &value, NULL);
		/* check for either null or in range */
	    }
	}
	if (extfile) {
	    /* check for existence and readability of external file */
	}
    }
}

/* $Id$ */
