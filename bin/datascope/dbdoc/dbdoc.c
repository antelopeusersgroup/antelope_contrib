
#include <stdio.h>
#include <string.h>
#include "db.h"
#include "stock.h"

static void
usage (void)
{
    fprintf (stderr, "\nUsage: %s database\n",
	     Program_Name);
    banner(Program_Name, 0);
    exit (1);
}

char           *
type_name (long type)
{
    switch (type) {
	case dbREAL:return "real";
      case dbINTEGER:
	return "integer";
      case dbSTRING:
	return "string";
      case dbTIME:
	return "time";
      case dbYEARDAY:
	return "yearday";
    }
    return "unknown";
}

void 
format (FILE * out, char *fmt, char *s)
{
    char           *e;

    if (s == 0) {
	fputs ("\n", out);
	return;
    }
    while (*s != 0) {
	while (*s == ' ')
	    s++;
	if (*s == 0)
	    break;
	e = s + MIN (80, strlen (s));
	if (*e == 0) {
	    fputs (s, out);
	    fputs ("\n", out);
	    break;
	}
	while (*e != ' ' && e > s)
	    e--;

	*e = 0;
	fputs (s, out);
	fputs ("\n", out);
	*e = ' ';

	s = e + 1;
    }
}



void 
show_table_as_table (FILE * file, Pf * pf, Dbptr db, char *table_name)
{
    long             nfields;
    char           *desc;
    char           *field_name;
    long             type;
    char           *format;
    /* char           *units; */
    long             start,
                    end,
                    size;

    db = dblookup (db, 0, table_name, 0, 0);
    if (db.table < 0) {
	complain (0, "No such table in database: '%s'\n", table_name);
	return;
    }
    dbquery (db, dbTABLE_DESCRIPTION, &desc);
    fprintf (file, ".TS\nbox expand center;\n");
    fprintf (file, "lI s lB s s s\n");
    fprintf (file, "lI s l s s s\n");
    /* fprintf ( file, "lw(.5i) cw(.3i) cw(.5i) cw(.5i) cw(.7i) lw(2.5i).\n"
     * ) ; */
    fprintf (file, "l n l l r lw(2.5i).\n");
    fprintf (file, "Relation:\t%s\n", table_name);
    fprintf (file, "Description:\t%s\n", desc);
    fprintf (file, "_\n");
    fprintf (file, "field\t\t\tprint\tcharacter\tattribute\n");
    fprintf (file, "name\tno.\ttype\tformat\tpositions\tdescription\n");
    fprintf (file, "=\n");

    dbquery (db, dbFIELD_COUNT, &nfields);
    for (db.field = 0; db.field < nfields; db.field++) {
	dbquery (db, dbFIELD_NAME, &field_name);
	dbquery (db, dbFIELD_TYPE, &type);
	dbquery (db, dbFIELD_FORMAT, &format);
	/* dbquery ( db, dbFIELD_UNITS, &units ) ; */
	dbquery (db, dbFIELD_INDEX, &start);
	dbquery (db, dbFIELD_SIZE, &size);
	end = start + size;
	start++;
	dbquery (db, dbFIELD_DESCRIPTION, &desc);
	fprintf (file, "%s\t%ld\t%s\t%s\t%ld-%ld\t%s\n",
	     field_name, db.field + 1, type_name (type), format, start, end,
	/* units==0 ? "" : units, */
		 desc);
    }
    fprintf (file, ".TE\n");
    fprintf (file, ".LP\n");
}

void 
show_table_description (FILE * file, Pf * pf, Dbptr db, char *table_name)
{
    char           *detail;
    Tbl            *tbl;
    long             i,
                    n;

    db = dblookup (db, 0, table_name, 0, 0);
    if (db.table < 0) {
	complain (0, "No such table in database: '%s'\n", table_name);
	return;
    }
    fprintf (file, ".KS\n");
    fprintf (file, ".in 1i\n");
    fprintf (file, ".LP\n");
    fprintf (file, ".ta 1i 2i\n");
    fprintf (file, ".TI\n");
    fprintf (file, "Name:\t\\fB%s\\fP\n", table_name);

    fprintf (file, ".TI\n");
    fprintf (file, "Keys:\tPrimary:\t");
    dbquery (db, dbPRIMARY_KEY, &tbl);
    n = maxtbl (tbl);
    for (i = 0; i < n; i++)
	fprintf (file, "%s ", (char *) gettbl (tbl, i));
    fprintf (file, "\n");

    dbquery (db, dbALTERNATE_KEY, &tbl);
    n = maxtbl (tbl);
    if (n > 0) {
	fprintf (file, ".br\n");
	fprintf (file, "Alternate:\t");
	for (i = 0; i < n; i++)
	    fprintf (file, "%s ", (char *) gettbl (tbl, i));
	fprintf (file, "\n");
    }
    dbquery (db, dbFOREIGN_KEYS, &tbl);
    n = maxtbl (tbl);
    if (n > 0) {
	fprintf (file, ".br\n");
	fprintf (file, "Foreign:\t");
	for (i = 0; i < n; i++)
	    fprintf (file, "%s ", (char *) gettbl (tbl, i));
	fprintf (file, "\n");
    }
    fprintf (file, ".TI\n");
    dbquery (db, dbTABLE_DETAIL, &detail);
    fprintf (file, "Description:\t\n");
    format (file, "%s\n", detail);
    fprintf (file, ".in 0i\n");
    fprintf (file, ".LP\n");
    fprintf (file, ".Hl\n");
    fprintf (file, ".KE\n");
}

void 
show_field_description (FILE * file, Pf * pf, Dbptr db, char *field_name, char *used_in_tables)
{
    char           *table_name;
    char            null[STRSZ],
                   *np;
    char           *units,
                   *range,
                   *detail;
    char           *copy = strdup (used_in_tables);

    table_name = strtok (copy, " ");
    db = dblookup (db, 0, table_name, field_name, 0);
    if (db.table < 0) {
	complain (0, "No such table in database: '%s'\n", table_name);
	return;
    }
    free (copy);

    if (db.field < 0) {
	complain (0, "No such field in database: '%s'\n", field_name);
	return;
    }
    db.record = dbNULL;

    fprintf (file, ".KS\n");
    fprintf (file, ".IP Name: 14\n");
    fprintf (file, "\\fI%s\\fR\n", field_name);
    fprintf (file, ".IP Relation: 14\n");
    fprintf (file, "\\fB%s\\fR\n", used_in_tables);

    dbquery (db, dbFIELD_DETAIL, &detail);
    fprintf (file, ".IP Description: 14\n");
    format (file, "%s\n", detail);

    dbget (db, null);
    for (np = null; *np == ' '; np++);
    fprintf (file, ".IP \"NULL value:\" 14\n");
    fprintf (file, "%s\n", np);

    dbquery (db, dbFIELD_UNITS, &units);
    if (units != 0 && *units != 0) {
	fprintf (file, ".IP Units: 14\n");
	fprintf (file, "%s\n", units);
    }
    dbquery (db, dbFIELD_RANGE, &range);
    if (range != 0 && *range != 0) {
	fprintf (file, ".IP Range: 14\n");
	fprintf (file, "%s\n", range);
    }
    fprintf (file, ".LP\n");
    fprintf (file, ".Hl\n");
    fprintf (file, ".KE\n");
}

void 
catfile (FILE * out, char *filename)
{
    FILE           *in;
    char            aline[STRSZ];
    char           *rdata;

    if ((in = fopen (filename, "r")) == 0) {
	if ((rdata = datapath ("", "doc/dbdoc", filename, "")) == 0) {
	    complain (1, "Can't open file '%s'\n", filename);
	    return;
	} else {
	    if ((in = fopen (rdata, "r")) == 0) {
		complain (1, "Can't open file '%s'\n", filename);
		free (rdata);
		return;
	    }
	    free (rdata);
	}
    }
    while (fgets (aline, STRSZ, in) != 0)
	fputs (aline, out);
}

int
main (int argc, char **argv)
{
    Dbptr           db;
    long             i,
                    nfields;
    char           *table_name;
    char            list_of_tables[STRSZ];
    char           *used_in_tables;
    Tbl            *fields_tbl;
    Arr            *fields_arr;
    char           *field_name;
    Pf             *pf;
    Tbl            *tables;
    char           *filename;

    elog_init (argc, argv);

    if (argc > 1 && strcmp (argv[1], "-V") == 0) {
	banner (Program_Name, 0) ;
	exit (0);
    }
    if (argc != 2)
	usage ();

    if (dbopen (argv[1], "r", &db) != 0)
	die (1, "Can't open database %s", argv[1]);

    if (pfread (Program_Name, &pf) != 0)
	die (1, "Can't read parameter file\n");

    tables = pfget_tbl (pf, "tables");
    if (maxtbl(tables) == 0) {
	dbquery (db, dbSCHEMA_TABLES, &tables);
    }
    filename = pfget_string (pf, "intro");
    catfile (stdout, filename);

    filename = pfget_string (pf, "relation_tables_intro");
    catfile (stdout, filename);

    for (i = 0; i < maxtbl(tables); i++) {
	table_name = (char *) gettbl (tables, i);
	show_table_as_table (stdout, pf, db, table_name);
    }

    filename = pfget_string (pf, "relation_desc_intro");
    catfile (stdout, filename);

    for (i = 0; i < maxtbl(tables); i++) {
	table_name = (char *) gettbl (tables, i);
	show_table_description (stdout, pf, db, table_name);
    }

    fields_arr = newarr (0);
    for (i = 0; i < maxtbl(tables); i++) {
	table_name = (char *) gettbl (tables, i);
	db = dblookup (db, 0, table_name, 0, 0);
	dbquery (db, dbFIELD_COUNT, &nfields);
	for (db.field = 0; db.field < nfields; db.field++) {
	    dbquery (db, dbFIELD_NAME, &field_name);
	    used_in_tables = getarr (fields_arr, field_name);
	    if (used_in_tables != 0) {
		strcpy (list_of_tables, used_in_tables);
		strcat (list_of_tables, " ");
		free (used_in_tables);
	    } else
		*list_of_tables = 0;
	    strcat (list_of_tables, table_name);
	    setarr (fields_arr, field_name, strdup (list_of_tables));
	}
    }

    filename = pfget_string (pf, "attribute_desc_intro");
    catfile (stdout, filename);

    fields_tbl = keysarr (fields_arr);
    nfields = maxtbl (fields_tbl);
    for (i = 0; i < nfields; i++) {
	field_name = (char *) gettbl (fields_tbl, i);
	used_in_tables = getarr (fields_arr, field_name);
	show_field_description (stdout, pf, db,
				field_name, used_in_tables);
    }

    return 0;
}

/* $Id$ */
