/* Java class representing Datascope database table
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Structure based on $DSAP/src/lib/dataformat/db/p_db.h from DSAP 3.3 (public domain)
 *
 * Created by Tobin Fricke <tobin@splorg.org> on 2004-07-09
 */

typedef struct Table
{				       /* corresponding to actual file */
    char           *name;	       /* table name */
    int             id;		       /* table id */
    Relation       *rel;	       /* pointer to relation */
    char           *creation;	       /* description of how to create table */
    Arr            *index_arr;	       /* indexes on this table */
    int             count;	       /* count of records in table */

    char           *path;	       /* name of physical file */
    char           *dir;	       /* directory of file -- for file
				        * references */

    int             fd;		       /* open file descriptor */

    size_t          size;	       /* length of mmap'ed data */
    size_t          maxsize;	       /* malloc'd size for a view only */
    char           *data;	       /* pointer to mmap'ed data */

    int             writeable;	       /* flag = true if file is writeable */

    Dbindex        *dbindex;
    int            *tr_database;       /* translate table for database id */
    int            *tr_table;	       /* translate table for table id */
    char           *scratch;	       /* scratch record */
}               Table;
