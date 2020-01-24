/* Java class representing Datascope database table
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Structure based on $DSAP/src/lib/dataformat/db/p_db.h from DSAP 3.3 (public domain)
 *
 * Created by Tobin Fricke <tobin@splorg.org> on 2004-07-09
 */

package com.brtt.antelope;

import java.io.*;
import java.util.*;

/**
 * This class represents a Datascope table.  A datascope 'table' is 
 * effectively an instance of a 'relation', which defines the attributes
 * present and their order and specifies which attributes are keys.  Likewise,
 * a 'field' is an instance of an attribute.  In Datascope, when a table is
 * instantiated, the corresponding file on disk is mapped into memory, which
 * provides very efficient read/write access to the table.  In Java, mmap is
 * not available to us.  We can still choose whether we want to parse the 
 * entire table at the time of loading, or on a per-field demand basis.  That
 * decision has not yet been made.  We could have it both ways, via the use
 * of subclasses that operate differently.
 *
 * @author Tobin Fricke, University of California 
 */

class DatabaseTable {

	/** Table name */
    	public String name;

	/** Table ID -- might not be necessary. */
	public int id; 

	/** Relation defining the structure of this Table. */
	DatabaseRelation rel;

	/** Description of how to create this table (what does this mean?). */
	String creation;

    // Arr            *index_arr;	       /* indexes on this table */

	/** number of records in this table */
	int count;

	/** name of physical file */
        String path;

	/** directory of file. file references are relative to this. */
	String dir;

     
//    int             fd;		       /* open file descriptor */

 //   size_t          size;	       /* length of mmap'ed data */
 //   size_t          maxsize;	       /* malloc'd size for a view only */
 //   char           *data;	       /* pointer to mmap'ed data */

    /** Is this table writeable? (If this flag is false, then the
        table is read-only.) */

    boolean        writeable;	  

  //  Dbindex        *dbindex;
  //  int            *tr_database;       /* translate table for database id */
  //  int            *tr_table;	       /* translate table for table id */
  //  char           *scratch;	       /* scratch record */

    /* Public Methods */

    public DatabaseTable(DatabaseRelation relation, String filename) {
	
    }

    public DatabaseTable(DatabaseRelation relation, Reader reader) {
	
    }

    public DatabaseRow getRow(int index) {
	return (DatabaseRow)(records.get(index));
    }

} 
