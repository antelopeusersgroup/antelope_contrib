/*
 * Copyright (c) 2004 by the Regents of the University of California.
 *
 * Created 2004-07-08 by Tobin Fricke <tobin@splorg.org>
 *
 */

package com.brtt.antelope;

import java.util.*;
import java.io.*;

/** 
 * This class represents a Datascope database [descriptor], which gives the
 * schema(s) and the table location(s) and name(s) for a database.  As such,
 * the class doesn't really do more than find all the necessary files and
 * call the necessary methods to get them all parsed.  The real work will
 * occur in DatabaseSchema, DatabaseTable, etc.
 *
 * @author Tobin Fricke, University of California
 */

/** 
 *  TableLoc (what should it be named?) is a helper class that just serves
 *  to provide (path,basename) pairs that will be elements in the table search
 *  path for a database.  Is there already a Pair(Object,Object) class?  We
 *  could even use JDK 1.5's generics to make a nice Pair<String,String>. (-:
 */

class TableLoc {
    String path;
    String basename;

    TableLoc(String path, String basename) {
	this.path = path;
	this.basename = basename;
    }

}

/** This class represents a descriptor for a Datascope database, that 
 *  contains a list of schemas and a list of (directory, basename) tuples
 *  that provide a search path for tables. 
 *
 *  @author Tobin Fricke, University of California
 */

public class Database {

    /** Contains the schema that describe the format of this database. 
     *  All schemas listed for this database are merged into this object.
     */

    DatabaseSchema schema;

    /** Contains a list of directories and basenames (combined using the
     *  TableLoc class to provide pairs) that forms a search path for tables
     *  in the database.  The addSource() and addSources() methods are used
     *  to add an entry to this path. */

    private List sources;

    /** Add a list of sources in the format "path{name}:path{name}:...".
     *   */

    public void addSources(String sourcelist) {
	
	/* First split this string into fields separated by colons */
	
	/* This could all be done in two lines of Perl. */

	while (sourcelist.length() > 0) {
	    
	    String source;

	    /* Pop off the first element of the list */

	    if (sourcelist.indexof(":") >= 0) {
		source = sourcelist.indexof(":");
		sourcelist = sourcelist.subString(sourcelist.indexof(":")+1, 
						  sourcelist.length());
	    } else {
		source = sourcelist;
		sourcelist = "";
	    }

	    /* Now, split into the format foo{bar} or just foo */

	    if (source.indexof("{") >= 0) {
		directory = source.subString(0,source.indexof("{"));

		if (source.indexOf("}") < 0) {
		    // FixMe: throw an error
		}

		basename = source.subString(source.indexOf("{")+1,
					    source.indexOf("}"));
		
		if (source.indexOf("}") < source.length()) {
		    // FixMe: generate a warning about spurious extra characters
		}

	    } else {
		directory = source;
	    }

	    /* Now we have the top item of the list parsed into 'directory'
	       and 'basename'. */

	    addSource(directory, basename);
	}
	
    }

    /** Add a given path and basename to the tables search path. */

    public void addSource(String path, String basename) {
 
        /* I'm not sure whether there's actually any value in this lazy
           object creation, or whether we might as well just allocate these
           containers from the get-go. */
        
	if (sources == null)
	    sources = new ArrayList();

	sources.add(new TableLoc(path, basename));
    }

    /** Add a given schema to the database's schema list.  The directory 
     *  containing the descriptor file is searched for the schema definition,
     *  and if it is not found there, the directory $ANTELOPE/data/schemas is
     *  searched.  Note: this means we have to remember the directory that 
     *  contained the descriptor file. 
     *
     *  Attribute and Relation names must be unique across all schemas;
     *  that is, a given name corresponds to at most one object, either
     *  an Attribute or a Relation, in this set of schemas.   Note: should we 
     *  verify the uniqueness of names, and throw an exception if it is 
     *  violated, or should we trust the user?  Probably the former. (-:
     */
    
    public void addSchema(String schemaName) {

	/* First we have to find the schema file. */

	String filename;

	Reader reader = new FileReader(filename);

	/* Now we compile the Schema. */

	DatabaseSchema schema = new DatabaseSchema(reader);

	/* Now combine this schema with the existing schema (if any). */

	if (this.schema == null)
	    this.schema = schema;
	else
	    this.schema.merge(schema);
	    
    }

    /** Add the given schema to the database's schema list.  In actual fact
     *  we will probably just merge it in with a single schema object 
     *  describing the database. */
 
    public void addSchema(DatabaseSchema schema) {
	schemas.add(schema);
    }

    /** Get the table with the given name.  Once the database descriptor is
     *  all set up (either read from a file or constructed programatically),
     *  this is the method that will ultimately be called to get a table, which
     *  is what the user is after anyway. */

    public DatabaseTable getTable(String tableName) {

	/* Search for the Relation definition of this table. */

	if (!schema.relations.contains(tableName)) {
	    // Fixme: throw an exception
	}

	DatabaseRelation relation = (DatabaseRelation)(schema.relations.get(tableName));

	/* Search for the table in the 'sources' list. */

	String filename = null;

	/* Now we have all of the information we need. */

	return new DatabaseTable(relation, filename);

    }
}
