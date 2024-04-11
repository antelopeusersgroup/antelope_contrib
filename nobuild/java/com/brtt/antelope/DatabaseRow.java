/* Java class representing a Datascope database row
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Created by Tobin Fricke <tobin@splorg.org> on 2004-07-15
 */

package com.brtt.antelope;

import java.io.*;
import java.util.*;

/**
 * This class represents a row in a Datascope table.  Note: we have to
 * decide whether to return the String representation of a field, or 
 * parsed datatypes like ints, doubles, etc.  In a sense a DatabaseRow
 * is an instantiation of a DatabaseRelation.
 *
 * @author Tobin Fricke, University of California
 */

class DatabaseRow {

    /** Construct a DatabaseRow given the defining Relation and the textual
     *  representation of the row.  Note: this is an inefficient way to do 
     *  this.  For efficiency we should use memory-mapped I/O via FileChannel.
     *  If we did that, then we would have two types of DatabaseRow objects --
     *  those that are mutable and mapped to the on-disk storage, and those 
     *  that are free-floating.
     */

    public DatabaseRow(DatabaseRelation relation, String str) {
	for (Iterator i = relation.fields.iterator(); i.hasNext(); ) {

	    DatabaseAttribute attribute = (DatabaseAttribute)(i.getNext());
	    
	    /* It seems that we might want to defer parsing to the
	       DatabaseAttribute class.  But we'll do it here for now. */

	}

    }

    /** Retrieve the ith field from this row. Note: we might want to implement
     *  a collection interface (list List or Dictionary), since that's pretty much
     *  what a record is. */
 
    Object get(int i) {
	return fields.get(i);
    }

    public List fields;
}
