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
 * This class represents a Datascope schema.
 *
 * @author Tobin Fricke, University of California
 *
 */

public class DatabaseSchema {

    /** Mapping from names to DatabaseRelations, defining the relations (tables)
     *  that exist in this schema. Note that this is the same namespace as
     *  'attributes'. */

    public Dictionary relations;

    /** Mapping from names to DatabaseAttributes, defining the attributes (fields)
     *  that exist in this schema.  Note that this is the same namespace as 
     *  'relations'. */

    public Dictionary attributes;

    /** The name of this schema, a short string like "css3.0".  This field is
     *  required. */

    public String name;

    /** A short description of this schema. */

    public String description;

    /** A verbose description of this schema. */

    public String detail;

    /** (What is this?) */

    public double lddate;

    /** Parse a textual description of  database schema into a DatabaseSchema
     *  object. */

    public static DatabaseSchema parse(InputStream input) {

	DatabaseSchema schema = new DatabaseSchema();

/*
	while (token == getToken(input)) {
	    if (token.comapreTo("Include")) {
		parseIncludeStatement(schema, input);
	    } else if (token.compareTo("Schema")) {
		parseSchemaStatement(schema, input);
	    } else if (token.compareTo("Attribute")) {
		DatabaseAttribute attribute = DatabaseAttribute.parse(input);
	    } else if (token.compareTo("Relation")) {
		DatabaseRelation relation = DatabaseRelation.parse(input);
	    } else {
		// complain and fail
		return null;
	    }
	}
*/
      return null;
    }

    public static void parseIncludeStatement(DatabaseSchema schema, 
					     InputStream input) {
    }

    public static void parseSchemaStatement(DatabaseSchema schema, 
					    InputStream input) {
    }

    /** Check that this Schema object is self-consistent.  Things to check for:
     *  (1) There should be no names in common between the 'relations' and 
     * 'attributes'
     *  dictionaries.  Every relation and attribute should be hashed under the
     *  correct name.  Every field name referenced should exist in one of these
     *  dictionaries.  The required fields should be filled in.  */

    public boolean isWellFormed() {
      return true;
    }

    /** Produce a textual representation of this schema. */

    public String unparse() {
      return null;
    }
}
