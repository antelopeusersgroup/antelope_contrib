/*
 * Copyright (c) 2004 by the Regents of the University of California.
 *
 * Created 2004-07-08 by Tobin Fricke <tobin@splorg.org>
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

public class DatabaseAttribute {

    /** Name of this attribute.  This field is required. */

    public String name;

    /** Type of the field; allowable values are "Real," "Integer," "String,"
	"Time," "Date," and "YearDay."  This field is required. */
    
    public String type;          // should actually be an enumeration

    /** Size of the field in characters. */

    public int size;

    /** The preferred format (in printf form) for converting the value to text.
        This field is required. */

    public String formatStr;

    /** Units in which this attribute is given.  This is just a character string
     *  accessible via such tools as dbquery. */

    public String units;

    /** An expression which may be evaluated to determine whether a given value
     *  of this attribute is within range.  The expression language is not
     *  given here (where is it given?) and is currently ignored by this
     *  implementation. */

    public String range;

    /** The Null value for this attribute.  This value must conform to the 
     *  formatStr, and must fit within the size of the field when formatted
     *  according to the formatStr. */

    public String nullvalue;

    /** Short description of the attribute.  This value is accessible through 
     *  tools such as dbquery. */

    public String description;

    /** Verbose description of the attribute.  This value is accessible through
     *  tools such as dbquery. */

    public String detail;

    /** Parse a textual description of a DatabaseAttribute.  This will most 
     *  likely only be called by DatabaseSchema.parse(). */

    public static DatabaseAttribute parse(InputStream input) {
      return null;
    }

    /** Produce a textual description of this DatabaseAttribute.  This will most
     *  likely only be called by DatabaseSchema.unparse(). */

    public String unparse() {
      return null;
    }

    /** Verify whether this DatabaseAttribute is self-consistent. */
    
    public boolean isWellFormed() {
      return true;
    }

}
