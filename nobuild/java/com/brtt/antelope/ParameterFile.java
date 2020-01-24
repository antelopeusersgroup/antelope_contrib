/* Java class representing a Datascope parameter file
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Created by Tobin Fricke <tobin@splorg.org> on 2004-07-14
 *
 * Based on DSAP 3.3
 */

package com.brtt.antelope;

import java.io.*;
import java.util.*;

/** This class represents a Datascope parameter file, and will be able to
 *  both parse and construct them.  Right now it does nothing. 
 *
 *  @author Tobin Fricke, University of California
 */

class ParameterFile {

    /** Read a parameterfile from the given Reader. This
     *  corresponds to the API function pfin(). */

    public ParameterFile(Reader in) {
    }

    /** Read a parameterfile from the given filename.  This
     *  corresponds to the API function pfread().  */

    public ParameterFile(String filename) {
	// can we call one constructor from another?
	// ParameterFile(new FileReader(filename));
    }

    /** Produce a parameterfile from the given string. This corresponds to the 
     *  C function pfcompile(). */

    public static ParameterFile compile(String s) {
	return new ParameterFile(new StringReader(s));
    }

    /** Write the ParameterFile represented by this object out to a file on
     *  disk (or a string, or standard output, etc).  Note: the equivalent
     *  method is called "unparse" in some other classes. */

    public void write(Writer w) {
    }

    /** Provide a string representation of this ParameterFile object. Should
     *  this be something short and witty, or should we serialize the whole
     *  ParameterFile? */

    public String toString() {
	// is there a StringWriter class?
        return null;
    }

    /** Get a field from the parameter file, returned as a java object. Note:
     *  maybe we could/should implement the interface for some java container
     *  type? */

    public Object get(String name) {
      return null;
    }

    /* Now we could have get_boolean, get_double, get_int, get_string,
       get_tbl, get_arr. */

    /** Return a list of the keys of this parameter file. */

    public List keys() {
      return null;
    }

    /** Add a field to this parameter file. */

    public void put(String name, Object obj) {
    }

    /* put_boolean, put_double, put_int, put_string, put_arr, put_tbl */
}
