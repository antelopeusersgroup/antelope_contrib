/*
 * Copyright (c) 2004 by the Regents of the University of California.
 *
 * Created 2004-07-13 by Tobin Fricke <tobin@splorg.org>
 *
 */

package com.brtt.antelope;

/** This exception is thrown by the DatabaseSchema and ParameterFile parsers
    when an unexpected token is encountered. 

    @author Tobin Fricke, University of California
*/

public class SyntaxException extends java.lang.Throwable {
    
    /** Create a new SyntaxException with the given error message. The
        error message should be something like "Expected IDENTIFIER." */

    public SyntaxException(String message) {
	this.message = message; 
    }

    /** Create a new SyntaxException with the given error message and 
	include the unexpected token. The
        error message should be something like "Expected IDENTIFIER." */

    public SyntaxException(String message, DatabaseSchemaToken token) {
	this.message = message; 
	this.token = token;
    }
    
    /** Return a textual explanation of the error. */

    public String toString() { 
	return message + "Encountered at token (" + 
	    token + ")."; 
    }

    private String message;

    /** The erroneous token.  Fixme: change to a more general
	token type. */

    public DatabaseSchemaToken token;
}
