/*
 * Copyright (c) 2004 by the Regents of the University of California.
 *
 * Created 2004-07-08 by Tobin Fricke <tobin@splorg.org>
 *
 */

package com.brtt.antelope;

import java.util.*;
import java.io.*;


class SyntaxException extends java.lang.Throwable {
    public SyntaxException(java.lang.String str) {message = str; }
    private String message;
    public String toString() { return message; }
}
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

    public String timedate;

    /** Parse a textual description of  database schema into a DatabaseSchema
     *  object. */

    public static DatabaseSchema parse(Reader input) {

	try {
	    
	    DatabaseSchema schema = new DatabaseSchema();
	    DatabaseSchemaLexer lexer = new DatabaseSchemaLexer(input);
	    DatabaseSchemaToken token = null;
	    
	    while (null != (token = lexer.getToken())) {
		if (token.type == lexer.INCLUDE) {
		    parseIncludeStatement(schema, lexer);
		} else if (token.type == lexer.SCHEMA) {
		    parseSchemaStatement(schema, lexer);
		} else if (token.type == lexer.ATTRIBUTE) {
		    DatabaseAttribute attribute = DatabaseAttribute.parse(input);
		} else if (token.type == lexer.RELATION) {
		    DatabaseRelation relation = DatabaseRelation.parse(input);
		} else {
		    throw new SyntaxException("Error: unexpected token " + token); 
		}
	    }
	 
	} catch (IOException e) {
	    System.out.println("IOException "+e+"!"); // FIXME
	} catch (SyntaxException e) {
	    System.out.println("Syntax error: "+e);
	}


	    return null;
	}

    
    private static void parseIncludeStatement(DatabaseSchema schema, 
					     DatabaseSchemaLexer lexer) 
	throws SyntaxException {
    }


    /** Require that the next token is the given character.  Throw it
	away if it exists, throw a SyntaxException if it doesn't. */

    private static void expectChar(DatabaseSchemaLexer lexer, String chr) throws SyntaxException, IOException {
	DatabaseSchemaToken token = lexer.getToken();

	if (token.type == lexer.CHARACTER_LITERAL && (chr.compareTo((String)(token.value)) == 0)) {
	    /* Throw it away. */
	} else {
	    throw new SyntaxException("Expected character '"+chr+"' but got "+
				      "unexpected token "+token);
	}
    }

    private static String expectString(DatabaseSchemaLexer lexer) throws SyntaxException, IOException { 
	DatabaseSchemaToken token = lexer.getToken();
	if (token.type == lexer.STRING_LITERAL) {
	    return (String)(token.value);
	} else {
	    throw new SyntaxException("Expected a string literal, but found "+
				      "unexpected token "+token);
	}
    }

    private static String expectIdentifier(DatabaseSchemaLexer lexer) throws SyntaxException, IOException { 
	DatabaseSchemaToken token = lexer.getToken();
	if (token.type == lexer.IDENTIFIER_LITERAL) {
	    return (String)(token.value);
	} else {
	    throw new SyntaxException("Expected a string literal, but found "+
				      "unexpected token "+token);
	}
    }

    private static void parseSchemaStatement(DatabaseSchema schema, 
					     DatabaseSchemaLexer lexer) 
	throws SyntaxException, IOException {
	
	DatabaseSchemaToken token = lexer.getToken();
	
	if (token.type != lexer.IDENTIFIER_LITERAL) 
	    throw new SyntaxException("Error: unexpected token " + token) ;

	schema.name = (String)token.value;

	while (true) {
	    token = lexer.getToken();
	    
	    if (token.type == lexer.CHARACTER_LITERAL && (String)(token.value) == ";") {
		break;
	    } else if (token.type == lexer.DESCRIPTION) {
		expectChar(lexer,"(");
		schema.description = expectString(lexer);
		System.out.println("Got schema name " + schema.description);
		expectChar(lexer,")");
	    } else if (token.type == lexer.DETAIL) {
		schema.description = expectString(lexer);
	    } else if (token.type == lexer.TIMEDATE) {
		schema.timedate = expectIdentifier(lexer);
	    } else {
		throw new SyntaxException("Expected but did not find " +
					  "DESCRIPTION, DETAIL, TIMEDATE, or ;. Unexpected token " + token) ;
	    }
	}
    }
    
    /** Check that this Schema object is self-consistent.  Things to check for:
     *  (1) There should be no names in common between the 'relations' and 
     * 'attributes'
     *  dictionaries.  Every relation and attribute should be hashed under the
     *  correct name.  Every field name referenced should exist in one of these
     *  dictionaries.  The required fields should be filled in.  */

    public boolean isWellFormed() {
	return true; // FIXME
    }

    /** Produce a textual representation of this schema. */

    public String unparse() {
	return null; //FIXME
    }

    /** as a facility for testing, this class can be run from the 
	command line, taking a schema file as an argument.  The class will
	attempt to parse the given schema, check its validity, and spit it
	out again. */
    
    public static void main(String argv[]) {
	if (argv.length == 0) {
	    System.out.println("Usage : java DatabaseSchema <inputfile>");
	} else {
	    
	    for (int i = 0; i < argv.length; i++) {
		
		try {		    
		    System.out.println("Opening the file '" + argv[i] + "'.");
		    
		    Reader input = new java.io.FileReader(argv[i]);
		    
		    System.out.println("Parsing the schema.");
		    
		    DatabaseSchema schema = DatabaseSchema.parse(input);
		    
		    System.out.println("Checking the schema object for consistency.");
		    
		    boolean valid = schema.isWellFormed();
		    
		    System.out.println("Checking schema wellformedness: " + (valid ? "passed" : "FAILED")); 
		    
		    System.out.println("Dumping the schema from internal representation:");
		    
		    schema.unparse();
		    
		} catch (java.io.FileNotFoundException e) {
		    System.out.println("File not found : \""+argv[i]+"\"");
		} catch (java.io.IOException e) {
		    System.out.println("IO error scanning file \""+argv[i]+"\"");
		    System.out.println(e);
		} catch (Exception e) {
		    System.out.println("Unexpected exception:");
		    e.printStackTrace();
		}
	    }
	}
    }
    
}


