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
 * This class represents, parses, and unparses a Datascope schema.
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
     *  object. Note: why don't we just turn this into a constructor? */

    public static DatabaseSchema parse(Reader input) {

	DatabaseSchema schema = new DatabaseSchema();
    
	try {
	    
	    DatabaseSchemaLexer lexer = new DatabaseSchemaLexer(input);
	    DatabaseSchemaToken token = null;
	    
	    while (null != (token = lexer.getToken())) {
		if (token.type == lexer.INCLUDE) {
		    parseIncludeStatement(schema, lexer);
		} else if (token.type == lexer.SCHEMA) {
		    parseSchemaStatement(schema, lexer);
		} else if (token.type == lexer.ATTRIBUTE) {
		    DatabaseAttribute attribute = DatabaseAttribute.parse(lexer);
		    if (schema.attributes == null)
			schema.attributes = new Hashtable();
		    schema.attributes.put(attribute.name, attribute);
		} else if (token.type == lexer.RELATION) {
		    DatabaseRelation relation = DatabaseRelation.parse(lexer);
		    if (schema.relations == null)
			schema.relations = new Hashtable();
		    schema.relations.put(relation.name, relation);
		} else {
		    throw new SyntaxException("Expected INCLUDE, SCHEMA, ATTRIBUTE, or RELATION while parsing SCHMADEF. ",token); 
		}
	    }
	    
	} catch (IOException e) {
	    System.out.println("IOException "+e+"!"); // FIXME
	} catch (SyntaxException e) {
	    System.out.println("Syntax error: "+e);
	}
	
	return schema;
    }
    
    
    private static void parseIncludeStatement(DatabaseSchema schema, 
					     DatabaseSchemaLexer lexer) 
	throws SyntaxException, IOException {
	String include = lexer.expectIdentifier();
	/* FIXME: parse the included schema, and merge it into this one. */
    }

    private static void parseSchemaStatement(DatabaseSchema schema, 
					     DatabaseSchemaLexer lexer) 
	throws SyntaxException, IOException {
	
	schema.name = lexer.expectIdentifier();

	while (true) {
	    DatabaseSchemaToken token = lexer.getToken();
	    if (token.type == lexer.CHARACTER_LITERAL && ((String)(token.value)).compareTo(";")==0 ) {
		break;
	    } else if (token.type == lexer.DESCRIPTION) {
		lexer.expectChar("(");
		schema.description = lexer.expectString();
		lexer.expectChar(")");
	    } else if (token.type == lexer.DETAIL) {
		schema.detail = lexer.expectString();
	    } else if (token.type == lexer.TIMEDATE) {
		schema.timedate = lexer.expectIdentifier();
	    } else {
		throw new SyntaxException("Expected but did not find " +
					  "DESCRIPTION, DETAIL, TIMEDATE, or ';'.", 
					  token) ;
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

    public void unparse(Writer w) throws IOException{

	w.write("Schema " + name + "\n");
	if (description != null)
	    w.write("  Description ( \"" + description + "\" )\n");
	if (detail != null)
	    w.write("  Detail {" + detail + "}\n");
	if (timedate != null)
	    w.write("  Timedate " + timedate + "\n");
	w.write("  ;\n\n");
	
	for (Enumeration e = attributes.elements(); e.hasMoreElements(); ) {
	    DatabaseAttribute attribute = (DatabaseAttribute)(e.nextElement());
	    attribute.unparse(w);
	    w.write("\n");
	}

	for (Enumeration e = relations.elements(); e.hasMoreElements(); ) {
	    DatabaseRelation relation = (DatabaseRelation)(e.nextElement());
	    relation.unparse(w);
	    w.write("\n");
	}
	
    }

    public void unparseAsXML(Writer w) throws IOException {

	w.write("<schema name=\"" + name + "\"");
	if (description != null)
	    w.write(" description=\"" + description + "\"");

	if (timedate != null)
	    w.write(" timedate=\"" + timedate + "\"");
	w.write(">\n");

	if (detail != null) {
	    w.write("  <detail>" + detail + "</detail>\n");
	}

	for (Enumeration e = attributes.elements(); e.hasMoreElements(); ) {
	    DatabaseAttribute attribute = (DatabaseAttribute)(e.nextElement());
	    attribute.unparseAsXML(w);
	}

	for (Enumeration e = relations.elements(); e.hasMoreElements(); ) {
	    DatabaseRelation relation = (DatabaseRelation)(e.nextElement());
	    relation.unparseAsXML(w);
	}

    }

    /** as a facility for testing, this class can be run from the 
	command line, taking a schema file as an argument.  The class will
	attempt to parse the given schema, check its validity, and spit it
	out again. */
    
    public static void main(String argv[]) {
	if (argv.length == 0) {
	    System.out.println("Usage : java DatabaseSchema <inputfile> -action1 -action2 ...");
	    System.out.println("Where the available actions are \"toXML\" and \"regurgitate\".");
	} else {
	    
	    DatabaseSchema schema = null;

	    for (int i = 0; i < argv.length; i++) {
		
		try {	

		    if (argv[i].charAt(0) == '-') {
                        if (schema == null) {
                          System.err.println("Error: command specified before schema loaded.. remember, the schema file goes *before* the action.");
                          continue;
                        }
			
			if (argv[i].compareTo("-toXML") == 0) {
			    BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));	
			    schema.unparseAsXML(w);
			    w.flush();
			} else if (argv[i].compareTo("-regurgitate") == 0) {
			    BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));	
			    schema.unparse(w);
			    w.flush();
			}
			
		    } else {

			/* Assume this is a schema file.  Load and parse it. */

			Reader input = new java.io.FileReader(argv[i]);
			schema = DatabaseSchema.parse(input);
		    }
		    
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


