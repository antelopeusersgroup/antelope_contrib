/*
 * Copyright (c) 2004 by the Regents of the University of California.
 *
 * Created 2004-07-08 by Tobin Fricke <tobin@splorg.org>
 */

package com.brtt.antelope;

import java.util.*;
import java.io.*;

/** 
 * This class represents a relation (table format) in a Datascope schema.
 *
 * See dbschema(5) for more information.
 *
 * @author Tobin Fricke, University of California
 *
 */

public class DatabaseRelation {

    /** The name of this relation.  This field is required. Datascope
     *  allows the special form "Anonymous" to appear here, in which case
     *  a unique (but otherwise meaningless) name is generated.  This is
     *  not yet supported. */

    public String name;

    /** Specifies the attributes which make up a row in the table, in the order 
     *  in  which  they  appear.
     *  Design question: Do we want this to include the names of the attributes,
     *  or do we want to dereference the names to DatabaseAttribute
     *  objects?  Or maybe both, in a lazy evaluation fashion. This
     *  field is required.*/

    public List fields;

    /** List of the primary keys for the table. */

    public List primary;

    /** Listof the alternate keys for the table. */

    public List alternate;

    /** List of the foreign keys for the table. */

    public List foreign;

    /**  If  a  table  has an integer key which identifies a row in that table,
     *   it is specified with the Defines */

    public String Defines;

    /** List<String> of separators. */

    public List separator;

    /** Short description of the table. */

    public String description;

    /** Verbose description of the table. */

    public String detail;

    /** Parse a textual description of a DatabaseRelation.  This will most 
     *  likely only be called by DatabaseSchema.parse(). */

    public static DatabaseRelation parse(DatabaseSchemaLexer lexer) {
	DatabaseRelation relation = new DatabaseRelation();

	relation.name = expectIdentifier();

	while (true) {
	    token = lexer.getToken();
	    if (token.type == lexer.CHARACTER_LITERAL && ((String)(token.value)).compareTo(";")==0 ) {
		break;
	    } else if (token.type == lexer.FIELDS) {
		expectChar("(");
		while (true) {
		    token = lexer.getToken();
		    if (token.type == lexer.CHARACTER_LITERAL && ((String)(token.value)).compareTo(")")==0) {
			break;
		    } else if (token.type == lexer.IDENTIFIER_LITERAL) {
		    } else {
			// error
		    }
		}
	    } else if (token.type == lexer.PRIMARY) {
	    } else if (token.type == lexer.ALTERNATE) {
	    } else if (token.type == lexer.FOREIGN) {
	    } else if (token.type == lexer.DEFINES) {
		expectIdentifier();
	    } else if (token.type == lexer.SEPARATOR) {
		expectChar("(");
		expectString();
		expectChar(")");
	    } else if (token.type == lexer.DESCRIPTION) {
		expectChar(lexer,"(");
		schema.description = expectString(lexer);
		expectChar(lexer,")");
	    } else if (token.type == lexer.DETAIL) {
		schema.detail = expectString(lexer);
	    } else {
		throw SyntaxException("");
	    }
	     
	}  
	
    }

    /** Produce a textual description of this DatabaseRelation.  This will most
     *  likely only be called by DatabaseSchema.unparse(). */

    public String unparse() {
	String result = "";
	result += "Relation " + name + "\n";
	result += "   Fields (" + ") \n";
	result += "   Primary (" + ") \n";
	result += "   Alternate (" + ") \n";
	result += "   Foreign (" + ") \n";
	result += "   Defines " + "\n";
	result += "   Separator (" + ")\n";
	result += "   Description ( \"" + description + "\" )\n";
	result += "   Detail {" + detail + "}\n";
        result += "   ;";
	return result;
    }

    /** Verify whether this DatabaseRelation is self-consistent with respect to
     *  a given schema. */
    
    public boolean isWellFormed(DatabaseSchema schema) {

	if (!isWellFormed()) return false;

	return true;
    }

    /** Verify whether this DatabaseRelation is self-consistent.  */

    public boolean isWellFormed() {
	return true;
    }

}
