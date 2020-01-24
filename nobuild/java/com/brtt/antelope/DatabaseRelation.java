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

    public String defines;

    /** String that separates individual records (by default, "\n"). */

    public String recordSeparator;

    /** String that separates individual fields (by default, " " [or ""?]). 
     *  DataScope requires that fields have a fixed number of characters, so
     *  the recordSeparator and fieldSeparator are pretty much just fluff.
     *  This can save us effort in parsing, since we can explicity calculate
     *  the character range corresponding to a given field in a given record.
     *  But it might be quite useful to allow a second mode of processing, 
     *  where the tables are actually parsed according to these separators
     *  and some (not yet invented) quoting convention.  Then we could use
     *  a Datascope-style interface to access comma-separated-values files, 
     *  etc, which would be pretty handy. */

    public String fieldSeparator;

    /** Short description of the table. */

    public String description;

    /** Verbose description of the table. */

    public String detail;


    /** Parse a textual description of a DatabaseRelation.  This will most 
     *  likely only be called by DatabaseSchema.parse(). */

    public static DatabaseRelation parse(DatabaseSchemaLexer lexer) 
	throws SyntaxException, IOException {
	DatabaseRelation relation = new DatabaseRelation();

	relation.name = lexer.expectIdentifier();

	while (true) {
	    DatabaseSchemaToken token = lexer.getToken();
	    if (token.type == lexer.CHARACTER_LITERAL && ((String)(token.value)).compareTo(";")==0 ) {
		break;
	    } else if (token.type == lexer.FIELDS) {
		relation.fields = lexer.parseIdentifierList(relation.fields);
	    } else if (token.type == lexer.PRIMARY) {
		relation.primary = lexer.parseIdentifierList(relation.primary);
	    } else if (token.type == lexer.ALTERNATE) {
		relation.alternate = lexer.parseIdentifierList(relation.alternate);
	    } else if (token.type == lexer.FOREIGN) {
		relation.foreign = lexer.parseIdentifierList(relation.foreign);
	    } else if (token.type == lexer.DEFINES) {
		relation.defines = lexer.expectIdentifier();
	    } else if (token.type == lexer.SEPARATOR) {
		lexer.expectChar("(");
		relation.fieldSeparator = lexer.expectString();
		// FIXME: get recordSeparator
		lexer.expectChar(")");
	    } else if (token.type == lexer.DESCRIPTION) {
		lexer.expectChar("(");
		relation.description = lexer.expectString();
		lexer.expectChar(")");
	    } else if (token.type == lexer.DETAIL) {
		relation.detail = lexer.expectString();
	    } else {
		throw new SyntaxException("Expected FIELDS, PRIMARY, ALTERNATE, FOREIGN, SEPARATOR, DESCRIPTION, DETAIL, or ';' while parsing RELATION.");
	    }
	     
	}  

	return relation;
    }

    private String unparseList(List l) {
	String s = "";
	if (l == null) return "(null)";
	for (Iterator i = l.iterator(); i.hasNext(); ) {
	    Object o = i.next();
	    if (s.length() > 0) s += " ";
	    s += o.toString();
	}
	return s;
    }

    /** Produce a textual description of this DatabaseRelation.  This will most
     *  likely only be called by DatabaseSchema.unparse(). */

    public void unparse(Writer w) throws IOException {

	w.write("  Relation " + name + "\n");
	if (fields != null) 
	    w.write("   Fields (" + unparseList(fields) + ") \n");
	if (primary != null)
	    w.write("   Primary (" + unparseList(primary) + ") \n");
	if (alternate != null)
	    w.write("   Alternate (" + unparseList(alternate) + ") \n");
	if (foreign != null)
	    w.write("   Foreign (" + unparseList(foreign) + ") \n");
	if (defines != null)
	    w.write("   Defines " + defines + "\n");
	if (fieldSeparator != null || recordSeparator != null) 
	    w.write("   Separator ( " +
		    (fieldSeparator != null  ? 
		     "\"" + fieldSeparator + "\" " : "") +
		    (recordSeparator != null ? 
		     "\"" + recordSeparator +"\" " : "") +
		    ")\n");
	if (description != null)
	    w.write("   Description ( \"" + description + "\" )\n");
	if (detail != null) 
	    w.write("   Detail {" + detail + "}\n");
        w.write("   ;\n");
    }

    public void unparseAsXML(Writer w) throws IOException {

	w.write("  <relation name=\"" + name + "\"");
	if (fields != null) 
	    w.write(" fields=\"" + unparseList(fields) + "\"");
	if (primary != null)
	    w.write(" primary=\"" + unparseList(primary) + "\" ");
	if (alternate != null)
	    w.write(" alternate=\"" + unparseList(alternate) + "\"");
	if (foreign != null)
	    w.write(" foreign=\"" + unparseList(foreign) + "\"");
	if (defines != null)
	    w.write(" defines=\"" + defines + "\"");
	if (fieldSeparator != null)
	    w.write(" fieldseparator=\"" + fieldSeparator + "\"");
	if (recordSeparator != null) 
	    w.write(" recordseparator=\"" + recordSeparator + "\"");
	if (description != null)
	    w.write(" description=\"" + description + "\"");
	
	if (detail != null) 
	    w.write(">\n    <detail>" + detail + "</detail>\n  </relation>\n");
	else
	    w.write("/>\n");
    }

    /** Verify whether this DatabaseRelation is self-consistent with respect to
     *  a given schema.  Currently returns the same value as isWellFormed(). */
    
    public boolean isWellFormed(DatabaseSchema schema) {

	if (!isWellFormed()) return false;

	return true;
    }

    /** Verify whether this DatabaseRelation is self-consistent.  Currently
     *  returns true in all circumstances. */

    public boolean isWellFormed() {
	return true;
    }

}
