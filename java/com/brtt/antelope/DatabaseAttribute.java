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

    public String format;

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

    public String nullval;

    /** Short description of the attribute.  This value is accessible through 
     *  tools such as dbquery. */

    public String description;

    /** Verbose description of the attribute.  This value is accessible through
     *  tools such as dbquery. */

    public String detail;

    /** Parse a textual description of a DatabaseAttribute.  This will most 
     *  likely only be called by DatabaseSchema.parse(). */

    public static DatabaseAttribute parse(DatabaseSchemaLexer lexer) 
	throws SyntaxException, IOException {

	DatabaseAttribute attribute = new DatabaseAttribute();

	attribute.name = lexer.expectIdentifier();

	DatabaseSchemaToken token = lexer.getToken();
	
	switch (token.type) {
	    case DatabaseSchemaLexer.REAL:    break; /*FIXME*/
	    case DatabaseSchemaLexer.INTEGER: break;
	    case DatabaseSchemaLexer.STRING:  break;
	    case DatabaseSchemaLexer.TIME:    break;
	    case DatabaseSchemaLexer.YEARDAY: break;
	    case DatabaseSchemaLexer.ASCII:   break;
	    case DatabaseSchemaLexer.DBPTR:   break;

	    default:
		throw new SyntaxException("Expected a type (REAL, INTEGER, STRING, TIME, ASCII, or YEARDAY).",
					  token);
	}

	lexer.expectChar("(");
	attribute.size = lexer.expectNumber();
	lexer.expectChar(")");

	while (true) {

	    token = lexer.getToken();

	    if (token.type == lexer.CHARACTER_LITERAL && ((String)(token.value)).compareTo(";")==0 ) {
		break;
	    } else if (token.type == lexer.FORMAT) {
		lexer.expectChar("(");
		attribute.format = lexer.expectString();
		lexer.expectChar(")");
	    } else if (token.type == lexer.UNITS) {
		lexer.expectChar("(");
		attribute.format = lexer.expectString();
		lexer.expectChar(")");
	    } else if (token.type == lexer.RANGE) {
		lexer.expectChar("(");
		attribute.range = lexer.expectString();
		lexer.expectChar(")");
	    } else if (token.type == lexer.NULLVAL) {
		lexer.expectChar("(");
		attribute.nullval = lexer.expectString();
		lexer.expectChar(")");
	    } else if (token.type == lexer.DESCRIPTION) {
		lexer.expectChar("(");
		attribute.description = lexer.expectString();
		lexer.expectChar(")");
	    } else if (token.type == lexer.DETAIL) {
		attribute.detail = lexer.expectString();
	    } else {
		throw new SyntaxException("Expected FORMAT, UNITS, RANGE, NULLVAL, DESCRIPTION, DETAIL, or ';'.", token);
	    }
	}

	return attribute;
    }

    /** Produce a textual description of this DatabaseAttribute.  This will most
     *  likely only be called by DatabaseSchema.unparse(). */

    public void unparse(Writer w) throws IOException {
	w.write("Attribute " + name + "\n");
	w.write("  ImagineTheTypeNameHere (" + size + ")\n");
	if (format != null)
	    w.write("  Format ( \"" + format + "\" )\n");
	if (nullval != null)
	    w.write("  Null ( \"" + null + "\" )\n");
	if (range != null)
	    w.write("  Range ( \"" + range + "\" )\n");
	if (units != null)
	    w.write("  Units ( \"" + units + "\" )\n");
	if (description != null)
	    w.write("  Description ( \"" + description + "\" )\n");
	if (detail != null)
	    w.write("  Detail {" + detail + "}\n");
	w.write("  ;");
    }

    
    /** Write this datascope attribute out as an XML element called 'attribute'.  The datascope schema's 
     *  name, type, size format string, null value, range, units, and description are included as xml
     *  attributes; only the detail field is given as a sub-element (which is convenient because the detail
     *  field can be very long and should be unparsed CDATA. 
     */

    public void unparseAsXML(Writer w) throws IOException {
	w.write("  <attribute name=\"" + name + "\" type=\"" + "\" size=\"" + size);
	if (format != null)
	    w.write(" format=\"" + format + "\"");
	if (nullval != null)
	    w.write(" null=\"" + nullval + "\"");
	if (range != null)
	    w.write(" range=\"" + range + "\"");
	if (units != null)
	    w.write(" units=\"" + units + "\"");
	if (description != null)
	    w.write(" description=\"" + description + "\"");
	if (detail != null)
	    w.write(">\n    <detail>" + detail + "</detail>\n  </attribute>");
	else
	    w.write("/>\n");
    }


    /** Verify whether this DatabaseAttribute is self-consistent. Not Implemented yet.*/
    
    public boolean isWellFormed() {
      return true;
    }

}
