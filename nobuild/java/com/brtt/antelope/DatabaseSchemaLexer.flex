/* Lexical Analyzer for Datascope Schema definitions.
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Written by Tobin Fricke <tobin@splorg.org> on 2004-07-09 at IGPP, UCSD
 *
 * Compile with JLex, http://www.cs.princeton.edu/~appel/modern/java/JLex/
 *
 * http://bmrc.berkeley.edu/courseware/cs164/fall99/assignment/a1/tutorial.html
 *
 * Based on $DSAP/src/lib/dataformat/db/dblex.l from DSAP 3.4 (public domain)
 *
 * Status:  Bare-bones beginning.  Not functional.
*/

package com.brtt.antelope;

import java.io.*;
import java.util.*;


%%

/** This section contains directives to JLex. */

/* The 'class' directive tells JLex what to name the generated scanner class.*/

%class DatabaseSchemaLexer

/* By default the scanning function is called 'Yylex'. We override that here.*/

%function getToken

/* The 'debug' directive causes the generated class to contain a 'main' method
   that reads a file (name given on the command line), scans it, and spits the
   output of the scanning function to standard out. */
  
//%debug

/* Here we choose the return type of the scanning function. */

%type DatabaseSchemaToken

%state STRING, CURLYSTRING
%line
%column

/* The following text will be included verbatim in the generated class. */

%{

    public static final int  IDENTIFIER_LITERAL = 16; 
    public static final int  CHARACTER_LITERAL = 17;
    public static final int  INCLUDE = 18;

/* The following can be generated with the following command:

   cat $DSAP/src/lib/dataformat/db |
     awk ' $1=="#" && $2=="define" { print "public static final int ",$3," = ",$4,";" }' 
*/
    
    public static final int  STRING_LITERAL  =  257 ;
    public static final int  INTEGER_LITERAL  =  258 ;
    public static final int  SCHEMA  =  259 ;
    public static final int  ATTRIBUTE  =  260 ;
    public static final int  RELATION  =  261 ;
    public static final int  DESCRIPTION  =  262 ;
    public static final int  DETAIL  =  263 ;
    public static final int  LIKE  =  264 ;
    public static final int  TIMEDATE  =  265 ;
    public static final int  UNITS  =  266 ;
    public static final int  NULLVAL  =  267 ;
    public static final int  FORMAT  =  268 ;
    public static final int  RANGE  =  269 ;
    public static final int  SEPARATOR  =  270 ;
    public static final int  REAL  =  271 ;
    public static final int  INTEGER  =  272 ;
    public static final int  TIME  =  273 ;
    public static final int  YEARDAY  =  274 ;
    public static final int  ASCII  =  275 ;
    public static final int  WAVEFORM  =  276 ;
    public static final int  RESPONSE  =  277 ;
    public static final int  BFLOAT  =  278 ;
    public static final int  BDOUBLE  =  279 ;
    public static final int  BSHORT  =  280 ;
    public static final int  BINT  =  281 ;
    public static final int  DBPTR  =  282 ;
    public static final int  FIELDS  =  283 ;
    public static final int  PRIMARY  =  284 ;
    public static final int  ALTERNATE  =  285 ;
    public static final int  FOREIGN  =  286 ;
    public static final int  DEFINES  =  287 ;
    public static final int  TRANSIENT  =  288 ;
    
%}

%{

  /** a buffer into which we copy string literals */

  StringBuffer string = new StringBuffer();
  
  private DatabaseSchemaToken symbol(int type) {
    return new DatabaseSchemaToken(type, yyline+1, yycolumn+1);
  }

  private DatabaseSchemaToken symbol(int type, Object value) {
    return new DatabaseSchemaToken(type, yyline+1, yycolumn+1, value);
  }

%}

%{

    /* FIXME: It might be appropriate to move the following into a separate
       DatabaseSchemaParser class. */

    /** Require that the next token is the given character.  Throw it
	away if it exists, throw a SyntaxException if it doesn't. */

    public void expectChar(String chr) throws SyntaxException, IOException {
	DatabaseSchemaToken token = getToken();

	if (token.type == CHARACTER_LITERAL && (chr.compareTo((String)(token.value)) == 0)) {
	    /* Throw it away. */
	} else {
	    throw new SyntaxException("Expected character '"+chr+"'.", token);
	}
    }

    public String expectString() throws SyntaxException, IOException { 
	DatabaseSchemaToken token = getToken();
	if (token.type == STRING_LITERAL) {
	    return (String)(token.value);
	} else {
	    throw new SyntaxException("Expected STRING_LITERAL.", token);
	}
    }

    public String expectIdentifier() throws SyntaxException, IOException { 
	DatabaseSchemaToken token = getToken();
	if (token.type == IDENTIFIER_LITERAL) {
	    return (String)(token.value);
	} else {
	    throw new SyntaxException("Expected IDENTIFIER_LITERAL.", token);
	}
    }

    public int expectNumber() throws SyntaxException, IOException { 
	DatabaseSchemaToken token = getToken();
	if (token.type == INTEGER_LITERAL) {
	    return ((Integer)(token.value)).intValue();
	} else {
	    throw new SyntaxException("Expected INTEGER_LITERAL.", token);
	}
    }

    /** Read a list of identifiers, enclosed in parentheis. Example: (foo bar
        baz)  If the argument is null, then a new list is created and
        returned; otherwise the list given in the argument is used.   */

    public List parseIdentifierList(List list) 
	throws SyntaxException, IOException {
	
	expectChar("(");
	
	if (list == null) {
	    list = new ArrayList();
	}
	
	while (true) {
	    DatabaseSchemaToken token = getToken();
	 
	    if (token.type == IDENTIFIER_LITERAL) {
		list.add(token.value);
	    } else if (token.type == CHARACTER_LITERAL && ((String)(token.value)).compareTo(")")==0) {
		break;
	    } else {
		throw new SyntaxException("Expected IDENTIFIER or ')'.", token);
	    }
	}
	return list;
    }

%}

StringChar = [^\r\n\"\\]

%%     
    
/* This section contains the scanner rules */

<YYINITIAL> {

[ \t\r\n]	{ /* Discard whitespace. */ }

Schema		{ return symbol(SCHEMA); }
Attribute 	{ return symbol(ATTRIBUTE); }
Relation 	{ return symbol(RELATION); }
Timedate	{ return symbol(TIMEDATE); }
Real		{ return symbol(REAL); }
Integer		{ return symbol(INTEGER); }
Time		{ return symbol(TIME); }
YearDay		{ return symbol(YEARDAY); }
String		{ return symbol(ASCII); }
Bfloat		{ return symbol(BFLOAT); }
Bdouble		{ return symbol(BDOUBLE); }
Bshort		{ return symbol(BSHORT); }
Bint		{ return symbol(BINT); }
Dbptr		{ return symbol(DBPTR); }

Separator	{ return symbol(SEPARATOR); }
Like		{ return symbol(LIKE); }
Units		{ return symbol(UNITS); }
Null		{ return symbol(NULLVAL); }
Format		{ return symbol(FORMAT); }
Range		{ return symbol(RANGE); }

Fields		{ return symbol(FIELDS); }
Primary		{ return symbol(PRIMARY); }
Alternate	{ return symbol(ALTERNATE); }
Foreign		{ return symbol(FOREIGN); }
Defines 	{ return symbol(DEFINES); }
Transient	{ return symbol(TRANSIENT); }

Description	{ return symbol(DESCRIPTION); }
Detail		{ return symbol(DETAIL); }

[0-9]*		{ return symbol(INTEGER_LITERAL, new Integer(yytext())); /* number */ }

\{ 		{ yybegin(CURLYSTRING); 
		  string.setLength(0); /* string */ }

\"		{ yybegin(STRING); 
		  string.setLength(0); /* string */ }

[A-Za-z0-9_\$\%\:\.]*   { return symbol(IDENTIFIER_LITERAL, yytext());  /* string/identifier */ }

.		{ return symbol(CHARACTER_LITERAL, yytext()); /* character */ }
}

<STRING> {
\"		    { yybegin(YYINITIAL);  return symbol(STRING_LITERAL, string.toString());}
{StringChar}+       { string.append(yytext()); }
"\\\""              { string.append( '\"' ); }
"\\\\"              { string.append( '\\' ); }
}

<CURLYSTRING> {
\}		{ yybegin(YYINITIAL); return symbol(STRING_LITERAL, string.toString()); }
[^}]+		{ string.append(yytext()); }
}