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


%%

/* This section contains directives to JLex. */

%class DatabaseSchemaLexer

%{

/* The following can be generated with the following command:

   cat $DSAP/src/lib/dataformat/db |
     awk ' $1=="#" && $2=="define" { print "public static final int ",$3," = ",$4,";" }' 
*/

    public static final int  STRING  =  257 ;
    public static final int  NUMBER  =  258 ;
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

%%     
    
/* This section contains the scanner rules */

[ \t]		{  /* Discard whitespace. */
}

Schema		{ return SCHEMA; }
Attribute 	{ return ATTRIBUTE; }
Relation 	{ return RELATION; }
Timedate	{ return TIMEDATE; }
Real		{ return REAL; }
Integer		{ return INTEGER; }
Time		{ return TIME; }
YearDay		{ return YEARDAY; }
String		{ return ASCII; }
Bfloat		{ return BFLOAT; }
Bdouble		{ return BDOUBLE; }
Bshort		{ return BSHORT; }
Bint		{ return BINT; }
Dbptr		{ return DBPTR; }

Separator	{ return SEPARATOR; }
Like		{ return LIKE; }
Units		{ return UNITS; }
Null		{ return NULLVAL; }
Format		{ return FORMAT; }
Range		{ return RANGE; }

Fields		{ return FIELDS; }
Primary		{ return PRIMARY; }
Alternate	{ return ALTERNATE; }
Foreign		{ return FOREIGN; }
Defines 	{ return DEFINES; }
Transient	{ return TRANSIENT; }

Description	{ return DESCRIPTION; }
Detail		{ return DETAIL; }


[0-9]*		{ /* number */ }
\{ 		{ /* string */ }
\"		{ /* string */ }

[A-Za-z0-9_\$\%\:\.]*   { /* string/identifier */ }

\n		{ }

.		{ /* character */ }


