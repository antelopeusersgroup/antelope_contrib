/* Token class for Datascope scanner

   Based on examples from JLex 1.4

   Created 2004-07-12 by Tobin Fricke, University of California
*/

package com.brtt.antelope;

public class DatabaseSchemaToken {

    public int type;
    public Object value;
    private int line;
    private int column;

    public DatabaseSchemaToken(int type, int line, int column) {
	this.type = type;
	this.line = line;
	this.column = column; 
	this.value = null;
    }
    
    public DatabaseSchemaToken(int type, int line, int column,  Object value) {
	this.type = type;
	this.line = line;
	this.column = column; 
	this.value = value;
    }
    
    public String toString() {   
	return "line "+line+", column "+column+ ", type: "+type+/* ", sym: "+sym+ */
	    (value == null ? "" : (", value: '"+value+"'"));
    }
}
