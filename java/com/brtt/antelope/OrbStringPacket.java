/* com.brtt.antelope.OrbStringPacket
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Written 2004-06-29 by Tobin Fricke, University of California
 */

package com.brtt.antelope;

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * This class represents a single Antelope Orb character packet, which 
 * carries a single string.
 *
 * @author  Tobin Fricke, University of California
 */

public class OrbStringPacket extends OrbPacket {
    
    /** Constructors */

    public OrbStringPacket(String contents) {
	this.contents = contents;
    }

    /** Public Methods */
    
    /** Unpack the on-the-wire representation into an OrbStringPacket.  
     *  This is called automatically by the parent class's unstuff method.*/

    public static OrbPacket unstuff(double time, int pktid, SourceName srcname, 
				    byte pkt[], int pktsize) {
	try {
	    return new OrbStringPacket(new String(pkt, 0, pktsize, "US-ASCII"));
	} catch (UnsupportedEncodingException e) {
	    return new OrbStringPacket(new String(pkt, 0, pktsize));
	}
    }
    
    /** Stuff this OrbStringPacket object into its binary representation. */
    
    public OrbRawPacket stuff() {
	byte pkt[];
	try {
	    pkt = contents.getBytes("US-ASCII");
	} catch (UnsupportedEncodingException e) {
	    pkt = contents.getBytes();
	}
	return new OrbRawPacket(time, pktid, srcname, pkt, pkt.length);
    }
    
    /** Return the value encapsulated in this packet; in this case, a String. */

    public String getValue() {
	return contents;
    }

    /** This gets a string description of the Antelope data packet.
     * @return A string suitable for display.  */

    public String toString() {
	return super.toString() + " \"" + contents + "\"";
    }
        
    /** Private Class Methods */

    private String contents;
        
}
