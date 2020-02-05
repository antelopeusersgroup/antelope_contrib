/* com.brtt.antelope.OrbParameterFilePacket
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Created 2004-06-29 by Tobin Fricke <tobin@splorg.org>, University of California
 */

package com.brtt.antelope;

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * This class represents a single Antelope Orb Parameter File packet.
 *
 * @author  Somebody
 */

public class OrbParameterFilePacket extends OrbPacket {
    
    /** Constructors */

    public OrbParameterFilePacket(String contents) {
	
    }

    /** Public Methods */
    
    /** Unpack the on-the-wire representation into an OrbStringPacket.  
     *  This is called automatically by the parent class's unstuff method.*/
    
    public static OrbStringPacket unstuff(byte[] pkt) {
	return null; /*FIXME*/
    }
    
    /** Stuff this OrbStringPacket object into its binary representation. */
    
    public byte[] stuff() {
	return null; /*FIXME*/
    }
    
    /** Return the value encapsulated in this packet; in this case, a String. */

    public Object getValue() {
	return null; /*FIXME*/
    }

    /** This gets a string description of the Antelope data packet.
     * @return A string suitable for display.  */

    public String toString() {
	return super.toString();
    }
        
    /** Private Class Methods */

    private Object contents;
        
}
