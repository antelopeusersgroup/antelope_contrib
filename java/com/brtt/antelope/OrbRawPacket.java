/* com.brtt.antelope.OrbRawPacket.java
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Created 2004-06-29 by Tobin Fricke, University of California
 */

package com.brtt.antelope;

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * This class represents a raw Antelope packet -- ie, one that simply contains
 * an uninterpreted array of bytes.  This class is an excellent starting point
 * for writing new Orb*Packet classes.   Additionally, this is a good class to
 * use if you to represent or transport an Orb Packet without unstuffing it.
 *
 * @author  Tobin Fricke, University of California
 */

public class OrbRawPacket extends OrbPacket {
    
    /** Constructors */

    /** In case the user wants to manually construct the packet, we have this null constructor. */

    public OrbRawPacket() {
    }

    public OrbRawPacket(double time, int pktid, SourceName srcname, 
			byte pkt[], int pktsize) {
	this.pktid = pktid;
	this.pktsize = pktsize;
	this.packet = new byte[pktsize];
	System.arraycopy(pkt, 0, this.packet, 0, pktsize);
    }

    /** Public Methods */
    
    /** Unpack the on-the-wire representation into an OrbRawPacket.  For the
     *  case of a raw packet, this is a very boring procedure.
     *  This is called automatically by the parent class's unstuff method.*/
 
    public static OrbPacket unstuff(double time, int pktid, SourceName srcname, 
				       byte pkt[], int pktsize) {
	return new OrbRawPacket(time, pktid, srcname, pkt, pktsize);
    }
    
    /** Return a stuffed (OrbRawPacket) version of this packet.  Since
     *  this is already an OrbRawPacket, we just return 'this'. */
    
    public OrbRawPacket stuff() {
	return this;
    }
    
    /** Return the value encapsulated in this packet, a String. */

    public Object getValue() {
	return packet;
    }

    /** This gets a string description of the Antelope data packet.
     * @return A string suitable for display.  */

    public String toString() {

	/* ToDo: This can be expanded to include a hex dump of the first N
	   bytes. */

	return super.toString();
    }
        
    /* The following are public so that the Orb class can re-use a single
     * instance of this class */

    /** Holds the packet data. */

     public byte packet[] = null;

    /** Holds the size of packet data currently in the buffer (member field 
     *  'packet').  This can be different from packet.size, because the array
     *  can be re-used.
     */

     public int pktsize = 0;

    /** Private Class Methods */

}
