/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.OrbPacket
 * Created on February 23, 2001, 3:49 PM
 *
 * Portions Copyright (c) 2004 Regents of the University of California
 * 2004-06-29 Tobin Fricke <tobin@splorg.org> University of California
 */

package com.brtt.antelope;

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * This class represents a single Antelope Orb packet.  By itself, this
 * class is not particularly useful; it's main purpose is to provide a 
 * parent for the OrbWaveformPacket, OrbDatabasePacket, ... packet classes
 * and to dispatch to these subclasses for unstuffing.
 *
 * @author  Danny Harvey, BRTT
 * @author  Tobin Fricke, University of California
 */
public abstract class OrbPacket extends Object {
    /**
     * Holds the packet epoch time. This is set by Orb.get or Orb.reap for 
     * packets coming from the Orb.  For outgoing packets, this value is 
     * used as the packet timestamp if it is nonzero; if this field is zero,
     * the current time is used instead. FIXME: I am not sure whether this
     * is the proper behavior.  Specifically, is there a difference between
     * packet timestamps and channel timestamps?
     */
    public double time;
    
    /**
     * Holds the Orb packet id.  This is set by Orb.get or Orb.reap for 
     * packets coming from the Orb and I assume it is set by the orbserver
     * when it receives a packet.  Therefore all user classes can consider
     * this field to be read-only.
     */
    public int pktid;
    
    /**
     * Holds the packet Orb source name.  This is set by Orb.get or Orb.reap
     * for packets coming from the Orb.
     */
    public SourceName srcname;
    
    /** Constructors */
    
    /**
     * Creates a new OrbPacket object.  This class is not meant to be
     * instantiated directly.  Instead the "unstuff" factory method should
     * be used to dispatch to one of the subclasses.
     **/

    public OrbPacket() {
    }
          
    /** Public Methods */

    
    public abstract OrbRawPacket stuff();

    /**
     * This converts on Antelope data packet into a usable format.
     * @exception java.io.IOException
     *              IO error during parsing of data packet.
     */

    public static OrbPacket unstuff(OrbRawPacket packet) throws IOException {
	return unstuff(packet.time, packet.pktid, 
		       packet.srcname, packet.packet, packet.pktsize);
    }

    public static OrbPacket unstuff(double time, int pktid, SourceName srcname, 
				    byte pkt[], int pktsize) throws IOException {

	// parse the sourcename?

	// here we dispatch to the subclasses

	/* beware: if a subclass doesn't redefine the unstuff method,
	   an infinite loop will result. */
	
        if (srcname.type.compareTo("waveform") == 0) 
	    return OrbWaveformPacket.unstuff(time, pktid, srcname, pkt, pktsize);
/*	
	if (srcname.type.compareTo("database") == 0) 
	    return OrbDatabasePacket.unstuff(pkt);
	
	if (srcname.type.compareTo("parameter") == 0) 
	    return OrbParameterPacket.unstuff(pkt);
*/
	if (srcname.type.compareTo("string") == 0)
	    return OrbStringPacket.unstuff(time, pktid, srcname, pkt, pktsize);

	if (srcname.type.compareTo("log") == 0)
	    return OrbLogPacket.unstuff(time, pktid, srcname, pkt, pktsize);

	return OrbRawPacket.unstuff(time, pktid, srcname, pkt, pktsize);
    }
    
    /**
     * This gets a string description of the Antelope data packet.
     * @return A string suitable for display.
     */
    public String toString() {
        int i;
        
        DecimalFormat fmpktid = new DecimalFormat ( "000000" );
        Epoch epoch = new Epoch (time);
        
        String s =    fmpktid.format(pktid) + ":" 
                        + epoch.toString () + ":"
                        + srcname
                        ;
        
        return (s);
    }
        
   
}
