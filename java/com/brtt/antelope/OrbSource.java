/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * OrbSource.java
 *
 * Created on February 23, 2001, 3:47 PM
 */

package com.brtt.antelope;

/**
 * This class represents the status of a single Antelope Orb "source" packet type.
 *
 * @author  Danny Harvey
 * @version 1.0
 */
public class OrbSource extends Object implements Comparable {

    /**
     * Holds the epoch time when the source status was retrieved
     */
   public double whenTime;

    /**
     * Holds the epoch time of the most recently received packet.
     */
   public double latestTime;

    /**
     * Holds the epoch time of the least recently received packet.
     */
   public double oldestTime;

    /**
     * Holds the total number of bytes within the Orb for this packet source name.
     */
   public int nbytesOrb;

    /**
     * Holds the total number of packets within the Orb for this packet source name.
     */
   public int npktsOrb;

    /**
     * Holds the packet id of the most recently received packet.
     */
   public int latestPktid;

    /**
     * Holds the packet id of the least recently received packet.
     */
   public int oldestPktid;

    /**
     * Holds the activity flag for this packet source name.
     */
   public int active;

    /**
     * Holds the packet source name.
     */
   public String srcname;

    /** Constructors */

    /**
     * Creates a new OrbSource object.
     **/
    public OrbSource() {
    }

    /** Public Methods */

    /**
     * This gets a string description of the Antelope Source status.
     * @return A string suitable for display with the source status.
     */
    public String getList () {
        String s =    srcname + " " + npktsOrb + " " + nbytesOrb
                    + " " + oldestPktid + " " + latestPktid + "\n"
                    ;

        return (s);
    }

    /**
     * This can be used to compare the source names of two different OrbSource
     * objects.
     * @param src Another OrbSource object for comparison.
     * @return An integer that is &lt; 0, == 0, or &gt; 0, as the string comparison
     *          of this object's srcname and the srcname of src.
     */
    public int compareTo (Object src) {
        int i = (this.srcname).compareTo(((OrbSource)src).srcname);
        return (i);
    }

}
