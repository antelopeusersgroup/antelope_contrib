/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.OrbClient.java
 *
 * Created on February 23, 2001, 3:49 PM
 */

package com.brtt.antelope;

import java.text.*;

/**
 * This class represents a single channel of waveform data contained
 *	in a single Antelope Orb packet.
 *
 * @author  Danny Harvey, BRTT
 * @version 1.0
 */
public class OrbPacketChannel extends Object {
    
    /**
     * Holds the souce name.
     */

    public SourceName srcname;
    
    /**
     * Holds the calib value.
     */
    public double calib;
    
    /**
     * Holds the calper value.
     */
    public double calper;
    
    /**
     * Holds the segtype value.
     */
    public String segtype;
    
    /**
     * Holds the epoch time of the first data sample.
     */
    public double time;
    
    /**
     * Holds the data sampling rate.
     */
    public double samprate;
    
    /**
     * Holds the number of data samples.
     */
    public int nsamp;
    
    /**
     * Holds the data sample values.
     */
    public int data[] = null;
    
    /**
     * Holds the current size of data[].  (Is this necessary? Isn't the same information
     * available as data.length? --tobin ) 
     */
    public int datasize = 0;
        
    /** Constructors */
    
    /**
     * Creates a new OrbPacketChannel object.
     **/

    public OrbPacketChannel() {
    }

    /**
     * Creates a new OrbPacketChannel object.
     **/

    public OrbPacketChannel(int data[], SourceName srcname, double calib, double calper, 
			    String segtype, double time, double samprate) {
	
	this.data = data;              // FixMe: does this make a copy of the array?
	this.datasize = data.length;
	this.nsamp = data.length;
	
	this.srcname = srcname;        // again, should we clone()?
	this.calib = calib;
	this.calper = calper;
	this.segtype = segtype;
	this.time = time;
	this.samprate = samprate;
    }

    /** Public Methods */
    
    /**
     * This gets a string description of the Antelope packet channel.
     * @return A string suitable for display.
     */
    public String toString() {
        Epoch epoch = new Epoch (time);

	String s = epoch + ":" + srcname + " " + samprate + " " + nsamp;
        
        for (int i=0; i < (nsamp > 10 ? 10 : nsamp) ; i++ ) {
            s += " " + data[i];
        }
        
        return (s);
    }
   
}
