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
     * Holds the SEED network code.
     */
    public String net;
    
    /**
     * Holds the SEED station code.
     */
    public String sta;
    
    /**
     * Holds the SEED channel code.
     */
    public String chan;
    
    /**
     * Holds the SEED location code.
     */
    public String loc;
    
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
     * Holds the current size of data[]
     */
    public int datasize = 0;
        
    /** Constructors */
    
    /**
     * Creates a new OrbPacketChannel object.
     **/
    public OrbPacketChannel() {
    }
          
    /** Public Methods */
    
    /**
     * This gets a string description of the Antelope packet channel.
     * @return A string suitable for display.
     */
    public String getList () {
        Epoch epoch = new Epoch (time);
        String s;
        int i;
        
        if (loc.length() < 1) {
            s =    epoch.toString () + ":"
                        + net + "_" + sta + "_" + chan
                        ;
        } else {
            s =    epoch.toString () + ":"
                        + net + "_" + sta + "_" + chan + "_" + loc
                        ;
        }
        
        s += " " + samprate + " " + nsamp;
        
        int n = nsamp;
        if (n > 10) n = 10;
        for (i=0; i<n; i++) {
            s += " " + data[i];
        }
        
        return (s);
    }
   
}
