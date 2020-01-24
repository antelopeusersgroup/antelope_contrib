/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.Stock.java
 *
 * Created on February 24, 2001, 8:40 PM
 */

package com.brtt.antelope;

/**
 * This class provides Antelope utility class methods.
 *
 * @author  Danny Harvey
 * @version 1.0
 */
public class Stock extends Object {

    /** Creates new Stock */
    public Stock() {
    }
    
    /** Class Methods */
    
    /**
     * This class method converts a byte-array format ip-address into a
     *      string of form "aaa.bbb.ccc.ddd".
     * @param address A four element byte array ip-address.
     * @return A string version of the ip-address.
     */  
    public static String ipString (byte address[]) {
        int i[] = new int[4];
        int j;
        
        for (j=0; j<4; j++) {
            i[j] = address[j];
            if (i[j] < 0) i[j] += 256;
        }
        String s = i[0] + "." + i[1] + "." + i[2] + "." + i[3];
        
        return (s);
    }

}
