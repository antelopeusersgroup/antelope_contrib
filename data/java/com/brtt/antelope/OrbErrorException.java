/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.OrbErrorException.java
 *
 * Created on February 25, 2001, 3:44 PM
 */

package com.brtt.antelope;

/**
 * This class represents the error exceptions for the Orb classes.
 *
 * @author  Danny Harvey
 * @version 1.0
 */
public class OrbErrorException extends java.lang.Exception {

    /**
     * Creates new <code>OrbErrorException</code> without detail message.
     */
    public OrbErrorException() {
    }


    /**
     * Creates new <code>OrbErrorException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public OrbErrorException(String msg) {
        super(msg);
    }
}


