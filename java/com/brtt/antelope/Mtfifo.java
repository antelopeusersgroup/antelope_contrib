/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.Mtfifo
 *
 * Created on March 2, 2001, 2:44 PM
 */

package com.brtt.antelope;

import java.util.*;

/**
 * This class provides a MT safe FIFO object.
 *
 * @author  Danny Harvey, BRTT
 * @version 1.0
 */
public class Mtfifo extends java.lang.Object {
    private LinkedList q = new LinkedList();

    /** 
     * Creates a new MT safe FIFO object.
     */
    public Mtfifo() {
    }
    
    /** 
     * Pushes an object onto the MT FIFO.
     * @param o The object to be pushed.
     */
    public synchronized void push(Object o) {
        q.addFirst(o);
        this.notify();
    }
    
    /** 
     * Pops an object from the MT FIFO.
     * @return The popped object.
     **/
    public synchronized Object pop() {
        while (q.size() == 0) {
            try { this.wait(); }
            catch (InterruptedException e) {}
        }
        return q.removeLast();
    }

}
