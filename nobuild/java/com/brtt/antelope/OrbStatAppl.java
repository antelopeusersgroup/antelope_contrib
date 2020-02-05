/*
 * OrbStatAppl.java
 *
 * Created on March 4, 2001, 12:02 PM
 */

package com.brtt.antelope;

/**
 *
 * @author  default
 * @version 
 */
public class OrbStatAppl extends java.applet.Applet {

    /** Initialization method that will be called after the applet is loaded
     *  into the browser.
     */
    public void init () {
        new JFrameOrbStat ().show ();
    }

}
