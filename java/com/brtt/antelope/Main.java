/*
 * Main.java
 *
 * Created on February 22, 2001, 9:39 AM
 */

package com.brtt.antelope;

import java.io.*;
import java.net.*;
import java.util.*;

/**
 * This is a main class for testing the Antelope Orb classes.
 *
 * @author  Danny Harvey, BRTT
 * @version 1.0
 */
public class Main extends java.lang.Object {

    /** Creates new Main */
    public Main () {
    }

    /**
    * @param args the command line arguments
    */
    public static void main (String args[]) throws Throwable {
        int i;
        OrbPacket packet;
        String orbname = ":54120" ;
        String select = ".*" ;
        
        if (args.length > 0) {
            orbname = args[0];
        }
        if (args.length > 1) {
            select = args[1];
        }
        
        Orb orb = new Orb(orbname, "r&");
        i = orb.select ( select );
        System.out.println ( i + " sources selected" );
        
        for (i=0; i<3000; i++) {
            packet = orb.reap ();
            System.out.println (packet.getList ());
        }
    }

}
