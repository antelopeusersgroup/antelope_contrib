/* Class to encapsulate Orb Image packets
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Created 2004-06-30 by Tobin Fricke, University of California
 */

package com.brtt.antelope;

import java.io.*;
import java.text.*;
import java.util.*;

import com.sun.image.codec.jpeg.*;
import java.awt.image.*;

/**
 * This class represents the experimental image packet type, which currently has
 * sourcename signature /EXP/IMG.  
 * 
 * @author  Tobin Fricke, University of California
 */

public class OrbImagePacket extends OrbPacket {
    
    /** Constructors */

    /** In case the user wants to manually construct the packet, we have this 
     *  null constructor. */

    public OrbImagePacket() {
    }

    public OrbImagePacket(double time, int pktid, SourceName srcname, 
			  byte pkt[], int pktsize) 
	throws IOException {
	
	this.time = time;
	this.pktid = pktid;
	this.srcname = srcname.clone();
	
        if (pkt == null || pktsize < 1) return;
	
        ByteArrayInputStream inBuf = new ByteArrayInputStream(pkt, 0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
	
	int version = pktBuf.readUnsignedShort ();  
	this.description = buf2str(pktBuf, 64);
	
	// some versions of the format have a format string next

	// we'll assume it's JPEG for now

	JPEGImageDecoder decoder = JPEGCodec.createJPEGDecoder(pktBuf);

	BufferedImage image = decoder.decodeAsBufferedImage();
	
	this.image = image;

    }



    /** Public Methods */
    
    /** Unpack the on-the-wire representation into an OrbImagePacket.  
     *  This is called automatically by the parent class's unstuff method.*/
 
    public static OrbPacket unstuff(double time, int pktid, SourceName srcname, 
				       byte pkt[], int pktsize) 
		throws IOException {
	return new OrbImagePacket(time, pktid, srcname, pkt, pktsize);
    }

	        
    /** Stuff this OrbImagePacket object into its binary representation. */
    
    public byte[] stuff() {
	// not implemented
    }
    
    /** Return the value encapsulated in this packet, a String. */

    public Object getValue() {
	return image;
    }

    /** This gets a string description of the Antelope data packet.
     * @return A string suitable for display.  */

    public String toString() {
	return "[OrbImagePacket \"" + description + "\" " + image.getWidth() + "x" + image.getHeight() + "]";
    }

    /** Public fields */

    public String description; 
    public BufferedImage image;

    /** Private Class Methods */

    /** This main method is just for testing purposes. 

    @author Tobin Fricke, University of California

    */


    static void createAndShowGUI() {
	JFrame.setDefaultLookAndFeelDecorated(true);
	
	JFrame frame = new JFrame("HelloWorldSwing");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	
	    JLabel label = new JLabel("Hello World");
	    frame.getContentPane().add(label);
	    
	    frame.pack();
	    frame.setVisible(true);
    }

    public static void main (String args[]) {

	
	javax.swing.SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    createAndShowGUI();
		}
	    });
	
	JFrame frame = new javax.swing.JFrame("OrbImageViewer");
	frame.show();
	
	try {
	    Orb orb = new Orb("bohemia.splorg.org:6580","r");
	    OrbRawPacket pkt;
	    orb.select("PF_GVS/MGENC");
	    orb.after(0);
	    while ((pkt = orb.reap()) != null) {
		OrbImagePacket imgPkt = OrbImagePacket.unstuff(pkt);
//		imgPkt.image;
	    }
	    orb.close();
	} catch (Exception e) {
	    System.err.println("Exception caught: " + e.getMessage());
	}
    }
}
