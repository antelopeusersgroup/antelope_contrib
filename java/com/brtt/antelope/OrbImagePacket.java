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

// import com.sun.image.codec.jpeg.*;
import java.awt.image.*;

/**
 * This class represents the experimental image packet type, which currently 
 * has sourcename signature /EXP/IMG.  The image is decoded by Java into an 
 * object of class java.awt.Image, but the original image data is kept around
 * in the field imageData.
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
	this.srcname = srcname; //FIXME .clone();

	/* We probably want to keep the original packet contents around.
	 * Should we do this by inheriting from OrbRawPacket, or ... ? 
 	 */

        if (pkt == null || pktsize < 1) return;

        ByteArrayInputStream inBuf = new ByteArrayInputStream(pkt, 0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
	
	int version = pktBuf.readUnsignedShort ();  
	this.description = buf2str(pktBuf, 64);

        this.imageData = new byte[ pktsize - (2 + 64) ];
	System.arraycopy(pkt, 2 + 64, this.imageData, 0, imageData.length);
	
	// some versions of the format have a format string next

        /* Java has an over-abundance of image-handling functions. There are
           lots of classes for handling images and there appears to be a
           substantial overlap in functionality. */

        this.image = (new javax.swing.ImageIcon(imageData, description)).getImage();
	
        /* Another way to do it would be as follows. */

	// JPEGImageDecoder decoder = JPEGCodec.createJPEGDecoder(pktBuf);
	// BufferedImage image = decoder.decodeAsBufferedImage();

    }



    /** Public Methods */
    
    /** Unpack the on-the-wire representation into an OrbImagePacket.  
     *  This is called automatically by the parent class's unstuff method.*/
 
    public static OrbPacket unstuff(OrbRawPacket packet) throws IOException {
	return unstuff(packet.time, packet.pktid, 
		       packet.srcname, packet.packet, packet.pktsize);
    }

    public static OrbPacket unstuff(double time, int pktid, SourceName srcname, 
				       byte pkt[], int pktsize) 
		throws IOException {

	return new OrbImagePacket(time, pktid, srcname, pkt, pktsize);
    }

	        
    /** Stuff this OrbImagePacket object into its binary representation. */
    
    public OrbRawPacket stuff() {
	// not implemented
      
        /* Check to see whether a coded version of the image exists. */

        if (imageData == null || imageData.length == 0) {
           if (image == null) {
             /* This is an error condition -- there is no image here! */
           } {
             /* Encode the image object into the imageData byte array. */
           }
        }

        /* Here we do the serialization. */ 

        return null;
    }
    
    /** Return the value encapsulated in this packet, a String. */

    public Object getValue() {
	return image;
    }

    /** This gets a string description of the Antelope data packet.
     * @return A string suitable for display.  */

    public String toString() {
	
	// return "[OrbImagePacket \"" + description + "\" " + image.getWidth() + "x" + image.getHeight() + "]";
	return super.toString();
    }

    /** Public fields */

    /** Textual description of this image. */
    public String description; 
  
    /** Decoded image object.  Use this to display the image. */
    public java.awt.Image image;

    /** Raw image data.  Use this if you want to package up the image and
     *  send it somewhere else. */
    public byte[] imageData;

    /** Private Class Methods */

    /** This main method is just for testing purposes. 

    @author Tobin Fricke, University of California

    */

/*
    static void createAndShowGUI() {
	JFrame.setDefaultLookAndFeelDecorated(true);
	
	JFrame frame = new JFrame("HelloWorldSwing");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	
	    JLabel label = new JLabel("Hello World");
	    frame.getContentPane().add(label);
	    
	    frame.pack();
	    frame.setVisible(true);
    }

*/
    public static void main (String args[]) {
/*
	
	javax.swing.SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    createAndShowGUI();
		}
	    });
	
	JFrame frame = new javax.swing.JFrame("OrbImageViewer");
	frame.show();
	
*/
	try {
	    Orb orb = new Orb("bohemia.splorg.org:6580","r");
	    OrbRawPacket pkt;
	    orb.select("SIO_Revelle_Axis1/EXP/IMG");
	    orb.after(0);
	    while ((pkt = (OrbRawPacket)(orb.reap(false))) != null) {
		System.out.println("Got a packet; unstuffing it...");
		System.out.println("The packet I got is \""+pkt+"\".");
		OrbPacket unstuffedpkt = OrbImagePacket.unstuff(pkt);
		OrbImagePacket imgPkt = (OrbImagePacket)unstuffedpkt;
		System.out.println("Got image packet with description \"" +
				   imgPkt.description + "\".");
	    }
	    orb.close();
	} catch (Exception e) {
	    System.err.println("Exception caught: " + e.getMessage());
	}
    }

    /* We should put these into some private data access class. */

    private static String buf2str(DataInputStream in, int l) 
	throws IOException {
	int i;
        if (l < 1) {
            return (new String(""));
        }
        byte ibuf[] = new byte[l];
        in.readFully (ibuf);
        for (i=0; i<l; i++) if (ibuf[i] == 0) break;
        return (new String (ibuf, 0, i));
    }
}
