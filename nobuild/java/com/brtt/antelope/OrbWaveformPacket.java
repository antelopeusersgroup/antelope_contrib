/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * based on com.brtt.antelope.OrbPacket
 * Created on February 23, 2001, 3:49 PM
 *
 * Portions Copyright (c) 2004 Regents of the University of California
 * 2004-06-29 Tobin Fricke <tobin@splorg.org> University of California
 */

package com.brtt.antelope;

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * This class represents a single Antelope Orb packet.
 *
 * @author  Danny Harvey, BRTT
 * @version 1.0
 */
public class OrbWaveformPacket extends OrbPacket {
    static private final int GENC_DEBUG = 0;
    
    /**  Holds the waveform channel objects */
 
    public List channels = null;
        
    /** Constructors */

    /** This constructor creates a basic OrbWaveformPacket that doesn't yet
     *  contain any channels of waveform.  You can use the addChannel method
     *  to add channels. 
     */

    public OrbWaveformPacket(double time, int pktid, SourceName srcname) {
	this.time = time;
	this.pktid = pktid;
	this.srcname = srcname;
    }

    /** Public Methods */
    
    public void addChannel(OrbPacketChannel channel) {
	if (channels == null) {
	    channels = new Vector();
	}
	channels.add(channel);
    }

    /**
     * This converts on Antelope data packet into a usable format.
     * @exception java.io.IOException
     *              IO error during parsing of data packet.
     */

    public static OrbPacket unstuff(double time, int pktid, SourceName srcname,
				    byte pkt[], int pktsize) {
	
	// parse sourcename?

	// We will save our sub-unstuffers a little work by creating an empty
	// OrbWaveformPacket from which they can work

	OrbWaveformPacket wfpkt = new OrbWaveformPacket(time, pktid, srcname);

	try {
	    if (srcname.format.compareTo("MGENC") == 0) {
		unstuffMGENC(wfpkt, pkt, pktsize);
		return wfpkt;
	    }
	    
	    if (srcname.format.compareTo("GENC") == 0) {
		unstuffGENC(wfpkt, pkt, pktsize);
		return wfpkt;
	    }
	    
	    if (srcname.format.compareTo("GEN") == 0) {
		unstuffGEN(wfpkt, pkt, pktsize);
		return wfpkt;
	    }
	} catch (IOException e) {
	    return null;
	}

	return null; // fixme: throw an exception?
    }

    public OrbRawPacket stuff() {
	try {
	    byte pkt[] = stuffGEN();
	    return new OrbRawPacket(time, pktid, srcname, pkt, pkt.length);
	} catch (Exception e) {
	    // FIXME
	}
	return null;
    }

    /**
     * This gets a string description of the Antelope data packet.
     * @return A string suitable for display.
     */
    public String toString() {
        int i;
        
        DecimalFormat fmpktid = new DecimalFormat ( "000000" );
        Epoch epoch = new Epoch (time);
        
        String s =    fmpktid.format(pktid) + ":" 
                        + epoch.toString () + ":"
                        + srcname
                        ;
        
        s += "\n        nchannels = " + (channels == null ? 0 : channels.size());
        
        for (i=0; i<(channels == null ? 0 : channels.size()); i++) {
            s += "\n        " + channels.get(i).toString() ;
        }
        
        return (s);
    }
        
    /** Private Class Methods */
    
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

    private static void str2buf (String s, DataOutputStream out, int len) 
	throws IOException {
	// What should we do if we don't have enough bytes to send?  Pad with
	// nulls or spaces? 
	// Throw an exception?  Model the behavior of buf2str?
	
	byte[] bytes = s.getBytes("US-ASCII");
	for (int i=0; i<len; i++) 
	    out.writeByte(i < bytes.length ? bytes[i] : 0);
    }
    
    private static void unstuffMGENC(OrbWaveformPacket dest, 
				     byte packet[], int pktsize)
	throws IOException {
	
        if (packet == null || pktsize < 1) return;
        
        ByteArrayInputStream inBuf = new ByteArrayInputStream(packet, 
							      0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
	
	int version = pktBuf.readUnsignedShort ();  
        int nchannels = pktBuf.readUnsignedShort ();
	
	/* I think it is somewhat bad form to explicity create the channels
	   object here, rather than relying on the addChannel interface; but
	   here we have the advantage of knowing exactly how many channels
	   we will be adding. */

	dest.channels = new Vector(nchannels);
	
	for (int i = 0; i < nchannels; i ++) {
	    dest.channels.add(new OrbPacketChannel());
	}

	/* An MGENC packet with only one channels is pretty much the same
	   as a GENC packet, but some of the fields are reordered. */
	
	for (int c = 0; c < nchannels; c++) {

	    /* Here we fill in the channel objec that we've already added. */

	    OrbPacketChannel channel = (OrbPacketChannel)(dest.channels.get(c)); 
	    
	    if (nchannels == 1) {
		/* FixMe: The channel sourcename is not completely the same
		   as the packet sourcename! */
		channel.srcname = dest.srcname;
	    } else {
		String net = buf2str(pktBuf, 10);
		String sta = buf2str(pktBuf, 10);
		String chan = buf2str(pktBuf, 10);
		String loc = buf2str(pktBuf, 10);

		channel.srcname = new SourceName(net, sta, chan, loc);
	    }
	    
	    channel.segtype = buf2str(pktBuf, 2); 
	    if (nchannels == 1) {
		channel.time = dest.time;	  
	    } else {
		channel.time = pktBuf.readDouble();	  
	    }
	    
	    channel.nsamp       = pktBuf.readUnsignedShort () ;
	    channel.samprate    = pktBuf.readFloat () ;
	    channel.calib       = pktBuf.readFloat () ;
	    channel.calper      = pktBuf.readFloat () ;
	    
	    channel.datasize = channel.nsamp;
	    channel.data = new int[channel.datasize];

            int nout;
            if (channel.nsamp < 5) {
		for (int i=0; i<channel.nsamp; i++)
		    channel.data[i] = pktBuf.readInt();
		nout = channel.nsamp;
            } else { 
		int bytecount = pktBuf.readUnsignedShort();
		nout = uncompressGENC (channel.data, pktBuf, bytecount) ;
		for (int i=1; i<channel.nsamp; i++) 
		    channel.data[i] += channel.data[i-1];
            }
	    
	    if (nout != channel.nsamp) {
		System.out.println ("nsamp = " + channel.nsamp + ", but nout = " + nout);
	    }
	}
	
        pktBuf.close() ;
        inBuf.close() ;
    }
    

    private static void unstuffGENC(OrbWaveformPacket dest, 
			     byte packet[], int pktsize)
	throws IOException {
	
        if (packet == null || pktsize < 1) return;
        
        ByteArrayInputStream inBuf = new ByteArrayInputStream (packet, 0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
        
        double samprate    = pktBuf.readFloat () ;
        double calib       = pktBuf.readFloat () ;
        double calper      = pktBuf.readFloat () ;
        int nsamp          = pktBuf.readUnsignedShort () ;
        String segtype     = buf2str (pktBuf, 2); 
        
        int nchannels = 1;
   	dest.channels = new Vector(nchannels);
	OrbPacketChannel channel = new OrbPacketChannel();
	dest.channels.add(channel);

        channel.srcname  = dest.srcname;

        channel.calib    = calib;
        channel.calper   = calper;
        channel.samprate = samprate;
        channel.nsamp    = nsamp;
        channel.segtype  = segtype;
        
	channel.datasize = channel.nsamp;
	channel.data = new int[channel.datasize];
        
        int nout = uncompressGENC (channel.data, pktBuf) ;
        
        if (nout != nsamp) {
            System.out.println ("nsamp = " + nsamp + ", but nout = " + nout);
        }
            
        pktBuf.close () ;
        inBuf.close () ;
        
        for (int i=1; i<channel.nsamp; i++) channel.data[i] += channel.data[i-1];
    }
        
    private static void unstuffGEN(OrbWaveformPacket dest, 
			    byte packet[], int pktsize)
	throws IOException {
	
        if (packet == null || pktsize < 1) return;
        
        ByteArrayInputStream inBuf = new ByteArrayInputStream (packet, 0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
        
        double samprate    = pktBuf.readFloat () ;
        double calib       = pktBuf.readFloat () ;
        double calper      = pktBuf.readFloat () ;
        int nsamp          = pktBuf.readUnsignedShort () ;
        String segtype     = buf2str (pktBuf, 2); 
        
        int nchannels = 1;
	dest.channels = new Vector(nchannels);
	OrbPacketChannel channel = new OrbPacketChannel();
	dest.channels.add(channel);
	
        channel.time     = dest.time;
	channel.srcname  = dest.srcname;
        channel.calib    = calib;
        channel.calper   = calper;
        channel.samprate = samprate;
        channel.nsamp    = nsamp;
        channel.segtype  = segtype;
        
	channel.datasize = channel.nsamp;
	channel.data = new int[channel.datasize];
                       
        for (int i=0; i<channel.nsamp; i++) channel.data[i] = pktBuf.readInt();
            
        pktBuf.close () ;
        inBuf.close () ;

    }
    
    /** Stuff the packet using the GEN format. ToDo: We might want to be
     *  able to supply a ready-made buffer to stuffGEN so that we can avoid
     *  repeatedly allocating arrays. */

    public byte[] stuffGEN() throws IOException {
	
	ByteArrayOutputStream outBuf = new ByteArrayOutputStream();
        DataOutputStream pktBuf = new DataOutputStream (outBuf);

	// FixMe: check that exactly one channel exists
	OrbPacketChannel channel = (OrbPacketChannel)(channels.get(0));

	pktBuf.writeFloat((float)channel.samprate);
	pktBuf.writeFloat((float)channel.calib);
	pktBuf.writeFloat((float)channel.calper);
	pktBuf.writeShort(channel.nsamp); // FixMe: Is this unsigned?
	str2buf(channel.segtype, pktBuf, 2);
	
	// FixMe: warn if too many channels
	
	for (int i=0; i<channel.nsamp; i++) 
	    pktBuf.writeInt(channel.data[i]);
	
	byte packet[] = outBuf.toByteArray();
        pktBuf.close();
        outBuf.close();
	return packet;
    }
    
    private static int uncompressGENC (int out[], DataInputStream in) throws IOException {
	return uncompressGENC(out, in, in.available());
    }
    
    private static int uncompressGENC (int out[], DataInputStream in, int n) throws IOException {
        int reserved = in.available() - n;  /* This is kind of a hack. */
        int gcnsamp   = in.readInt () ;
        if (GENC_DEBUG == 1) System.out.println ( "start " + gcnsamp + " " + n ) ;
        int gclen     = in.readUnsignedByte () ;
        if (GENC_DEBUG == 1) System.out.println ( gclen ) ;
        int len, k, iout=0, msbit, nout=0;
        while (in.available() - reserved > 0) {
            len = in.readUnsignedByte () + 1 ;
            n = in.available () - reserved;
            if (GENC_DEBUG == 1) System.out.println ( "start blockette " + len + " " + n ) ;
            msbit = 7;
            for (k=0; k<gclen; k++) {
                out[iout] = 0;
                msbit = bitunpackGENC (out, iout, in, msbit, len);
                out[iout] = (out[iout]<<(32-len))>>(32-len);
                iout++;
                n = in.available () - reserved;
                if (GENC_DEBUG == 1) System.out.println (iout + " " + n + " " + msbit);
                if (iout >= nout + gcnsamp) break;
            }
            if (msbit != 7) in.skip (1) ;
            n = in.available () - reserved;
            if (GENC_DEBUG == 1) System.out.println ( "end blockette " + n ) ;
            if (iout >= nout + gcnsamp) {
                nout += gcnsamp;
                if (in.available () - reserved < 1) break;
                gcnsamp     = in.readInt () ;
                n = in.available () -reserved;
                if (GENC_DEBUG == 1) System.out.println ( "start " + gcnsamp + " " + n ) ;
                gclen       = in.readUnsignedByte () ;
                if (GENC_DEBUG == 1) System.out.println ( gclen ) ;
            }
        }        
        return (nout);
    }
    
    
    private static int bitunpackGENC (int out[], int iout, DataInputStream in, 
                                    int msbit, int length) throws IOException {
         int lsbit=0, mask=0, nn=0, i=0;
         
         lsbit = msbit - length + 1;
         if (lsbit < 0) {
             nn = msbit + 1;
             mask = ((1<<nn)-1)<<(-lsbit);
             i = in.readUnsignedByte () ;
             out[iout] = out[iout] | ((i<<(-lsbit)) & mask);
             msbit = 7;
             msbit = bitunpackGENC (out, iout, in, msbit, length-nn) ;
         } else {
             mask = (1<<length)-1;
             if (lsbit == 0) {
                 out[iout] = out[iout] | (in.readUnsignedByte () & mask) ;
                 msbit = 7;
             } else {
                 in.mark (1) ;
                 out[iout] = out[iout] | ((in.readUnsignedByte () >> lsbit) & mask) ;
                 in.reset () ;
                 msbit = msbit - length;
             }
         }
         
         return (msbit);
    }
   
}
