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

import java.io.*;
import java.text.*;

/**
 * This class represents a single Antelope Orb packet.
 *
 * @author  Danny Harvey, BRTT
 * @version 1.0
 */
public class OrbPacket extends Object {
    static private final int GENC_DEBUG = 0;
    
    /**
     * Holds the packet epoch time.
     */
    public double time;
    
    /**
     * Holds the packet data.
     */
    public byte packet[] = null;
    
    /**
     * Holds the Orb packet id.
     */
    public int pktid;
    
    /**
     * Holds the packet Orb source name.
     */
    public String srcname;
    
    /**
     * Holds the current packet data size.
     */
    public int pktsize = 0;
        
    /**
     * Holds the packet type (after unstuffing). One of "waveform", for waveform
     *      data, "database", for an ASCII database row, "parameter", for a 
     *      free-form parameter packet, "string", for a character string packet,
     *      "log", for a log packet, or "unknown".
     */
    public String type;
            
    /**
     * Holds the packet format (for waveform packets after unstuffing).
     */
    public String format;
                
    /**
     * Holds a packet name (database table name for type db, parameter
     *      object name for type pf, etc., after unstuffing).
     */
    public String name;
                
    /**
     * Holds the packet SEED network code as derived from the srcname
     *      (for waveform packets after unstuffing).
     */
    public String net;
                    
    /**
     * Holds the packet SEED station code as derived from the srcname
     *      (for waveform packets after unstuffing).
     */
    public String sta;
                        
    /**
     * Holds the packet SEED channel code as derived from the srcname
     *      (for waveform packets after unstuffing).
     */
    public String chan;
                        
    /**
     * Holds the packet SEED location code as derived from the srcname
     *      (for waveform packets after unstuffing).
     */
    public String loc;
    
    /**
     * Holds the number of waveform channels (after unstuffing)
     */
    public int nchannels = 0;
    
    /**
     * Holds the waveform channel objects (after unstuffing)
     */
    public OrbPacketChannel channel[] = null;
        
    /** Constructors */
    
    /**
     * Creates a new OrbPacket object.
     **/
    public OrbPacket() {
    }
          
    /** Public Methods */
        
    /**
     * This converts on Antelope data packet into a usable format.
     * @exception java.io.IOException
     *              IO error during parsing of data packet.
     */
    public void unstuff () throws IOException {
        parseSrcname (); 
        
        if (type.compareTo("waveform") != 0) return;

        if (format.compareTo("MGENC") == 0) {
	    unstuffMGENC ();
	    return;
	}

        if (format.compareTo("GENC") == 0) {
            unstuffGENC ();
            return;
        }
                
        if (format.compareTo("GEN") == 0) {
            unstuffGEN ();
            return;
        }

	
    }
    
    /**
     * This parses the OrbPacket {@link #srcname} into the public fields 
     * {@link #type}, {@link #format}, {@link #name}, {@link #net},
     * {@link #sta}, {@link #chan} and {@link #loc}.
     */    
    public void parseSrcname () {
         nchannels = 0;
         name = "";
         format = "";
         net = "";
         sta = "";
         chan = "";
         loc = "";
         if (srcname.length() > 3 && srcname.substring(0,4).compareTo("/db/") == 0) {
             type = "database";
             if (srcname.length() > 4) name = srcname.substring(5,srcname.length());
             return;
         } else if (srcname.length() > 3 && srcname.substring(0,4).compareTo("/pf/") == 0) {
             type = "parameter";
             if (srcname.length() > 4) name = srcname.substring(5,srcname.length());
             return;
         } else if (srcname.length() > 3 && srcname.substring(0,4).compareTo("/ch/") == 0) {
             type = "string";
             if (srcname.length() > 4) name = srcname.substring(5,srcname.length());
             return;
         } else if (srcname.length() > 4 && srcname.substring(0,5).compareTo("/log/") == 0) {
             type = "log";
             if (srcname.length() > 5) name = srcname.substring(6,srcname.length());
             return;
         } else if (srcname.charAt(0) == '/') {
             type = "unknown";
             return;
         }
         
         type = "waveform";
         int i = srcname.indexOf ("_");
         if (i < 0) {
             i = srcname.indexOf ("/");
             if (i < 0) {
                 net = srcname;
                 format = "IW";
                 return;
             }
             net = srcname.substring(0, i);
             format = srcname.substring(i+1, srcname.length());
             return;
         }
         net = srcname.substring(0, i);
         String temp = srcname.substring(i+1, srcname.length());
         i = temp.indexOf ("_");
         if (i < 0) {
             i = temp.indexOf ("/");
             if (i < 0) {
                 sta = temp;
                 format = "IW";
                 return;
             }
             sta = temp.substring(0, i);
             format = temp.substring(i+1, temp.length());
             return;
         }
         sta = temp.substring(0, i);
         temp = temp.substring(i+1, temp.length());
         i = temp.indexOf ("_");
         if (i < 0) {
             i = temp.indexOf ("/");
             if (i < 0) {
                 chan = temp;
                 format = "IW";
                 return;
             }
             chan = temp.substring(0, i);
             format = temp.substring(i+1, temp.length());
             return;
         }
         chan = temp.substring(0, i);
         temp = temp.substring(i+1, temp.length());
         i = temp.indexOf ("/");
         if (i < 0) {
             loc = temp;
             format = "IW";
             return;
         }
         loc = temp.substring(0, i);
         format = temp.substring(i+1, temp.length());
    }
    
    /**
     * This gets a string description of the Antelope data packet.
     * @return A string suitable for display.
     */
    public String getList () {
        int i;
        
        DecimalFormat fmpktid = new DecimalFormat ( "000000" );
        Epoch epoch = new Epoch (time);
        
        String s =    fmpktid.format(pktid) + ":" 
                        + epoch.toString () + ":"
                        + srcname
                        ;
        
        s += "\n        nchannels = " + nchannels ;
        
        for (i=0; i<nchannels; i++) {
            s += "\n        " + channel[i].getList () ;
        }
        
        return (s);
    }
        
    /** Private Class Methods */
    
    private String buf2str (DataInputStream in, int l) throws IOException {
        int i;
        
        if (l < 1) {
            return (new String(""));
        }
        byte ibuf[] = new byte[l];
        in.readFully (ibuf);
        for (i=0; i<l; i++) if (ibuf[i] == 0) break;
        return (new String (ibuf, 0, i));
    }

    private void str2buf (String s, DataOutputStream out, int len) throws IOException {
	// What should we do if we don't have enough bytes to send?  Pad with nulls or spaces? 
	// Throw an exception?  Model the behavior of buf2str?
	
	byte[] bytes = s.getBytes("US-ASCII");
	for (int i=0; i<len; i++) 
	    out.writeByte(i < bytes.length ? bytes[i] : 0);
    }
    
    private void unstuffMGENC() throws IOException {
	
        if (packet == null || pktsize < 1) return;
        
        ByteArrayInputStream inBuf = new ByteArrayInputStream (packet, 0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
	
	int version = pktBuf.readUnsignedShort ();  
        nchannels   = pktBuf.readUnsignedShort ();
	
        if (channel == null || channel.length < nchannels) {
            channel = new OrbPacketChannel[nchannels];
	    for (int i=0; i<nchannels; i++) {
		if (channel[i] == null) {
		    channel[i] = new OrbPacketChannel();
		}
	    }
	}
	
	/* An MGENC packet with only one channels is pretty much the same
	   as a GENC packet, but some of the fields are reordered. */
	
	for (int c=0; c<nchannels; c++) {
	    if (nchannels == 1) {
		channel[c].net = net;
		channel[c].sta = sta;
		channel[c].chan = chan;
		channel[c].loc = loc;
	    } else {
		channel[c].net = buf2str(pktBuf, 10);
		channel[c].sta = buf2str(pktBuf, 10);
		channel[c].chan = buf2str(pktBuf, 10);
		channel[c].loc = buf2str(pktBuf, 10);
	    }
	    
	    channel[c].segtype = buf2str(pktBuf, 2); 
	    if (nchannels == 1) {
		channel[c].time = this.time;	  
	    } else {
		channel[c].time = pktBuf.readDouble();	  
	    }
	    
	    channel[c].nsamp       = pktBuf.readUnsignedShort () ;
	    channel[c].samprate    = pktBuf.readFloat () ;
	    channel[c].calib       = pktBuf.readFloat () ;
	    channel[c].calper      = pktBuf.readFloat () ;
	    
	    if (channel[c].data == null || channel[c].datasize < channel[c].nsamp) {
		channel[c].datasize = channel[c].nsamp;
		channel[c].data = new int[channel[c].datasize];
	    }
	    
            int nout;
            if (channel[c].nsamp < 5) {
		for (int i=0; i<channel[c].nsamp; i++)
		    channel[c].data[i] = pktBuf.readInt();
		nout = channel[c].nsamp;
            } else { 
		int bytecount = pktBuf.readUnsignedShort();
		nout = uncompressGENC (channel[c].data, pktBuf, bytecount) ;
		for (int i=1; i<channel[c].nsamp; i++) 
		    channel[c].data[i] += channel[c].data[i-1];
            }
	    
	    if (nout != channel[c].nsamp) {
		System.out.println ("nsamp = " + channel[c].nsamp + ", but nout = " + nout);
	    }
	}
	
        pktBuf.close() ;
        inBuf.close() ;
    }
    

    private void unstuffGENC () throws IOException {
        double samprate, calib, calper;
        int nsamp;
        String segtype;
        int i;
        
        if (packet == null || pktsize < 1) return;
        
        ByteArrayInputStream inBuf = new ByteArrayInputStream (packet, 0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
        
        samprate    = pktBuf.readFloat () ;
        calib       = pktBuf.readFloat () ;
        calper      = pktBuf.readFloat () ;
        nsamp       = pktBuf.readUnsignedShort () ;
        segtype     = buf2str (pktBuf, 2); 
        
        nchannels = 1;
        if (channel == null || channel.length < nchannels) {
            channel = new OrbPacketChannel[nchannels];
            channel[0] = new OrbPacketChannel ();
        }
        
        channel[0].time     = time;
        channel[0].net      = net;
        channel[0].sta      = sta;
        channel[0].chan     = chan;
        channel[0].loc      = loc;
        channel[0].calib    = calib;
        channel[0].calper   = calper;
        channel[0].samprate = samprate;
        channel[0].nsamp    = nsamp;
        channel[0].segtype  = segtype;
        
        if (channel[0].data == null || channel[0].datasize < channel[0].nsamp) {
            channel[0].datasize = channel[0].nsamp;
            channel[0].data = new int[channel[0].datasize];
        }
        
        int nout = uncompressGENC (channel[0].data, pktBuf) ;
        
        if (nout != nsamp) {
            System.out.println ("nsamp = " + nsamp + ", but nout = " + nout);
        }
            
        pktBuf.close () ;
        inBuf.close () ;
        
        for (i=1; i<channel[0].nsamp; i++) channel[0].data[i] += channel[0].data[i-1];
    }
        
    private void unstuffGEN () throws IOException {
        double samprate, calib, calper;
        int nsamp;
        String segtype;
        int i;
        
        if (packet == null || pktsize < 1) return;
        
        ByteArrayInputStream inBuf = new ByteArrayInputStream (packet, 0, pktsize);
        DataInputStream pktBuf = new DataInputStream (inBuf);
        
        samprate    = pktBuf.readFloat () ;
        calib       = pktBuf.readFloat () ;
        calper      = pktBuf.readFloat () ;
        nsamp       = pktBuf.readUnsignedShort () ;
        segtype     = buf2str (pktBuf, 2); 
        
        nchannels = 1;
        if (channel == null || channel.length < nchannels) {
            channel = new OrbPacketChannel[nchannels];
            channel[0] = new OrbPacketChannel ();
        }
        
        channel[0].time     = time;
        channel[0].net      = net;
        channel[0].sta      = sta;
        channel[0].chan     = chan;
        channel[0].loc      = loc;
        channel[0].calib    = calib;
        channel[0].calper   = calper;
        channel[0].samprate = samprate;
        channel[0].nsamp    = nsamp;
        channel[0].segtype  = segtype;
        
        if (channel[0].data == null || channel[0].datasize < channel[0].nsamp) {
            channel[0].datasize = channel[0].nsamp;
            channel[0].data = new int[channel[0].datasize];
        }
                       
        for (i=1; i<channel[0].nsamp; i++) channel[0].data[i] = pktBuf.readInt () ;
            
        pktBuf.close () ;
        inBuf.close () ;

    }
    
    private void stuffGEN() throws IOException {
	
	ByteArrayOutputStream outBuf = new ByteArrayOutputStream();
        DataOutputStream pktBuf = new DataOutputStream (outBuf);
	
	pktBuf.writeFloat((float)channel[0].samprate);
	pktBuf.writeFloat((float)channel[0].calib);
	pktBuf.writeFloat((float)channel[0].calper);
	pktBuf.writeShort(channel[0].nsamp); // FixMe: Is this unsigned?
	str2buf(channel[0].segtype, pktBuf, 2);
	
	// FixMe: warn if too many channels
	
	for (int i=1; i<channel[0].nsamp; i++) 
	    pktBuf.writeInt(channel[0].data[i]);
	
	packet = outBuf.toByteArray();
        pktBuf.close();
        outBuf.close();
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
