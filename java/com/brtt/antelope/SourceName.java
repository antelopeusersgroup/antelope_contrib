/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * Factored out of "com.brtt.antelope.OrbClient.java"
 *
 */

class SourceName {

    /**
     * Holds the packet type. One of "waveform", for waveform
     *      data, "database", for an ASCII database row, "parameter", for a 
     *      free-form parameter packet, "string", for a character string packet,
     *      "log", for a log packet, or "unknown".
     */
    public String type;
            
    /**
     * Holds the packet format (for waveform packets).
     */
    public String format;
                
    /**
     * Holds a packet name (database table name for type db, parameter
     *      object name for type pf, etc.).
     */
    public String name;
                
    /**
     * Holds the packet SEED network code as derived from the srcname
     *      (for waveform packets).
     */
    public String net;
                    
    /**
     * Holds the packet SEED station code as derived from the srcname
     *      (for waveform packets).
     */
    public String sta;
                        
    /**
     * Holds the packet SEED channel code as derived from the srcname
     *      (for waveform packets).
     */
    public String chan;
                        
    /**
     * Holds the packet SEED location code as derived from the srcname
     *      (for waveform packets).
     */
    public String loc;

    /** 
     * Assemble a standard sourcename from the component fields
     * {@link #type}, {@link #format}, {@link #name}, {@link #net},
     * {@link #sta}, {@link #chan} and {@link #loc}.

    public String toString() {
       // Not Implemented Yet	
    }

    /**
     * This parses the given sourcename into the public fields 
     * {@link #type}, {@link #format}, {@link #name}, {@link #net},
     * {@link #sta}, {@link #chan} and {@link #loc}.
     */    

    public void parse(String srcname) {
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

}
