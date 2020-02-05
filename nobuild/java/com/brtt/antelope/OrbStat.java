/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.OrbStat.java
 *
 * Created on February 23, 2001, 3:33 PM
 */

package com.brtt.antelope;

/**
 * This class represents the status of a single Antelope Orb.
 *
 * @author  Danny Harvey
 * @version 1.0
 */
public class OrbStat extends Object {
       
    /**
     * Holds the epoch time when the Orb status was retrieved.
     */ 
        public double whenTime;
          
    /**
     * Holds the epoch time when the current orbserver was started.
     */ 
        public double orbstartedTime;
          
    /**
     * Holds the epoch time when the Orb was last initialized.
     */ 
        public double orbinitializedTime;
          
    /**
     * Holds the total number of current orbserver-client connections.
     */ 
        public int connections;
        
        public int messages;
          
    /**
     * Holds the maximum total number of data bytes in this Orb.
     */ 
        public int maxdataBytes;
          
    /**
     * Holds the total number of errors encountered by this orbserver.
     */ 
        public int errors;
              
    /**
     * Holds the total number of rejected client connections by this orbserver.
     */ 
        public int rejected;
              
    /**
     * Holds the total number of client connections opened by this orbserver.
     */ 
        public int opens;
                     
    /**
     * Holds the total number of client connections closed by this orbserver.
     */ 
        public int closes;
                     
    /**
     * Holds the ip port number of this orbserver.
     */ 
        public int port;
                             
    /**
     * Holds the ip address of this orbserver.
     */
        public byte[] address = new byte[4];
                             
    /**
     * Holds the process id of this orbserver.
     */
        public int pid;
                             
    /**
     * Holds the current number of data sources managed by this orbserver.
     */
        public int nSources;
                                 
    /**
     * Holds the current number of orbserver-client connections.
     */
        public int nClients;
                                 
    /**
     * Holds the maximum number of data sources that can be accomodated by this Orb.
     */
        public int maxSources;
                                         
    /**
     * Holds the maximum number of data packets that can be accomodated by this Orb.
     */
        public int maxPkts;
                                         
    /**
     * Holds the version of this Orb.
     */
        public String version;
                                         
    /**
     * Holds the user name who started this orbserver.
     */
        public String who;
                                         
    /**
     * Holds the hostname of the computer where this orbserver is running.
     */
        public String host;
           
    /** Constructors */
    
    /**
     * Creates a new OrbStat object.
     **/
    public OrbStat() {
    }
    
    /** Public Methods */
          
    /**
     * This gets a string description of the Antelope Orb status.
     * @return A string suitable for display with the Orb status.
     */  
    public String getList () {
        Epoch es = new Epoch(orbstartedTime);
        Epoch ei = new Epoch(orbinitializedTime);
        Epoch est = new Epoch (whenTime);
        String s =    "            version = " + version + "\n"
                    + "                who = " + who + "\n"
                    + "               host = " + host + "\n"
                    + "           statTime = " + est.toString() + "\n"
                    + "     orbstartedTime = " + es.toString("%A %o of %B, %Y (%g) at %H:%M:%S") + "\n"
                    + " orbinitializedTime = " + ei.toString() + "\n"
                    + "         orbAddress = " + Stock.ipString(address) + "\n"
                    + "            orbPort = " + port + "\n"
                    + "             orbPid = " + pid + "\n"
                    + "        connections = " + connections + "\n"
                    + "           messages = " + messages + "\n"
                    + "             errors = " + errors + "\n"
                    + "           rejected = " + rejected + "\n"
                    + "              opens = " + opens + "\n"
                    + "             closes = " + closes + "\n"
                    + "       maxdataBytes = " + maxdataBytes + "\n"
                    + "         maxSources = " + maxSources + "\n"
                    + "            maxPkts = " + maxPkts + "\n"
                    + "           nSources = " + nSources + "\n"
                    + "           nClients = " + nClients + "\n"
                    ;
        
        return (s);
    }

}
