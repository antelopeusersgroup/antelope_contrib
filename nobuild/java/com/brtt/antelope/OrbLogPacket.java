/* com.brtt.antelope.OrbLogPacket
 *
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Written 2004-06-29 by Tobin Fricke, University of California
 */

package com.brtt.antelope;

/**
 * This class represents a single Antelope Orb log packet, which 
 * carries a single string.
 *
 * @author  Tobin Fricke, University of California
 */

public class OrbLogPacket extends OrbStringPacket {
    // they're actually exactly the same
    public OrbLogPacket(String contents) {
	super(contents);
    }
}
