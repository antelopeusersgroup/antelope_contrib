/*
 * Copyright (c) 2004 by the Regents of the University of California
 *
 * Created 2004-07-06 by Tobin Fricke <tobin@splorg.org>
 */

package com.brtt.antelope;

import java.applet.Applet;
import java.awt.Graphics;

/**
 * Wrapper to embed JFrameOrbStat as an applet in a web page.
 *
 * See http://java.sun.com/docs/books/tutorial/uiswing/components/applet.html
 *
 * @author Tobin Fricke, University of California
 */

public class OrbStatApplet extends Applet {

    public JFrameOrbStat orbStat;

    public void init() {
	orbStat = new JFrameOrbStat("JFrameOrbStat", "bohemia.splorg.org:6580");
    }

    public void start() {
	orbStat.show();
    }

    public void stop() {
    }

    public void destroy() {
    }

    public void paint(Graphics g) {
    }
}

