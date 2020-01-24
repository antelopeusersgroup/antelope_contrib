/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.SourcesModel.java
 *
 * Created on February 28, 2001, 4:18 PM
 */

package com.brtt.antelope;

import java.util.*;
import javax.swing.*;
import javax.swing.table.*;

/**
 * This class can be used to display Orb sources info 
 * in a Java Swing JTable widget.
 *
 * @author  Danny Harvey
 * @version 1.0
 */

public class SourcesModel extends AbstractTableModel {
    private String headers[] = {"srcname", "#pkts", "bytes", "oldestPkt",
                    "oldestTime", "latestPkt", "latestTime", "latency" };
    private Class columnClasses[] = {String.class, Number.class, Number.class,
                    Number.class, Number.class, Number.class, Number.class, Number.class };
    private String values[][] = null;
    private int nrows;
    public int columnWidths[] = null;

    /** Creates new SourcesModel */
                    
    public SourcesModel (Orb orb, JTable jtable) throws Throwable {
        update (orb);
        jtable.setModel (this);
        for (Enumeration e=jtable.getColumnModel().getColumns(); e.hasMoreElements(); ) {
           TableColumn tc = (TableColumn) e.nextElement();
           int c = tc.getModelIndex();
           tc.setPreferredWidth(8*this.columnWidths[c]);
        }
    }
    
    public void update (Orb orb) throws Throwable {
        OrbSource sources[], srcs[];
        int r, c;
        
        sources = orb.sources();
        nrows = sources.length;
        srcs = new OrbSource[nrows];
        java.lang.System.arraycopy (sources, 0, srcs, 0, nrows);
        java.util.Arrays.sort(srcs);
        values = new String[nrows][headers.length];
        for (r=0; r<nrows; r++) {
            Epoch e;
            double ep;
            
            for (c=0; c<headers.length; c++) {
                switch (c) {
                    case 0:
                        values[r][c] = srcs[r].srcname;
                        break;
                    case 1:
                        values[r][c] = String.valueOf(srcs[r].npktsOrb);
                        break;
                    case 2:
                        values[r][c] = String.valueOf(srcs[r].nbytesOrb);
                        break;
                    case 3:
                        values[r][c] = String.valueOf(srcs[r].oldestPktid);
                        break;
                    case 4:
                        e = new Epoch(srcs[r].oldestTime);
                        values[r][c] = e.toString("%j %H:%M:%S");
                        break;
                    case 5:
                        values[r][c] = String.valueOf(srcs[r].latestPktid);
                        break;
                    case 6:
                        e = new Epoch(srcs[r].latestTime);
                        values[r][c] = e.toString("%j %H:%M:%S");
                        break;
                    case 7:
                        ep = srcs[r].whenTime-srcs[r].latestTime;
                        if (ep < 0.0) {
                            e = new Epoch(-ep);
                        } else {
                            e = new Epoch(ep);
                        }
                        if (e.epoch > 86400.0) {
                            values[r][c] = e.toString("%j %H:%M:%S");
                        } else if (ep < 0.0) {
                            values[r][c] = "-" + e.toString("%H:%M:%S");
                        } else {
                            values[r][c] = e.toString("%H:%M:%S");
                        }
                        break;
                }
            }
        }
        columnWidths = new int[headers.length];
        for (c=0; c<headers.length; c++) {
            columnWidths[c] = headers[c].length();
            for (r=0; r<nrows; r++) {
                if (values[r][c].length() > columnWidths[c]) columnWidths[c] = values[r][c].length();
            }
        }
        fireTableDataChanged();
    }
    
    public int getRowCount() { return nrows; }
    public int getColumnCount() { return headers.length; }
    public Class getColumnClass(int c) { return columnClasses[c]; }
            
    public Object getValueAt(int r, int c) {
        return values[r][c];
    }
        
    public String getColumnName(int c) { return headers[c]; }

}
