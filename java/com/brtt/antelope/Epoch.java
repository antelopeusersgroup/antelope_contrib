/*
 * Copyright (c) 2001 Boulder Real Time Technologies, Inc.
 * All rights reserved
 *
 * This software may be used freely in any way as long as
 * the copyright statement above is not removed.
 *
 * com.brtt.antelope.Epoch.java
 *
 * Created on February 26, 2001, 5:41 PM
 */

package com.brtt.antelope;

import java.text.*;

/**
 * This class provides objects and methods to deal with Antelope epoch times.
 * An Antelope epoch time is a double precision floating number that is used
 * throughout Antelope to represent absolute time. The Antelope epoch time
 * is the "nominal" number of seconds (includes leap days but not leap seconds)
 * since 1970-Jan-1 00:00:00.000 GMT.
 *
 * @author  Danny Harvey
 * @version 1.0
 */
public class Epoch extends Object {
    /** Private Static Class Variables */
    static private final String monthName[] = { 
            "January", "February", "March", "April", "May", "June",
            "July", "Augusr", "September", "October", "November", "December" };
    static private final String dayName[] = {
            "Monday", "Tuesday", "Wednesday", "Thursday",
            "Friday", "Saturday", "Sunday" };
    static private int daysInMonth[] = {
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31 };
    
    /** Public Class Variables */
               
    /**
     * Holds the Antelope epoch time.
     */ 
    public double epoch;
                   
    /**
     * Holds the corresponding Julian date (of form yyyyddd where yyyy is
     * the 4-digit year and ddd is the 3-digit day-of-year).
     */
    public int jdate;
                   
    /**
     * Holds the corresponding year number (all digits present).
     */
    public int year;
                   
    /**
     * Holds the corresponding month number in the range 1 to 12.
     */
    public int month;
                   
    /**
     * Holds the corresponding month string.
     */
    public String monthString = null;
                   
    /**
     * Holds the corresponding day-of-week string.
     */
    public String dayString = null;
                   
    /**
     * Holds the corresponding day-of-month number in the range 1 to 31.
     */
    public int dayOfMonth;
                   
    /**
     * Holds the corresponding day-of-year number in the range 1 to 357.
     */
    public int dayOfYear;
                   
    /**
     * Holds the corresponding day-of-week number in the range 1 to 7.
     */
    public int dayOfWeek;
                   
    /**
     * Holds the corresponding hour number in the range 0 to 23.
     */
    public int hour;
                   
    /**
     * Holds the corresponding minute number in the range 0 to 59.
     */
    public int minute;
                   
    /**
     * Holds the corresponding second number in the range 0.00 to 59.99999999999.
     */
    public double second;
           
    /** Constructors */
    
    /**
     * Creates a new Epoch object.
     * @param epoch The Antelope double precision epoch time.
     **/
    public Epoch(double epoch) {
        int i, dim;
        
        this.epoch = epoch;
        double rdays = Math.floor(epoch/86400.0);
        double rem = epoch - rdays * 86400.0;
        hour = (int) Math.floor(rem / 3600.0);
        rem -= hour * 3600.0;
        minute = (int) Math.floor(rem / 60.0);
        rem -= minute * 60.0;
        second = rem;
        double dyear = Math.floor (rdays / 365.0) + 1970.0;
        double days = year2days ( dyear );
        double ddoy = rdays - days;
        if ( ddoy < -366.0 ) {
            ddoy = 0.0;
        } else if ( ddoy < 0.0 ) {
            dyear -= 1.0;
            if ( isleap ( (int) dyear ) ) {
                ddoy += 366.0;
            } else {
                ddoy += 365.0;
            }
        } else if ( ddoy >= 730.0 ) {
            ddoy = 0.0;
        } else if ( isleap ( (int) dyear ) ) {
            if ( ddoy > 365.0 ) {
                dyear += 1.0;
                ddoy -= 366.0;
            }
        } else if ( ddoy > 364.0 ) {
            dyear += 1.0;
            ddoy -= 365.0;
        }
        dayOfYear = (int) (ddoy + 1.0);
        year = (int) Math.max(Math.min(dyear,(double)Long.MAX_VALUE/10000.0),(double)Long.MIN_VALUE/10000.0);
        jdate = year*1000 + dayOfYear;
        boolean leap = isleap ( year );
        dayOfMonth = dayOfYear;
        for (i=0; i<12; i++) {
            dim = daysInMonth[i];
            if (leap && i == 1) dim++;
            if (dayOfMonth <= dim) break;
            dayOfMonth -= dim;
        }
        month = i + 1;
        if (month > 12) month = 12;
        monthString = new String (monthName[month-1]);
        dayOfWeek = (int) (rdays + 3.0) % 7;
        if (dayOfWeek < 0) dayOfWeek += 7;
        dayString = new String (dayName[dayOfWeek]);
        dayOfWeek++;
    }
    
    /** Public Instance Methods */
          
    /**
     * This gets a string version of the Antelope epoch time with a default
     *      format.
     * @return A string version of the Antelope epoch time.
     */ 
    public String toString () {
        DecimalFormat fs = new DecimalFormat ( "00.000" );
        DecimalFormat fm = new DecimalFormat ( "00" );
        return (new String (year + "/" + fm.format(month) + "/" + fm.format(dayOfMonth) + " (" + jdate + ") " + fm.format(hour) + ":" + fm.format(minute) + ":" + fs.format(second)));
    }

    /** Parse the given string containing a textual representation of a 
	date/time and produce an Epoch object representing the same time.
	Currently the only string parsed is "now" which results in an Epoch
	object representing the current date and time. 
	@param epoch The string to parse.
	@author Tobin Fricke, University of California
    */

    public static Epoch fromString(String epoch) {
	if (epoch.compareTo("now")==0) {
	    // http://joda-time.sourceforge.net/ might be useful
	    // java.util.Date is supposed to know about timezones and initialize to the UTC time
	    return new Epoch((new java.util.Date()).getTime() / 1000.0);
	} else {
	    return null; // fail
	}
    }
             
    /**
     * This gets a string version of the Antelope epoch time with a specified
     *      format.
     * @param format A format string that follows the definitions in the epoch(3)
     *              Antelope man page.
     * @return A string version of the Antelope epoch time.
     */  
    public String toString (String format) {
        int i, n;
        char c;
        
        DecimalFormat fm02 = new DecimalFormat ( "00" );
        DecimalFormat fm03 = new DecimalFormat ( "000" );
        DecimalFormat fm06 = new DecimalFormat ( "00.000" );
        DecimalFormat fm2 = new DecimalFormat ( "##" );
        DecimalFormat fme = new DecimalFormat ( "###########.###" );
        String out = "";
        n = format.length();
        for (i=0; i<n; i++) {
            c = format.charAt(i);
            if (c != '%') {
                out += c;
                continue;
            }
            i++;
            if (i >= n) break;
            c = format.charAt(i);
            switch (c) {
                case 'a':
                    out += dayString.substring(0, 3);
                    break;
                case 'A':
                    out += dayString;
                    break;
                case 'b':
                    out += monthString.substring(0, 3);
                    break;
                case 'B':
                    out += monthString;
                    break;
                case 'd':
                    out += fm02.format(dayOfMonth);
                    break;
                case 'D':
                    out += fm02.format(month) + "/" + fm02.format(dayOfMonth) + "/" + fm02.format(year%100);
                    break;
                case 'e':
                    out += fm2.format(dayOfMonth);
                    break;
                case 'E':
                    out += fme.format(epoch);
                    break;
                case 'H':
                    out += fm02.format(hour);
                    break;
                case 'I':
                    if (hour % 12 != 0) {
                        out += fm02.format(hour % 12);
                    } else {
                        out += "12";
                    }
                    break;
                case 'g':
                    out += year + "-" + fm03.format(dayOfYear);
                    break;
                case 'G':
                    out += year + "-" + fm02.format(month) + "-" + fm02.format(dayOfMonth);
                    break;
                case 'j':
                    out += fm03.format(dayOfYear);
                    break;
                case 'k':
                    out += fm2.format(hour);
                    break;
                case 'l':
                    if (hour % 12 != 0) {
                        out += fm2.format(hour % 12);
                    } else {
                        out += "12";
                    }
                    break;
                case 'm':
                    out += fm02.format(month);
                    break;
                case 'M':
                    out += fm02.format(minute);
                    break;
                case 'n':
                    out += "\n";
                    break;
                case 'o':
                    out += dayOfMonth + ordinal(dayOfMonth);
                    break;
                case 'O':
                    out += dayOfYear + ordinal(dayOfYear);
                    break;
                case 'P':
                    if (hour >= 12) {
                        out += "PM";
                    } else {
                        out += "AM";
                    }
                    break;
                case 's':
                    out += fm03.format((int)Math.rint(second*1000.0) % 1000);
                    break;
                case 'S':
                    out += fm02.format((int)second);
                    break;
                case 't':
                    out += "\t";
                    break;
                case 'T':
                    out += fm02.format(hour) + ":" + fm02.format(minute) + ":" + fm06.format(second);
                    break;
                case 'y':
                    out += fm02.format(year%100);
                    break;
                case 'Y':
                    out += year;
                    break;
                default:
                    out += c;
                    break;
            }
        }
        
        return (out);
    }
    
    /** Private Class Methods */
    private static double year2days ( double dyear ) {
        double leapdays = Math.floor ((dyear - 1969.0) / 4.0)
                        - Math.floor ((dyear - 1901.0) / 100.0)
                        + Math.floor ((dyear - 1601.0) / 400.0);
        return ((dyear - 1970.0)*365.0 + leapdays);
    }
    
    private static boolean isleap ( int year ) {
        return (year % 4 == 0 && year % 100 != 0 || year % 400 == 0);
    }
    
    private static String ordinal ( int i ) {
        String out;
        
        if (i % 100 <= 20 && i >= 4) {
            out = "th";
        } else {
            switch ( i % 10 ) {
                case 1:
                    out = "st";
                    break;
                case 2:
                    out = "nd";
                    break;
                case 3:
                    out = "rd";
                    break;
                default:
                    out = "th";
                    break;
            }
        }
        return (out);
    }

}
