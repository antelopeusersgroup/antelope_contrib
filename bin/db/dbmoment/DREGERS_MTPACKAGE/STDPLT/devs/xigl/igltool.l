.TH IGLTOOL l "5 April 1989"
.SH NAME
igltool \- IGL filter for sunview graphics
.SH SYNOPSIS
.B "igltool standard-par-options special-par-options"
.SH DESCRIPTION
.I Igltool
is a filter for the
.I Intermediate Graphics Language
(IGL) to produce graphics under
.I SunView.
As with all IGL filters, igltool reads graphics commands off standard
input (from a pipe or file).  It then displays the result on a SunView
device running
.I
sunview
(1).
It can detect if the output device is a color screen and automatically
sets up for color.  Igltool graphics are displayed in a window the size of
the screen, showing a portion of the available canvas.  Igltool exits
only when explicitly quit.
(1).

.SH STANDARD-PARS
The standard pars for igltool are the same as those for other IGL
filters.  Special mention is made here for those with interesting
effects for igltool.
.IP xpagelen=(float)
.br
This parameter sets the horizontal dimension of the canvas to the
requested length in inches.  (Current limit is about 30.0 inches).
.IP ypagelen=(float)
.br
Same as xpagelen, for vertical dimension.
.IP scale=(float)
.br
Sets the overall scale for the plot, scale=3.0 makes the plot three
times bigger.  xscale=(float) and yscale=(float) are applied on top
of scale to change x and y scaling independently.
.IP xorig=(float)
.br
sets the origin for the plot at the requested position (in inches)
from the left edge of the canvas.
.IP yorig=(float)
.br
Same as xorig, for vertical position measured up from the bottom of
the canvas.

.SH SPECIAL-PARS
These parameters are unique to igltool.
.IP bw_only=(0,1)
This option, when set (bw_only=1)
forces black-on-white output independent of the graphics
device, allowing black/white plots on color devices.  The default is
unset (bw_only=0), permitting color.

.SH FEATURES
Igltool has all the features of a standard SunView window.  The frame
permits closing, quiting, resizing, moving, opening and exposing the window.
When closed, igltool features its own icon.  There are scrollbars for
both vertical and horizontal movement of the canvas (usually larger than
the screen) beneath the window.
.br
The mouse has several functions under igltool.  These are listed according
to button:
.IP Left
The left mouse button is the 'sticky' button.  To use it, depress the left
button then drag the mouse across the image to where you would like the image
positioned.  The image will follow the mouse as if stuck to it.  This
allows more intuitive scrolling.
.IP Middle
The middle mouse button is fast quit.  If depressed, you will be immediately
requested to confirm the quit.
.IP Right
The right mouse button is the menu button.  The pop-up menu has three
options:  visible, invisible and quit.  Igltool is normally in the
.B visible
state.  When
.B invisible
is selected, the cursor and scrollbars are un-displayed (invisible),
although still fully functional.  The foreground color is changed to
nearly black to hide the frame (Note: this is color table entry 255).
This feature is designed for photographs directly from the screen.

.SH BUGS
The patterns and colors may both be used on color screens, which is
not standard in IGL, but may be useful to some.
.br
The standard SunView options for setting frame dimensions, foreground
and background colors, and frame position are not available.
.br
There is no interactive graphics (a limitation of IGL).
.br
Foreground (color 0) and background (color 255) must be different.
This is a SunView limitation.
.br
The sticky mouse is very slow on the color screen, and is not likely to
keep up.

.SH NOTES
IGL is copyrighted by Robert W. Clayton, all rights reserved.

.SH AUTHOR
Igltool was written by Richard J. Stead, based on several existing
IGL filters for other devices.
